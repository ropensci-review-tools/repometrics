list_gh_org_repos <- function (org = "ropensci", n_per_page = 100) {

    checkmate::assert_character (org, len = 1L)

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    u_base <- "https://api.github.com/orgs/"
    u_org <- paste0 (u_base, org, "/repos")

    page_num <- 1L
    is_empty <- FALSE
    names <- NULL

    while (!is_empty) {

        req <- httr2::request (u_org) |>
            add_gh_token_to_req () |>
            httr2::req_url_query (per_page = n_per_page, page = page_num)

        resp <- httr2::req_perform (req)
        httr2::resp_check_status (resp)

        body <- httr2::resp_body_json (resp)

        names <- c (
            names,
            vapply (body, function (i) i$name, character (1L))
        )
        page_num <- page_num + 1L
        is_empty <- length (body) == 0L || is_test_env
    }

    return (paste0 (org, "/", names))
}

pkgs_are_r <- function (pkgs) {

    u_base <- "https://api.github.com/repos/"
    u_base <- "https://api.github.com/repos/reconhub/epicookbook/contents/{+path}"

    urls <- paste0 (u_base, pkgs, "/contents")

    is_r_pkg <- vapply (urls, function (u) {

        req <- httr2::request (u)

        req <- add_gh_token_to_req (req)
        resp <- tryCatch (
            httr2::req_perform (req),
            error = function (e) NULL
        )
        if (is.null (resp)) {
            return (FALSE)
        }
        httr2::resp_check_status (resp)

        body_files <- httr2::resp_body_json (resp, simplify = TRUE) |>
            dplyr::filter (type == "file")
        return ("DESCRIPTION" %in% body_files$name)
    }, logical (1L))

    return (is_r_pkg)
}

write_pkgs_json <- function (pkgs, dir = getwd ()) {

    requireNamespace ("jsonlite")

    checkmate::assert_directory_exists (dir)

    is_r_pkg <- pkgs_are_r (pkgs)
    res <- data.frame (package = pkgs, is_r = is_r_pkg)

    outfile <- fs::path (dir, "packages.json")

    jsonlite::write_json (res, path = outfile, pretty = TRUE)

    return (outfile)
}

clone_gh_org_repos <- function (dir = getwd (), orgs = NULL) {

    checkmate::assert_directory_exists (dir)
    checkmate::assert_character (orgs, min.len = 1L)

    pkgs <- lapply (orgs, list_gh_org_repos)
    pkgs <- unlist (pkgs)
    if (length (pkgs) == 0L) {
        return (NULL)
    }
    outfile <- fs::path (dir, "packages.json")
    if (!fs::file_exists (outfile)) {
        write_pkgs_json (pkgs, dir = dir)
    }
    pkgs_json <- jsonlite::read_json (outfile, simplify = TRUE) |>
        dplyr::filter (is_r)

    for (p in pkgs_json$package) {
        url <- paste0 ("https://github.com/", p)
        dir_org <- fs::path (dir, gsub ("\\/.*$", "", p))
        if (!fs::dir_exists (dir_org)) {
            fs::dir_create (dir_org)
        }
        dest_dir <- fs::path (dir, p)
        if (!fs::dir_exists (dest_dir)) {
            withr::with_dir (
                dir,
                gert::git_clone (url)
            )
        }
    }
}
