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

    return (names)
}

clone_gh_org_repos <- function (dir = getwd (), orgs = NULL) {

    checkmate::assert_directory_exists (dir)
    checkmate::assert_character (orgs, min.len = 1L)

    pkgs <- lapply (orgs, function (i) paste0 (i, "/", list_gh_org_repos (i)))
    pkgs <- unlist (pkgs)
    if (length (pkgs) == 0L) {
        return (NULL)
    }
    for (p in pkgs) {
        url <- paste0 ("https://github.com/", p)
        dir <- fs::path (d0, gsub ("\\/.*$", "", p))
        if (!fs::dir_exists (dir)) {
            fs::dir_create (dir)
        }
        dest_dir <- fs::path (d0, p)
        if (!fs::dir_exists (dest_dir)) {
            withr::with_dir (
                dir,
                gert::git_clone (url)
            )
        }
    }
}
