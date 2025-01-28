# nocov start
get_gh_token <- function () {
    e <- Sys.getenv ()
    nms <- names (e)
    tok <- unique (e [grep ("GITHUB", nms)])
    if (length (tok) != 1L) {
        tok <- unique (e [grep ("GITHUB\\_(PAT|TOK)", nms)])
    }
    if (length (tok) != 1L) {
        cli::cli_abort (
            "Unable to determine unique GitHub token from environment variables"
        )
    }
    return (tok)
}

add_gh_token_to_req <- function (req) {

    if (!nzchar (Sys.getenv ("GITHUB_WORKFLOW"))) {
        tok <- get_gh_token ()
        headers <- list (Authorization = paste0 ("Bearer ", tok))
        req <- httr2::req_headers (req, "Authorization" = headers)
    }

    return (req)
}
# nocov end

gh_rest_api_endpoint <- function (path = NULL,
                                  orgrepo = NULL,
                                  endpoint = NULL) {

    checkmate::assert_character (endpoint, len = 1L)

    if (!is.null (path)) {
        checkmate::assert_directory (path)
        orgrepo <- org_repo_from_path (path)
    } else {
        checkmate::assert_character (orgrepo, len = 2L)
    }

    u_base <- "https://api.github.com/repos/"
    u_org_repo <- paste0 (u_base, orgrepo [1], "/", orgrepo [2], "/")
    paste0 (u_org_repo, endpoint)
}

#' Pagination for Rest API. see
#' https://docs.github.com/en/rest/using-the-rest-api/using-pagination-in-the-rest-api # nolint
#' @noRd
gh_next_page <- function (resp) {

    link <- httr2::resp_headers (resp)$link

    next_page <- NULL

    if (!is.null (link)) {
        next_ptn <- "rel\\=\\\"next"
        if (grepl (next_ptn, link)) {
            links <- strsplit (link, ",\\s+") [[1]]
            link <- grep (next_ptn, links, value = TRUE)

            ptn <- "<([^>]+)>"
            next_page <- regmatches (link, regexpr (ptn, link))
            next_page <- gsub ("^.*&page\\=|>", "", next_page)
        }
    }

    return (next_page)
}

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
