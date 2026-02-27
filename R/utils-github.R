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
        req <- httr2::req_headers (req, "Authorization" = paste0 ("Bearer ", tok))
    }
    req <- httr2::req_headers (req, "User-Agent" = "request")

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

#' Generic paginator for GitHub GraphQL API calls.
#'
#' @param qry_fn A function of `(end_cursor, n_per_page)` returning a GraphQL
#'   query string.
#' @param extract_nodes A function of `(dat)` returning the list of nodes from
#'   one page of results.
#' @param extract_page A function of `(dat)` returning a list with
#'   `$hasNextPage` (logical) and `$endCursor` (character).
#' @param n_per_page Number of results per page.
#' @return A flat list of all accumulated nodes.
#' @noRd
gh_gql_paginate <- function (qry_fn,
                             extract_nodes,
                             extract_page,
                             n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    end_cursor <- results <- NULL
    has_next_page <- TRUE

    while (has_next_page) {
        q <- qry_fn (end_cursor, n_per_page)
        dat <- gh::gh_gql (query = q)
        results <- c (results, extract_nodes (dat))
        page_info <- extract_page (dat)
        has_next_page <- page_info$hasNextPage
        end_cursor <- page_info$endCursor
        if (is_test_env) has_next_page <- FALSE
    }

    return (results)
}

#' Generic paginator for GitHub REST API calls.
#'
#' @param endpoint_url The full URL of the REST endpoint.
#' @param n_per_page Number of results per page.
#' @param max_pages Maximum number of pages to fetch (default: all pages).
#' @param ... Additional query parameters passed to `httr2::req_url_query()`
#'   (e.g. `state = "all"`).
#' @return A flat list of all accumulated response body items.
#' @noRd
gh_rest_paginate <- function (endpoint_url,
                              n_per_page = 100L,
                              max_pages = Inf,
                              ...) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"

    req0 <- httr2::request (endpoint_url) |>
        add_gh_token_to_req ()

    extra_args <- list (...)
    if (length (extra_args) > 0L) {
        req0 <- do.call (httr2::req_url_query, c (list (req0), extra_args))
    }
    req0 <- httr2::req_url_query (req0, per_page = n_per_page)
    req <- req0

    body <- NULL
    next_page <- 1
    pages_done <- 0L

    while (!is.null (next_page)) {

        resp <- httr2::req_retry (req, max_tries = 5L) |>
            httr2::req_error (is_error = \ (resp) FALSE) |>
            httr2::req_perform ()

        if (httr2::resp_is_error (resp)) {
            next_page <- NULL
        } else {
            body <- c (body, httr2::resp_body_json (resp))
            pages_done <- pages_done + 1L
            next_page <- gh_next_page (resp)
            if (is_test_env || pages_done >= max_pages) next_page <- NULL
            req <- httr2::req_url_query (req0, page = next_page)
        }
    }

    return (body)
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
            next_page <- gsub ("\\&after\\=.*$", "", next_page)
        }
    }

    return (next_page)
}
