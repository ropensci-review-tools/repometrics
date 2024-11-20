cm_data_gh_contributors <- function (path) {

    log <- cm_data_gitlog (path)
    gh_url <- pkg_gh_url_from_path (path)


}

contribs_from_log <- function (log) {

    gh_handle <- unique (log$aut_name)
    gh_email <- log$aut_email [match (gh_handle, log$aut_name)]

    # Remove any duplicates of either, but excluding non-entries:
    rm_dup_rows <- function (x) {
        x <- gsub ("\\s+", "", x)
        index <- seq_along (x)
        index_out <- which (duplicated (x) & nzchar (x))
        if (length (index_out) > 0) {
            index <- index [-(index_out)]
        }
        return (index)
    }
    index1 <- rm_dup_rows (gh_handle)
    index2 <- rm_dup_rows (gh_email)

    # Then extract only instances where neither handles nor emails are
    # duplicated:
    index_table <- table (c (index1, index2))
    index <- as.integer (names (index_table) [which (index_table == 2L)])

    data.frame (
        handle = gh_handle,
        email = gh_email
    ) [index, ]
}

contribs_from_gh_api <- function (path) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"

    gh_url <- pkg_gh_url_from_path (path)

    org_repo <- gsub ("https://github.com/", "", gh_url, fixed = TRUE)
    if (!grepl ("\\/$", org_repo)) {
        org_repo <- paste0 (org_repo, "/")
    }

    u_base <- "https://api.github.com/repos/"
    u_org_repo <- paste0 (u_base, org_repo)
    u_endpoint <- paste0 (u_org_repo, "contributors")

    req <- httr2::request (u_endpoint) |>
        httr2::req_url_query (per_page = 100)

    add_token <- function (req) {
        if (!nzchar (Sys.getenv ("GITHUB_WORKFLOW"))) {
            tok <- get_gh_token ()
            headers <- list (Authorization = paste0 ("Bearer ", tok))
            req <- httr2::req_headers (req, "Authorization" = headers)
        }

        return (req)
    }

    body <- NULL
    next_page <- 1

    while (!is.null (next_page)) {

        req <- add_token (req)
        resp <- httr2::req_perform (req)
        httr2::resp_check_status (resp)

        body <- c (body, httr2::resp_body_json (resp))

        next_page <- get_next_page (resp)
        if (is_test_env) {
            next_page <- NULL
        }

        req <- httr2::request (u_endpoint) |>
            httr2::req_url_query (per_page = 10) |>
            httr2::req_url_query (page = next_page)
    }

    login <- vapply (body, function (i) i$login, character (1L))
    ctb_id <- vapply (body, function (i) i$id, integer (1L))
    avatar_url <- vapply (body, function (i) i$avatar_url, character (1L))
    api_url <- vapply (body, function (i) i$url, character (1L))
    gh_url <- vapply (body, function (i) i$html_url, character (1L))
    contributions <- vapply (body, function (i) i$contributions, integer (1L))

    data.frame (
        login = login,
        ctb_id = ctb_id,
        avatar_url = avatar_url,
        api_url = api_url,
        gh_url = gh_url,
        contributions = contributions
    )
}
