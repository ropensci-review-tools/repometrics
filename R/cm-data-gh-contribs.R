#' Get contributors from the git log
#'
#' @param path Local path to repository
#' @noRd
cm_data_contribs_from_log <- function (path) {

    log <- cm_data_gitlog (path)

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

#' Get contributors from the GitHub API, with the `_internal` form memoised for
#' the actual function call below.
#'
#' @param path Local path to repository
#' @param n_per_page Not used here, but needed so all functions can safely be
#' called with this parameter.
#' @noRd
cm_data_contribs_from_gh_api_internal <- function (path, n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    u_endpoint <- gh_rest_api_endpoint (path = path, endpoint = "contributors")

    req <- httr2::request (u_endpoint) |>
        httr2::req_url_query (per_page = n_per_page)

    body <- NULL
    next_page <- 1

    while (!is.null (next_page)) {

        req <- add_gh_token_to_req (req)
        resp <- httr2::req_perform (req)
        httr2::resp_check_status (resp)

        body <- c (body, httr2::resp_body_json (resp))

        next_page <- gh_next_page (resp)
        if (is_test_env) {
            next_page <- NULL
        }

        req <- httr2::request (u_endpoint) |>
            httr2::req_url_query (per_page = n_per_page) |>
            httr2::req_url_query (page = next_page)
    }

    login <- vapply (body, function (i) i$login, character (1L))
    ctb_id <- vapply (body, function (i) i$id, integer (1L))
    avatar_url <- vapply (body, function (i) i$avatar_url, character (1L))
    api_url <- vapply (body, function (i) i$url, character (1L))
    gh_url <- vapply (body, function (i) i$html_url, character (1L))
    contributions <- vapply (body, function (i) i$contributions, integer (1L))

    ctbs <- data.frame (
        login = login,
        ctb_id = ctb_id,
        avatar_url = avatar_url,
        api_url = api_url,
        gh_url = gh_url,
        contributions = contributions
    )

    ctbs_user_info <- lapply (ctbs$login, user_from_gh_api)
    ctbs_user_info <- do.call (rbind, ctbs_user_info)

    ctbs <- dplyr::left_join (ctbs, ctbs_user_info, by = c ("login", "ctb_id"))

    return (ctbs)
}
cm_data_contribs_from_gh_api <-
    memoise::memoise (cm_data_contribs_from_gh_api_internal)

user_from_gh_api <- function (user) {

    u_base <- "https://api.github.com/users/"
    u_endpoint <- paste0 (u_base, user)

    req <- httr2::request (u_endpoint) |>
        add_gh_token_to_req ()
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)
    body <- httr2::resp_body_json (resp)

    data.frame (
        login = body$login,
        ctb_id = body$id,
        name = null2na_char (body$name),
        company = null2na_char (body$company),
        email = null2na_char (body$email),
        location = null2na_char (body$location),
        blog = null2na_char (body$blog),
        bio = null2na_char (body$bio),
        public_repos = body$public_repos,
        followers = body$followers,
        following = body$following,
        created_at = body$created_at,
        updated_at = body$updated_at
    )
}
