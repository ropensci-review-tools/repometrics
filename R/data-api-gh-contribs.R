#' Get contributors from the GitHub API, with the `_internal` form memoised for
#' the actual function call below.
#'
#' @param path Local path to repository
#' @param n_per_page Not used here, but needed so all functions can safely be
#' called with this parameter.
#' @noRd
rm_data_contribs_from_gh_api_internal <- function (path, n_per_page = 100L) { # nolint

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    u_endpoint <- gh_rest_api_endpoint (path = path, endpoint = "contributors")

    req <- httr2::request (u_endpoint) |>
        httr2::req_url_query (per_page = n_per_page)

    body <- NULL
    next_page <- 1

    while (!is.null (next_page)) {

        req <- add_gh_token_to_req (req)
        resp <- httr2::req_retry (req) |>
            httr2::req_perform ()
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

    login <- avatar_url <- api_url <- gh_url <- character (0L)
    ctb_id <- contributions <- integer (0L)

    if (is.null (names (body)) && length (body) > 0L) {
        # If no contributors are on GH anymore, the API request can return
        # strange results.
        login <- vapply (body, function (i) i$login, character (1L))
        ctb_id <- vapply (body, function (i) i$id, integer (1L))
        avatar_url <- vapply (body, function (i) i$avatar_url, character (1L))
        api_url <- vapply (body, function (i) i$url, character (1L))
        gh_url <- vapply (body, function (i) i$html_url, character (1L))
        contributions <- vapply (body, function (i) i$contributions, integer (1L))
    }

    ctbs <- data.frame (
        login = login,
        ctb_id = ctb_id,
        avatar_url = avatar_url,
        api_url = api_url,
        gh_url = gh_url,
        contributions = contributions
    )

    ctbs_user_info <- lapply (ctbs$login, function (ctb) {
        tryCatch (
            user_from_gh_api (ctb),
            error = function (e) NULL
        )
    })
    ctbs_user_info <- do.call (rbind, ctbs_user_info)

    if (!is.null (ctbs_user_info)) {
        ctbs <- dplyr::left_join (ctbs, ctbs_user_info, by = c ("login", "ctb_id")) |>
            dplyr::filter (login != "actions-user")
    }

    return (ctbs)
}
rm_data_contribs_from_gh_api <-
    memoise::memoise (rm_data_contribs_from_gh_api_internal)

user_from_gh_api <- function (user) {

    u_base <- "https://api.github.com/users/"
    u_endpoint <- paste0 (u_base, user)

    req <- httr2::request (u_endpoint) |>
        add_gh_token_to_req ()
    resp <- httr2::req_retry (req) |>
        httr2::req_perform ()
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
