rm_data_releases_from_gh_api_internal <- function (path, # nolint
                                                   n_per_page = 100L,
                                                   latest_only = FALSE) {

    checkmate::assert_integerish (n_per_page)
    checkmate::assert_logical (latest_only)

    if (latest_only) {
        n_per_page <- 1L
    }

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"

    u_endpoint <- gh_rest_api_endpoint (path = path, endpoint = "releases")

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
        if (is_test_env || latest_only) {
            next_page <- NULL
        }

        req <- httr2::request (u_endpoint) |>
            httr2::req_url_query (per_page = n_per_page) |>
            httr2::req_url_query (page = next_page)
    }

    data.frame (
        id = vapply (body, function (i) i$id, integer (1L)),
        author_login =
            vapply (body, function (i) i$author$login, character (1L)),
        author_id = vapply (body, function (i) i$author$id, integer (1L)),
        tag_name = vapply (body, function (i) i$tag_name, character (1L)),
        target_commitish =
            vapply (body, function (i) i$target_commitish, character (1L)),
        name = vapply (body, function (i) i$name, character (1L)),
        draft = vapply (body, function (i) i$draft, logical (1L)),
        prerelease = vapply (body, function (i) i$prerelease, logical (1L)),
        created_at = vapply (body, function (i) i$created_at, character (1L)),
        published_at =
            vapply (body, function (i) i$published_at, character (1L))
    )
}
rm_data_releases_from_gh_api <-
    memoise::memoise (rm_data_releases_from_gh_api_internal)
