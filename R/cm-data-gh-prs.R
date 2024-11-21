prs_from_gh_api <- function (path, n_per_page = 100) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"

    u_endpoint <- gh_rest_api_endpoint (path = path, endpoint = "pulls")

    req0 <- req <- httr2::request (u_endpoint) |>
        httr2::req_url_query (state = "all") |>
        httr2::req_url_query (per_page = n_per_page) |>
        add_gh_token_to_req ()

    body <- NULL
    next_page <- 1

    while (!is.null (next_page)) {

        resp <- httr2::req_perform (req)
        httr2::resp_check_status (resp)

        body <- c (body, httr2::resp_body_json (resp))

        next_page <- gh_next_page (resp)
        if (is_test_env) {
            next_page <- NULL
        }

        req <- httr2::req_url_query (req0, page = next_page)
    }

    data.frame (
        url = vapply (body, function (i) i$html_url, character (1L)),
        id = vapply (body, function (i) i$number, numeric (1L)),
        number = vapply (body, function (i) i$number, integer (1L)),
        state = vapply (body, function (i) i$state, character (1L)),
        title = vapply (body, function (i) i$title, character (1L)),
        user_login = vapply (body, function (i) i$user$login, character (1L)),
        user_id = vapply (body, function (i) i$user$id, integer (1L)),
        pr_body = vapply (body, function (i) null2na_char (i$body), character (1L)),
        created_at = vapply (body, function (i) i$created_at, character (1L)),
        updated_at = vapply (body, function (i) i$updated_at, character (1L)),
        closed_at = vapply (
            body,
            function (i) null2na_char (i$closed_at),
            character (1L)
        ),
        merged_at = vapply (
            body,
            function (i) null2na_char (i$merged_at),
            character (1L)
        ),
        merge_commit_sha = vapply (
            body,
            function (i) null2na_char (i$merge_commit_sha),
            character (1L)
        ),
        assignee = vapply (body, function (i) null2na_char (i$assignee), character (1L)),
        requested_reviewers = vapply (
            body,
            function (i) null2na_char (i$requested_reviewers),
            character (1L)
        ),
        draft = vapply (body, function (i) i$draft, logical (1L))
    )
}
