rm_data_issues_from_gh_api_internal <- function (path, n_per_page = 100) { # nolint

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    u_endpoint <- gh_rest_api_endpoint (path = path, endpoint = "issues")

    req0 <- req <- httr2::request (u_endpoint) |>
        httr2::req_url_query (state = "all") |>
        httr2::req_url_query (per_page = n_per_page) |>
        add_gh_token_to_req ()

    body <- NULL
    next_page <- 1

    while (!is.null (next_page)) {

        resp <- httr2::req_retry (req, max_tries = 5L) |>
            httr2::req_perform ()

        if (httr2::resp_is_error (resp)) {
            next_page <- NULL
        } else {
            body <- c (body, httr2::resp_body_json (resp))

            next_page <- gh_next_page (resp)
            if (is_test_env) {
                next_page <- NULL
            }

            req <- httr2::req_url_query (req0, page = next_page)
        }
    }

    issues <- data.frame (
        number = vapply (body, function (i) i$number, integer (1L)),
        title = vapply (body, function (i) i$title, character (1L)),
        id = vapply (body, function (i) i$id, double (1L)),
        url = vapply (body, function (i) i$html_url, character (1L)),
        user_login = vapply (body, function (i) i$user$login, character (1L)),
        user_id = vapply (body, function (i) i$user$id, integer (1L)),
        labels = vapply (body, function (i) {
            these_labels <-
                vapply (i$labels, function (j) j$name, character (1L))
            paste0 (these_labels, collapse = ", ")
        }, character (1L)),
        state = vapply (body, function (i) i$state, character (1L)),
        assignee = vapply (
            body,
            function (i) null2na_char (i$assignee$login),
            character (1L)
        ),
        comments = vapply (body, function (i) i$comments, integer (1L)),
        created_at = vapply (body, function (i) i$created_at, character (1L)),
        updated_at = vapply (body, function (i) i$updated_at, character (1L)),
        closed_at = vapply (
            body,
            function (i) null2na_char (i$closed_at),
            character (1L)
        ),
        issue_body = vapply (
            body,
            function (i) null2na_char (i$body),
            character (1L)
        ),
        closed_by = vapply (
            body,
            function (i) null2na_char (i$closed_by$login),
            character (1L)
        ),
        state_reason = vapply (
            body,
            function (i) null2na_char (i$state_reason),
            character (1L)
        )
    )

    issues <- cbind (issues, get_issue_reactions (body))

    return (issues)
}
rm_data_issues_from_gh_api <-
    memoise::memoise (rm_data_issues_from_gh_api_internal)

get_issue_reactions <- function (body) {

    reactions <- c (
        "+1", "-1", "laugh", "hooray", "confused", "heart", "rocket", "eyes"
    )

    reaction_counts <- lapply (reactions, function (i) {
        vapply (body, function (j) j$reaction [[i]], integer (1L))
    })

    reaction_counts <- do.call (cbind, reaction_counts)
    reactions [1:2] <- c ("plus1", "minus1")
    colnames (reaction_counts) <- paste0 ("reaction_", reactions)

    return (reaction_counts)
}

rm_data_issue_comments_from_gh_api_internal <- function (path, # nolint
                                                         n_per_page = 100) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    u_endpoint <-
        gh_rest_api_endpoint (path = path, endpoint = "issues/comments")

    req0 <- req <- httr2::request (u_endpoint) |>
        httr2::req_url_query (per_page = n_per_page) |>
        add_gh_token_to_req ()

    body <- NULL
    next_page <- 1

    while (!is.null (next_page)) {

        resp <- httr2::req_retry (req, max_tries = 5L) |>
            httr2::req_perform ()

        if (httr2::resp_is_error (resp)) {
            next_page <- NULL
        } else {
            body <- c (body, httr2::resp_body_json (resp))

            next_page <- gh_next_page (resp)
            if (is_test_env) {
                next_page <- NULL
            }

            req <- httr2::req_url_query (req0, page = next_page)
        }
    }

    issue_url <- vapply (body, function (i) i$issue_url, character (1L))
    comment_url <- vapply (body, function (i) i$html_url, character (1L))
    comment_id <- vapply (body, function (i) i$id, double (1L))
    issue_number <- as.integer (basename (issue_url))
    user_login <- vapply (body, function (i) i$user$login, character (1L))
    user_id <- vapply (body, function (i) i$user$id, integer (1L))
    created_at <- vapply (body, function (i) i$created_at, character (1L))
    updated_at <- vapply (body, function (i) i$updated_at, character (1L))
    issue_body <- vapply (body, function (i) i$body, character (1L))

    data.frame (
        issue_url = issue_url,
        issue_number = issue_number,
        comment_url = comment_url,
        comment_id = comment_id,
        user_login = user_login,
        user_id = user_id,
        created_at = created_at,
        updated_at = updated_at,
        issue_body = issue_body
    )
}
rm_data_issue_comments_from_gh_api <- # nolint
    memoise::memoise (rm_data_issue_comments_from_gh_api_internal)
