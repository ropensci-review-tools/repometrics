rm_data_issues_from_gh_api_internal <- function (path, n_per_page = 100) { # nolint

    n_per_page <- n_per_page_in_tests (n_per_page)
    u_endpoint <- gh_rest_api_endpoint (path = path, endpoint = "issues")
    body <- gh_rest_paginate (u_endpoint, n_per_page = n_per_page, state = "all")

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

    n_per_page <- n_per_page_in_tests (n_per_page)
    u_endpoint <-
        gh_rest_api_endpoint (path = path, endpoint = "issues/comments")
    body <- gh_rest_paginate (u_endpoint, n_per_page = n_per_page)

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
