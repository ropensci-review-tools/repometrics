gh_stargazers_qry <- function (org = "ropensci-review-tools",
                               repo = "repometrics",
                               n_per_page = 100L,
                               end_cursor = NULL) {

    checkmate::assert_integerish (n_per_page)

    after_txt <- gql_cursor_txt (end_cursor)

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
            stargazers (first: ", n_per_page, after_txt, ") {
                pageInfo {
                    hasNextPage
                    endCursor
                }
                edges {
                    starredAt
                    node {
                        login
                    }
                }
            }
        }
    }")

    return (q)
}

rm_data_repo_stargazers_internal <- function (path, n_per_page = 100L) { # nolint

    or <- org_repo_from_path (path)

    stargazers <- gh_gql_paginate (
        qry_fn = function (end_cursor, n_per_page) {
            gh_stargazers_qry (
                org = or [1], repo = or [2],
                end_cursor = end_cursor, n_per_page = n_per_page
            )
        },
        extract_nodes = function (dat) dat$data$repository$stargazers$edges,
        extract_page = function (dat) dat$data$repository$stargazers$pageInfo,
        n_per_page = n_per_page
    )

    login <- vapply (stargazers, function (i) i$node$login, character (1L))
    starred_at <- vapply (stargazers, function (i) i$starredAt, character (1L))

    data.frame (
        login = login,
        starred_at = as.Date (starred_at)
    )
}
rm_data_repo_stargazers <- memoise::memoise (rm_data_repo_stargazers_internal)
