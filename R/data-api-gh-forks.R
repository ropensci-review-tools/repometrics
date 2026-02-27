gh_forks_qry <- function (org = "ropensci-review-tools",
                          repo = "repometrics",
                          n_per_page = 100L,
                          end_cursor = NULL) {

    checkmate::assert_integerish (n_per_page)

    after_txt <- gql_cursor_txt (end_cursor)

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
            forks (first: ", n_per_page, after_txt, ") {
                pageInfo {
                    hasNextPage
                    endCursor
                }
                nodes {
                    createdAt
                    nameWithOwner
                    url
                }
            }
        }
    }")

    return (q)
}

rm_data_repo_forks_internal <- function (path, n_per_page = 100L) {

    or <- org_repo_from_path (path)

    fork_data <- gh_gql_paginate (
        qry_fn = function (end_cursor, n_per_page) {
            gh_forks_qry (
                org = or [1], repo = or [2],
                end_cursor = end_cursor, n_per_page = n_per_page
            )
        },
        extract_nodes = function (dat) dat$data$repository$forks$nodes,
        extract_page = function (dat) dat$data$repository$forks$pageInfo,
        n_per_page = n_per_page
    )

    org_repo <- vapply (fork_data, function (i) i$nameWithOwner, character (1L))
    created <- vapply (fork_data, function (i) i$createdAt, character (1L))

    data.frame (
        org_repo = org_repo,
        created = as.Date (created)
    )
}
rm_data_repo_forks <- memoise::memoise (rm_data_repo_forks_internal)
