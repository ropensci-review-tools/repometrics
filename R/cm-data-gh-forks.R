gh_forks_qry <- function (org = "ropensci-review-tools",
                          repo = "repometrics",
                          n_per_page = 100L,
                          end_cursor = NULL) {

    checkmate::assert_integerish (n_per_page)

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

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

cm_data_repo_forks <- function (path, n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    or <- org_repo_from_path (path)
    end_cursor <- fork_data <- NULL
    has_next_page <- TRUE

    while (has_next_page) {

        q <- gh_forks_qry (
            org = or [1],
            repo = or [2],
            end_cursor = end_cursor,
            n_per_page = n_per_page
        )
        dat <- gh::gh_gql (query = q)

        fork_data <- c (fork_data, dat$data$repository$forks$nodes)

        has_next_page <- dat$data$repository$forks$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$forks$pageInfo$endCursor
        if (is_test_env) {
            has_next_page <- FALSE
        }
    }

    org_repo <- vapply (fork_data, function (i) i$nameWithOwner, character (1L))
    created <- vapply (fork_data, function (i) i$createdAt, character (1L))

    data.frame (
        org_repo = org_repo,
        created = as.Date (created)
    )
}
