gh_stargazers_qry <- function (org = "ropensci-review-tools",
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

rm_data_repo_stargazers_internal <- function (path, n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    or <- org_repo_from_path (path)
    end_cursor <- stargazers <- NULL
    has_next_page <- TRUE

    while (has_next_page) {

        q <- gh_stargazers_qry (
            org = or [1],
            repo = or [2],
            end_cursor = end_cursor,
            n_per_page = n_per_page
        )
        dat <- gh::gh_gql (query = q)

        stargazers <- c (stargazers, dat$data$repository$stargazers$edges)

        has_next_page <- dat$data$repository$stargazers$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$stargazers$pageInfo$endCursor
        if (is_test_env) {
            has_next_page <- FALSE
        }
    }

    login <- vapply (stargazers, function (i) i$node$login, character (1L))
    starred_at <- vapply (stargazers, function (i) i$starredAt, character (1L))

    data.frame (
        login = login,
        starred_at = as.Date (starred_at)
    )
}
rm_data_repo_stargazers <- memoise::memoise (rm_data_repo_stargazers_internal)
