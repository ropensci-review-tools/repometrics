#' The GitHub GraphQL query to extract information from all pull requests.
#'
#' The Rest API pull request just dumps meta-information about each PR. Getting
#' details on individual PRs then requires looping over each PR number and
#' making a separate call. This GraphQL query does everything in a single call.
#'
#' @param org The GitHub organization.
#' @param repo The GitHub repository.
#' @param end_cursor The end cursor from the previous query.
#'
#' @return The GraphQL query to pass to a `gh::gh_gql()` call.
#' @noRd
gh_prs_qry <- function (org = "ropensci-review-tools",
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
            pullRequests (first: ", n_per_page, after_txt, ") {
                pageInfo {
                    hasNextPage
                    endCursor
                }
                nodes {
                    number
                    author {
                        login
                    }
                    state
                    closed
                    title
                    reviewDecision
                    reviews (first: 100) {
                        nodes {
                            author {
                                login
                            }
                            submittedAt
                            state
                            body
                        }
                    }
                    merged
                    mergedBy {
                        login
                    }
                    mergeCommit {
                        oid
                    }
                    assignees (first: 100) {
                        nodes {
                            name
                        }
                    }
                    createdAt
                    closedAt
                    updatedAt
                    closingIssuesReferences (first: 100) {
                        nodes {
                            number
                        }
                    }
                    commits (first: 100) {
                        nodes {
                            commit {
                                oid
                            }
                        }
                    }
                    additions
                    changedFiles
                    deletions
                    participants (first: 100) {
                        nodes {
                            login
                        }
                    }
                    labels (first: 100) {
                        nodes {
                            name,
                        }
                    }
                    body
                    totalCommentsCount
                    comments (last: 100) {
                        nodes {
                            createdAt,
                            author {
                                login
                            },
                            body
                        }
                    }
                }
            }
        }
    }")

    return (q)
}

cm_data_prs_from_gh_api_internal <- function (path, n_per_page = 30L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    or <- org_repo_from_path (path)
    end_cursor <- pr_data <- NULL
    has_next_page <- TRUE

    while (has_next_page) {

        q <- gh_prs_qry (
            org = or [1],
            repo = or [2],
            end_cursor = end_cursor,
            n_per_page = n_per_page
        )
        dat <- gh::gh_gql (query = q)

        pr_data <- c (pr_data, dat$data$repository$pullRequests$nodes)

        has_next_page <- dat$data$repository$pullRequests$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$pullRequests$pageInfo$endCursor
        if (is_test_env) {
            has_next_page <- FALSE
        }
    }

    num_commits <- vapply (
        pr_data,
        function (i) length (i$commits$nodes),
        integer (1L)
    )
    commit_oids <- vapply (pr_data, function (i) {
        oids <- vapply (i$commits$nodes, function (j) {
            j$commit$oid
        }, character (1L))
        paste0 (oids, collapse = ",")
    }, character (1L))
    participants <- vapply (pr_data, function (i) {
        p <- vapply (i$participants$nodes, function (j) j$login, character (1L))
        paste0 (p, collapse = ",")
    }, character (1L))
    comments <- lapply (pr_data, function (i) {
        created_at <- vapply (
            i$comments$nodes,
            function (j) j$createdAt,
            character (1L)
        )
        author <- vapply (
            i$comments$nodes,
            function (j) j$author$login,
            character (1L)
        )
        body <- vapply (i$comments$nodes, function (j) j$body, character (1L))
        data.frame (
            author = author,
            created_at = created_at,
            body = body
        )
    })
    closing_issue_refs <- lapply (pr_data, function (i) {
        vapply (
            i$closingIssuesReferences$nodes,
            function (j) j$number,
            integer (1L)
        )
    })
    reviews <- lapply (pr_data, function (i) {
        login <- vapply (
            i$reviews$nodes,
            function (j) j$author$login,
            character (1L)
        )
        state <- vapply (i$reviews$nodes, function (j) j$state, character (1L))
        submitted_at <- vapply (
            i$reviews$nodes,
            function (j) null2na_char (j$submittedAt),
            character (1L)
        )
        body <- vapply (i$reviews$nodes, function (j) j$body, character (1L))
        data.frame (
            login = login,
            state = state,
            submitted_at = submitted_at,
            body = body
        )
    })

    # A few extra pro-processing ones just to avoid long lines:
    user_login <- vapply (pr_data, function (i) i$author$login, character (1L))
    merged_by <- vapply (
        pr_data,
        function (i) null2na_char (i$mergedBy$login),
        character (1L)
    )
    merge_commit <- vapply (
        pr_data,
        function (i) null2na_char (i$mergeCommit$oid),
        character (1L)
    )
    review_decision <- vapply (
        pr_data,
        function (i) null2na_char (i$reviewDecision),
        character (1L)
    )
    closed_at <- vapply (
        pr_data,
        function (i) null2na_char (i$closedAt),
        character (1L)
    )
    changed_files <- vapply (pr_data, function (i) i$changedFiles, integer (1L))
    total_comments <- vapply (
        pr_data,
        function (i) i$totalCommentsCount,
        integer (1L)
    )

    data.frame (
        number = vapply (pr_data, function (i) i$number, integer (1L)),
        user_login = user_login,
        state = vapply (pr_data, function (i) i$state, character (1L)),
        merged = vapply (pr_data, function (i) i$merged, logical (1L)),
        merged_by = merged_by,
        merge_commit = merge_commit,
        closed = vapply (pr_data, function (i) i$closed, logical (1L)),
        title = vapply (pr_data, function (i) i$title, character (1L)),
        review_decision = review_decision,
        created_at = vapply (pr_data, function (i) i$createdAt, character (1L)),
        closed_at = closed_at,
        updated_at = vapply (pr_data, function (i) i$updatedAt, character (1L)),
        num_commits = num_commits,
        additions = vapply (pr_data, function (i) i$additions, integer (1L)),
        deletions = vapply (pr_data, function (i) i$deletions, integer (1L)),
        changed_files = changed_files,
        commit_oids = commit_oids,
        closing_issue_refs = I (closing_issue_refs),
        total_comments = total_comments,
        participants = participants,
        body = vapply (pr_data, function (i) i$body, character (1L)),
        comments = I (comments),
        reviews = I (reviews)
    )
}
cm_data_prs_from_gh_api <- memoise::memoise (cm_data_prs_from_gh_api_internal)
