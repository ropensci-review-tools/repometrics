# Uses all parameters
gh_user_issues_qry <- function (login = "",
                                end_date = Sys.Date (),
                                nyears = 1,
                                n_per_page = 100L,
                                end_cursor = NULL) {

    # GraphQL API here has restriction:
    # "The total time spanned by 'from' and 'to' must not exceed 1 year"
    nyears <- min (c (1, nyears))
    checkmate::assert_numeric (nyears, len = 1L, upper = 1)

    from <- format (end_date - 365.25 * nyears, "%Y-%m-%dT%H:%M:%S")
    end_date <- format (end_date, "%Y-%m-%dT%H:%M:%S")

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            contributionsCollection (from: \"", from, "\", to: \"", end_date, "\") {
                startedAt
                endedAt
                issueContributions (first: ", n_per_page, after_txt, ") {
                    pageInfo {
                        hasNextPage
                        endCursor
                    }
                    totalCount
                    nodes {
                        issue {
                            createdAt
                            closedAt
                            number
                            comments {
                                totalCount
                            }
                            participants {
                                totalCount
                            }
                            repository {
                                nameWithOwner
                                languages (first: ", n_per_page, ") {
                                    totalCount
                                    edges {
                                        size
                                        node {
                                            name
                                        }
                                    }
                                }
                            }
                        }
                    }
                    }
                }
            }
        }
    }")

    return (q)
}

# Uses all parameters
gh_user_issues_internal <- function (login,
                                     end_date = Sys.Date (),
                                     nyears = 1,
                                     n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    created_at <- closed_at <- org_repo <- issue_num <- end_cursor <-
        num_issue_comments <- num_issue_participants <-
        num_repo_languages <- NULL
    repo_languages <- list ()
    total_issue_contribs <- 0L
    has_next_page <- TRUE

    while (has_next_page) {

        q <- gh_user_issues_qry (
            login = login,
            end_date = end_date,
            nyears = nyears,
            n_per_page = n_per_page,
            end_cursor = end_cursor
        )
        dat <- gh::gh_gql (query = q)

        collection <- dat$data$user$contributionsCollection
        collection_started_at <- collection$startedAt
        collection_end_date <- collection$endedAt

        has_next_page <- collection$issueContributions$pageInfo$hasNextPage
        end_cursor <- collection$issueContributions$pageInfo$endCursor
        if (is_test_env) {
            has_next_page <- FALSE
        }

        total_issue_contribs <-
            total_issue_contribs + collection$issueContributions$totalCount
        issues <- collection$issueContributions$nodes

        created_at <- c (
            created_at,
            vapply (issues, function (i) i$issue$createdAt, character (1L))
        )
        closed_at <- c (
            closed_at,
            vapply (
                issues,
                function (i) null2na_char (i$issue$closedAt),
                character (1L)
            )
        )
        org_repo <- c (
            org_repo,
            vapply (
                issues,
                function (i) i$issue$repository$nameWithOwner,
                character (1L)
            )
        )
        issue_num <- c (
            issue_num,
            vapply (issues, function (i) i$issue$number, integer (1L))
        )
        num_issue_comments <- c (
            num_issue_comments,
            vapply (
                issues,
                function (i) i$issue$comments$totalCount,
                integer (1L)
            )
        )
        num_issue_participants <- c (
            num_issue_participants,
            vapply (
                issues,
                function (i) i$issue$participants$totalCount,
                integer (1L)
            )
        )
        num_repo_languages <- c (
            num_repo_languages,
            vapply (
                issues,
                function (i) i$issue$repository$languages$totalCount,
                integer (1L)
            )
        )

        repo_languages <- c (repo_languages, lapply (issues, function (i) {
            langs <- i$issue$repository$languages$edges
            names <- vapply (langs, function (j) j$node$name, character (1L))
            sizes <- vapply (langs, function (j) j$size, integer (1L))
            data.frame (name = names, size = sizes)
        }))
    }

    res <- data.frame (
        opened_at = created_at,
        closed_at = closed_at,
        org_repo = org_repo,
        issue_num = issue_num,
        num_issue_comments = num_issue_comments,
        num_issue_participants = num_issue_participants,
        num_repo_languages = num_repo_languages,
        repo_languages = I (repo_languages)
    )

    attr (res, "started_at") <- collection_started_at
    attr (res, "end_date") <- collection_end_date

    return (res)
}
gh_user_issues <- memoise::memoise (gh_user_issues_internal)

gh_user_issue_cmts_qry <- function (login = "",
                                    nyears = 1,
                                    n_per_page = 100L,
                                    end_cursor = NULL) {

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            issueComments (first: ", n_per_page, after_txt, ", orderBy: { field: UPDATED_AT, direction: DESC } ) {
                pageInfo {
                    hasNextPage
                    endCursor
                }
                totalCount
                nodes {
                    createdAt
                    issue {
                        number
                        comments {
                            totalCount
                        }
                        participants {
                            totalCount
                        }
                        repository {
                            nameWithOwner
                        }
                    }
                }
            }
        }
    }")

    return (q)
}

# Uses all parameters except `end_date`
gh_user_issue_cmts_internal <- function (login,
                                         end_date = Sys.Date (),
                                         nyears = 1,
                                         n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    issue_comment_counts <- 0L
    end_cursor <- created_at <- org_repo <- issue_num <-
        num_comments <- num_participants <- NULL
    has_next_page <- TRUE

    start_timestamp <-
        format (Sys.Date () - 365.25 * nyears, "%Y-%m-%dT%H:%M:%S")
    start_timestamp <- as.POSIXct (start_timestamp)

    while (has_next_page) {

        q <- gh_user_issue_cmts_qry (
            login = login,
            n_per_page = n_per_page,
            end_cursor = end_cursor
        )
        dat <- gh::gh_gql (query = q)

        has_next_page <- dat$data$user$issueComments$pageInfo$hasNextPage
        end_cursor <- dat$data$user$issueComments$pageInfo$endCursor
        if (is_test_env) {
            has_next_page <- FALSE
        }

        issue_comment_counts <-
            issue_comment_counts + dat$data$user$issueComments$totalCount

        if (issue_comment_counts > 0L) {

            nodes <- dat$data$user$issueComments$nodes

            created_at <- c (
                created_at,
                vapply (nodes, function (i) i$createdAt, character (1L))
            )
            org_repo <- c (
                org_repo,
                vapply (
                    nodes,
                    function (i) i$issue$repository$nameWithOwner,
                    character (1L)
                )
            )
            issue_num <- c (
                issue_num,
                vapply (
                    nodes, function (i) i$issue$number,
                    integer (1L)
                )
            )
            num_comments <- c (
                num_comments,
                vapply (
                    nodes,
                    function (i) i$issue$comments$totalCount,
                    integer (1L)
                )
            )
            num_participants <- c (
                num_participants,
                vapply (
                    nodes,
                    function (i) i$issue$participants$totalCount,
                    integer (1L)
                )
            )

            if (utils::tail (as.POSIXct (created_at), 1) < start_timestamp) {
                has_next_page <- FALSE
            }
        } # else will always have has_next_page = FALSE
    }

    data.frame (
        org_repo = org_repo,
        issue_num = issue_num,
        created_at = created_at,
        num_comments = num_comments,
        num_participants = num_participants
    )
}
gh_user_issue_cmts <- memoise::memoise (gh_user_issue_cmts_internal)
