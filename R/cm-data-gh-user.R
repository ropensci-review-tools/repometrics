#' Query for both followers and following
#' @noRd
gh_user_follow_qry <- function (login = "",
                                followers = TRUE,
                                n_per_page = 100L,
                                end_cursor = NULL) {

    checkmate::assert_integerish (n_per_page)

    object <- ifelse (followers, "followers", "following")

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            ", object, "(first: ", n_per_page, after_txt, ") {
                pageInfo {
                    hasNextPage
                    endCursor
                }
                totalCount
                nodes {
                    login
                }
            }
        }
    }")

    return (q)
}

gh_user_follow_internal <- function (login, followers = TRUE, n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    end_cursor <- result <- NULL
    has_next_page <- TRUE

    object <- ifelse (followers, "followers", "following")

    while (has_next_page) {

        q <- gh_user_follow_qry (
            login = login,
            followers = followers,
            end_cursor = end_cursor,
            n_per_page = n_per_page
        )
        dat <- gh::gh_gql (query = q)

        result <- c (result, dat$data$user [[object]]$nodes)

        has_next_page <- dat$data$user [[object]]$pageInfo$hasNextPage
        end_cursor <- dat$data$user [[object]]$pageInfo$endCursor
        if (is_test_env) {
            has_next_page <- FALSE
        }
    }

    result <- vapply (result, function (i) i$login, character (1L))

    return (result)
}
gh_user_follow <- memoise::memoise (gh_user_follow_internal)

gh_user_commit_cmt_qry <- function (login = "",
                                    n_per_page = 100L,
                                    end_cursor = NULL) {

    checkmate::assert_integerish (n_per_page)

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            commitComments (first: ", n_per_page, after_txt, ") {
                pageInfo {
                    hasNextPage
                    endCursor
                }
                totalCount
                nodes {
                    author {
                        login
                    }
                    createdAt
                    repository {
                        url
                        stargazerCount
                    }
                    url
                }
            }
        }
    }")

    return (q)
}

gh_user_commit_cmt_internal <- function (login, n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    end_cursor <- timestamps <- repourl <- repo_stargazers <- NULL
    has_next_page <- TRUE

    while (has_next_page) {

        q <- gh_user_commit_cmt_qry (
            login = login,
            end_cursor = end_cursor,
            n_per_page = n_per_page
        )
        dat <- gh::gh_gql (query = q)

        nodes <- dat$data$user$commitComments$nodes
        timestamps <- c (
            timestamps,
            vapply (nodes, function (i) i$createdAt, character (1L))
        )
        repourl <- c (
            repourl,
            vapply (nodes, function (i) i$repository$url, character (1L))
        )
        repo_stargazers <- c (
            repo_stargazers,
            vapply (nodes, function (i) i$repository$stargazerCount, integer (1L))
        )

        has_next_page <- dat$data$user$commitComments$pageInfo$hasNextPage
        end_cursor <- dat$data$user$commitComments$pageInfo$endCursor
        if (is_test_env) {
            has_next_page <- FALSE
        }
    }

    repourl <- gsub ("https://github.com/", "", repourl, fixed = TRUE)
    repourl <- strsplit (repourl, "\\/")
    org <- vapply (repourl, function (i) i [1], character (1L))
    repo <- vapply (repourl, function (i) i [2], character (1L))

    return (data.frame (
        org = org,
        repo = repo,
        timestamp = timestamps,
        stargazers = repo_stargazers
    ))
}
gh_user_commit_cmt <- memoise::memoise (gh_user_commit_cmt_internal)

# These are aggregated per repository, so no page cursors needed. Only
# restriction is maxRepositories, but that also does not allow further paging.
gh_user_contrib_collect_commits_qry <- function (login = "",
                                                 ended_at = Sys.time (),
                                                 nyears = 1,
                                                 n_per_page = 100L) {

    # GraphQL API here has restriction:
    # "The total time spanned by 'from' and 'to' must not exceed 1 year"
    checkmate::assert_numeric (nyears, len = 1L, upper = 1)

    from <- format (ended_at - 60 * 60 * 24 * 365 * nyears, "%Y-%m-%dT%H:%M:%S")
    ended_at <- format (ended_at, "%Y-%m-%dT%H:%M:%S")

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            contributionsCollection (from: \"", from, "\", to: \"", ended_at, "\") {
                startedAt
                endedAt
                commitContributionsByRepository (maxRepositories: ", n_per_page, ") {
                    contributions (first: 1) {
                        pageInfo {
                            hasNextPage
                            endCursor
                        }
                        totalCount
                        nodes {
                            repository {
                                url
                            }
                        }
                    }
                }
            }
        }
    }")

    return (q)
}

gh_user_contrib_collect_commits_internal <- function (login,
                                                      ended_at = Sys.time (),
                                                      nyears = 1,
                                                      n_per_page = 100L) {

    q <- gh_user_contrib_collect_commits_qry (
        login = login,
        ended_at = ended_at,
        nyears = nyears,
        n_per_page = n_per_page
    )
    dat <- gh::gh_gql (query = q)

    started_at <- dat$data$user$contributionsCollection$startedAt
    ended_at <- dat$data$user$contributionsCollection$endedAt

    commits <- dat$data$user$contributionsCollection$commitContributionsByRepository

    repos <- vapply (
        commits,
        function (i) i$contributions$nodes [[1]]$repository$url,
        character (1L)
    )
    num_commits <- vapply (commits, function (i) i$contributions$totalCount, integer (1L))

    res <- data.frame (
        repo = gsub ("https://github.com/", "", repos, fixed = TRUE),
        num_commits = num_commits
    )
    attr (res, "started_at") <- started_at
    attr (res, "ended_at") <- ended_at

    return (res)
}
gh_user_contrib_collect_commits <-
    memoise::memoise (gh_user_contrib_collect_commits_internal)

gh_user_contrib_collect_issues_qry <- function (login = "",
                                                ended_at = Sys.time (),
                                                nyears = 1,
                                                n_per_page = 100L,
                                                end_cursor = NULL) {

    # GraphQL API here has restriction:
    # "The total time spanned by 'from' and 'to' must not exceed 1 year"
    checkmate::assert_numeric (nyears, len = 1L, upper = 1)

    from <- format (ended_at - 60 * 60 * 24 * 365 * nyears, "%Y-%m-%dT%H:%M:%S")
    ended_at <- format (ended_at, "%Y-%m-%dT%H:%M:%S")

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            contributionsCollection (from: \"", from, "\", to: \"", ended_at, "\") {
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

gh_user_contrib_collect_issues_internal <- function (login,
                                                     ended_at = Sys.time (),
                                                     nyears = 1,
                                                     n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    created_at <- closed_at <- org_repo <- issue_num <- end_cursor <-
        num_issue_comments <- num_issue_participants <- num_repo_languages <- NULL
    repo_languages <- list ()
    total_issue_contribs <- 0L
    has_next_page <- TRUE

    while (has_next_page) {

        q <- gh_user_contrib_collect_issues_qry (
            login = login,
            ended_at = ended_at,
            nyears = nyears,
            n_per_page = n_per_page,
            end_cursor = end_cursor
        )
        dat <- gh::gh_gql (query = q)

        collection <- dat$data$user$contributionsCollection
        collection_started_at <- collection$startedAt
        collection_ended_at <- collection$endedAt

        has_next_page <- collection$issueContributions$pageInfo$hasNextPage
        end_cursor <- collection$issueContributions$pageInfo$endCursor

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
    attr (res, "ended_at") <- collection_ended_at

    return (res)
}
gh_user_contrib_collect_issues <-
    memoise::memoise (gh_user_contrib_collect_issues_internal)
