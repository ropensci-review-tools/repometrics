# All functions accept same parameters, even through not all parameters are
# used in all functions.

gh_user_general_qry <- function (login = "") {

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            name
            location
            bio
            company
            email
            organizations (first: 100) {
                nodes {
                    location
                    name
                    resourcePath
                    url
                    websiteUrl
                    membersWithRole (first: 1) {
                        totalCount
                    }
                }
            }
            avatarUrl
            repositories (isFork: false, first: 1) {
                totalCount
            }
            repositoriesContributedTo (first: 1) {
                totalCount
            }
            starredRepositories (first: 1) {
                totalCount
            }
        }
    }")

    return (q)
}

# Only uses `login` param:
gh_user_general_internal <- function (login = "",
                                      end_date = Sys.Date (),
                                      nyears = 1,
                                      n_per_page = 100L) {

    q <- gh_user_general_qry (login = login)
    dat <- gh::gh_gql (query = q)

    user <- dat$data$user

    user_dat <- data.frame (
        login = user$login,
        name = null2na_char (user$name),
        email = null2na_char (user$email),
        location = null2na_char (user$location),
        company = null2na_char (user$company),
        bio = null2na_char (user$bio),
        avatarUrl = null2na_char (user$avatarUrl),
        num_repositories = null2na_int (user$repositories$totalCount),
        repos_contributed_to =
            null2na_int (user$repositoriesContributedTo$totalCount),
        num_starred_repos = null2na_int (user$starredRepositories$totalCount)
    )

    orgs <- user$organizations$nodes

    org_name <- org_gh_org <- org_url <-
        org_web_url <- org_location <- character (0L)
    org_num_members <- integer (0L)

    if (length (orgs) > 0L) {

        org_name <- vapply (orgs, function (i) i$name, character (1L))
        org_gh_org <- vapply (orgs, function (i) i$resourcePath, character (1L))
        org_url <- vapply (orgs, function (i) i$url, character (1L))
        org_web_url <- vapply (
            orgs,
            function (i) null2na_char (i$websiteUrl),
            character (1L)
        ) |> null2na_char ()
        org_location <- vapply (
            orgs,
            function (i) null2na_char (i$location),
            character (1L)
        ) |> null2na_char ()
        org_num_members <- vapply (
            orgs,
            function (i) i$membersWithRole$totalCount,
            integer (1L)
        )
    }

    orgs <- data.frame (
        name = org_name,
        gh_org = org_gh_org,
        url = org_url,
        web_url = org_web_url,
        location = org_location,
        num_members = org_num_members
    )

    list (user = user_dat, orgs = orgs)
}
gh_user_general <- memoise::memoise (gh_user_general_internal)

#' Query for both followers and following
#'
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

# Uses all parameters except `end_date` at `nyears`.
gh_user_follow_internal <- function (login,
                                     end_date = Sys.Date (),
                                     nyears = 1,
                                     n_per_page = 100L,
                                     followers = TRUE) {

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
                        nameWithOwner
                        stargazerCount
                    }
                    url
                }
            }
        }
    }")

    return (q)
}

# Uses all params except `end_date` and `nyears`:
gh_user_commit_cmt_internal <- function (login,
                                         end_date = Sys.Date (),
                                         nyears = 1,
                                         n_per_page = 100L) {

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
            vapply (
                nodes,
                function (i) i$repository$nameWithOwner,
                character (1L)
            )
        )
        repo_stargazers <- c (
            repo_stargazers,
            vapply (
                nodes,
                function (i) i$repository$stargazerCount,
                integer (1L)
            )
        )

        has_next_page <- dat$data$user$commitComments$pageInfo$hasNextPage
        end_cursor <- dat$data$user$commitComments$pageInfo$endCursor
        if (is_test_env) {
            has_next_page <- FALSE
        }
    }

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
#
# Uses all parameters
gh_user_commits_qry <- function (login = "",
                                 end_date = Sys.Date (),
                                 nyears = 1,
                                 n_per_page = 100L,
                                 end_cursor = NULL) {

    # GraphQL API here has restriction:
    # "The total time spanned by 'from' and 'to' must not exceed 1 year"
    nyears <- min (c (1, nyears))
    checkmate::assert_numeric (nyears, len = 1L, upper = 1)

    # These 'format' calls pad with hms = "00:00:00":
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
                totalCommitContributions
                commitContributionsByRepository (maxRepositories: ", n_per_page, ") {
                    contributions (first: ", n_per_page, after_txt, ") {
                        pageInfo {
                            hasNextPage
                            endCursor
                        }
                        totalCount
                        nodes {
                            occurredAt
                            commitCount
                            repository {
                                nameWithOwner
                            }
                        }
                    }
                }
            }
        }
    }")

    return (q)
}

gh_user_commits_internal <- function (login,
                                      end_date = Sys.Date (),
                                      nyears = 1,
                                      n_per_page = 100L) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    repos <- num_commits <- dates <- end_cursor <- end_cursors <- NULL

    has_next_page <- TRUE
    years_done <- 0L # Used below if nyears > 1

    while (has_next_page) {

        q <- gh_user_commits_qry (
            login = login,
            end_date = end_date,
            nyears = nyears,
            n_per_page = n_per_page,
            end_cursor = end_cursor
        )
        dat <- gh::gh_gql (query = q)

        collection <- dat$data$user$contributionsCollection
        commits <- collection$commitContributionsByRepository

        # Query always returns `n_per_page` items, even when empty, so empty
        # ones must first be removed:
        lens <- vapply (
            commits,
            function (i) length (i$contributions$nodes),
            integer (1L)
        )
        commits <- commits [which (lens > 0)]

        repos_i <- vapply (
            commits,
            function (i) i$contributions$nodes [[1]]$repository$nameWithOwner,
            character (1L)
        )

        dates_i <- lapply (commits, function (i) {
            vapply (
                i$contributions$nodes,
                function (j) j$occurredAt,
                character (1L)
            )
        })
        n_i <- vapply (dates_i, length, integer (1L))
        dates <- c (dates, unlist (dates_i))
        commit_count_i <- lapply (commits, function (i) {
            vapply (
                i$contributions$nodes,
                function (j) j$commitCount,
                integer (1L)
            )
        })
        num_commits <- c (num_commits, unlist (commit_count_i))

        repos <- c (repos, rep (repos_i, times = n_i))

        has_next_pages <- vapply (commits, function (i) {
            i$contributions$pageInfo$hasNextPage
        }, logical (1L))
        end_cursors_these <- vapply (commits, function (i) {
            i$contributions$pageInfo$endCursor
        }, character (1L))
        end_cursors_these <- unique (end_cursors_these [which (has_next_pages)])
        end_cursors <- c (end_cursors, end_cursors_these)
        has_next_page <- length (end_cursors) > 0L && !is_test_env
        if (has_next_page) {
            end_cursor <- end_cursors [1L]
            end_cursors <- end_cursors [-1L]
        } else if (nyears > 1) {
            this_year <-
                regmatches (end_date, regexpr ("^[0-9]{4}", end_date)) |>
                as.integer ()
            annual_days <- ifelse (this_year %% 4 == 0, 366L, 365L)
            end_date <- end_date - annual_days
            years_done <- years_done + 1
            if (years_done < nyears) {
                has_next_page <- FALSE
                end_cursor <- NULL
            }
        }
    }

    started_at <- dat$data$user$contributionsCollection$startedAt
    end_date <- dat$data$user$contributionsCollection$endedAt

    # suppress no visible binding note:
    repo <- date <- NULL
    res <- data.frame (
        repo = repos,
        num_commits = num_commits,
        date = dates
    ) |>
        dplyr::arrange (repo, date)
    attr (res, "started_at") <- started_at
    attr (res, "end_date") <- end_date

    return (res)
}
gh_user_commits <- memoise::memoise (gh_user_commits_internal)

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
