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
            if (years_done > nyears) {
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
