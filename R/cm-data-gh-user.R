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
