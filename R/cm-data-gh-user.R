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
