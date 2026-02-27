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

    object <- ifelse (followers, "followers", "following")

    result <- gh_gql_paginate (
        qry_fn = function (end_cursor, n_per_page) {
            gh_user_follow_qry (
                login = login, followers = followers,
                end_cursor = end_cursor, n_per_page = n_per_page
            )
        },
        extract_nodes = function (dat) dat$data$user [[object]]$nodes,
        extract_page = function (dat) dat$data$user [[object]]$pageInfo,
        n_per_page = n_per_page
    )

    vapply (result, function (i) i$login, character (1L))
}
gh_user_follow <- memoise::memoise (gh_user_follow_internal)
