#' Construct user-by-user square matrices of strengths of relation between
#' users.
#'
#' @param user_data Result of `lapply(logins, repometrics_data_user)`.
#' @noRd
user_relation_matrices <- function (user_data) {

    user_data <- add_user_login_cols (user_data)

    dat <- empty_user_mat (user_data, n = 4L)


}

empty_user_mat <- function (user_data, n = 4L) {

    n_users <- length (user_data)
    m <- array (NA_real_, dim = c (n_users, n_users, n))
    rownames (m) <- colnames (m) <- names (user_data)

    return (m)
}

#' Add 'login' columns to all user data, so each element can be combined.
#' @noRd
add_user_login_cols <- function (user_data) {

    is_df <- vapply (user_data [[1]], is.data.frame, logical (1L))
    index <- which (is_df)

    lapply (seq_along (user_data), function (u) {
        lapply (user_data [[u]] [index], function (i) {
            if (nrow (i) > 0L) {
                i$login <- names (user_data) [u]
            }
            return (i)
        })
    })
}

user_relate_commits <- function (user_data) {

    commits <- lapply (seq_along (user_data), function (u) {


    })
}
