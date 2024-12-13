#' Construct user-by-user square matrices of strengths of relation between
#' users.
#'
#' @param user_data Result of `lapply(logins, repometrics_data_user)`.
#' @noRd
user_relation_matrices <- function (user_data) {

    user_names <- names (user_data)
    user_data <- add_user_login_cols (user_data) |>
        combine_user_data ()

    dat <- empty_user_mat (user_data, user_names)


}

empty_user_mat <- function (user_data, user_names) {

    n_users <- length (user_names)
    n_fields <- length (user_data)
    m <- array (NA_real_, dim = c (n_users, n_users, n_fields))
    rownames (m) <- colnames (m) <- user_names
    attr (m, "dimnames") [[3]] <- names (user_data)

    return (m)
}

#' Add 'login' columns to all user data, so each element can be combined.
#' @noRd
add_user_login_cols <- function (user_data) {

    nms <- names (user_data)
    res <- lapply (seq_along (user_data), function (u) {
        nms_u <- names (user_data [[u]])
        res_u <- lapply (seq_along (user_data [[u]]), function (i) {
            ud <- user_data [[u]] [[i]]
            if (is.data.frame (ud) && nrow (ud) > 0L) {
                ud$login <- names (user_data) [u]
            } else if (is.character (ud)) {
                ud <- data.frame (ud, login = names (user_data) [u])
                names (ud) [1] <- names (user_data [[u]]) [i]
            }
            return (ud)
        })
        names (res_u) <- nms_u

        return (res_u)
    })
    names (res) <- nms

    return (res)
}

#' Combine all individual elements of 'user_data' for all users.
#'
#' The `add_user_login_cols` enables all data to be `rbind`-ed here.
#' @noRd
combine_user_data <- function (user_data) {

    data <- lapply (names (user_data [[1]]), function (n) {
        these <- lapply (user_data, function (i) i [[n]])
        res <- do.call (rbind, these)
        rownames (res) <- NULL
        return (res)
    })

    names (data) <- names (user_data [[1]])
    data$general <- NULL

    return (data)
}

user_relate_commits <- function (user_data) {

    commits <- lapply (seq_along (user_data), function (u) {


    })
}
