#' Construct user-by-user square matrices of strengths of relation between
#' users.
#'
#' @param users Result of `lapply(logins, repometrics_data_user)`.
#' @noRd
user_relation_matrices <- function (users) {


}

empty_user_mat <- function (users) {
    n <- length (users)
    matrix (NA_real, dim = c (n, n))
}
