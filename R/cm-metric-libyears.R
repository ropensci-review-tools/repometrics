#' Extract CHAOSS "libyears" metric
#'
#' \url{https://chaoss.community/kb/metric-libyears/}
#' Lower or negatve values indicate that a project is more up-to-date.
#'
#' @param path Local path to repository
#' @param end_date Not used here, but specified for consistent interface to all
#' metric fns.
#' @noRd
cm_data_libyears <- function (path, end_date = NULL) {

    deps <- rm_data_libyears (path)

    return (mn_med_sum (deps$libyears))
}

cm_metric_libyears <- function (path, end_date = NULL) {
    dat <- cm_data_libyears (path)
    return (dat [["mean"]])
}

cm_metric_dependency_count <- function (path, end_date = NULL) {

    deps <- rm_data_libyears (path)

    return (nrow (deps))
}

cm_metric_libyears_url <- function () {
    "chaoss.community/kb/metric-libyears"
}

cm_metric_dependency_count_url <- function () {
    "chaoss.community/kb/metric-libyears"
}
