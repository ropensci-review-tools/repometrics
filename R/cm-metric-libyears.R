#' Extract CHAOSS "libyears" metric
#'
#' \url{https://chaoss.community/kb/metric-libyears/}
#' Lower or negatve values indicate that a project is more up-to-date.
#'
#' @param path Local path to repository
#' @noRd
cm_metric_libyears <- function (path) {

    deps <- rm_data_libyears (path)

    return (mn_med_sum (deps$libyears))
}
