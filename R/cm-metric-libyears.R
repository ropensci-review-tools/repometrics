#' Extract CHAOSS "libyears" metric
#'
#' @param path Local path to repository
#' @noRd
cm_metric_libyears <- function (path) {

    deps <- rm_data_libyears (path)

    return (mn_med_sum (deps$libyears))
}
