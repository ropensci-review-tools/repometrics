#' Extract CHAOSS "libyears" metric
#'
#' @param path Local path to repository
#' @noRd
cm_metric_libyears <- function (path) {

    deps <- cm_data_libyears (path)

    c (mean = mean (deps$libyears), median = stats::median (deps$libyears))
}
