#' Return a tripartite vector of (mean, median, sum) of a given input vector
#'
#' @param x A numberic vector
#' @return Named vector of three numeric values of (mean, median, sum)
#' @noRd
mn_med_sum <- function (x) {
    ret <- c (mean = 0, median = 0, sum = 0)
    if (length (x) > 0) {
        ret <- c (mean = mean (x), median = stats::median (x), sum = sum (x))
    }
    return (ret)
}

#' Convert single values of length 0 or NULL to `NA_integer`.
#' @noRd
null2na_int <- function (x) {
    ifelse (length (x) == 0, NA_integer_, x)
}
