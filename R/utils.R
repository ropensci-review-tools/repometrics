mn_med_sum <- function (x) {
    ret <- c (mean = 0, median = 0, sum = 0)
    if (length (x) > 0) {
        ret <- c (mean = mean (x), median = stats::median (x), sum = sum (x))
    }
    return (ret)
}

null2na_int <- function (x) {
    ifelse (length (x) == 0, NA_integer_, x)
}
