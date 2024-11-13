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

set_num_cores <- function (num_cores) {
    if (num_cores <= 0L) {
        num_cores <- parallel::detectCores () + num_cores
    }
    num_cores <- min (num_cores, parallel::detectCores ())
    if (num_cores < 1L) {
        cli::cli_abort ("Number of cores must be at least 1")
    }
    return (num_cores)
}

pkg_name_from_path <- function (path) {
    desc <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    checkmate::assert_file_exists (desc)

    unname (read.dcf (desc) [, "Package"])
}

filter_git_hist <- function (h, n, step_days) {
    if (!is.null (n)) {
        h <- h [seq_len (n), ]
    }
    if (step_days >= 1L) {
        h$date <- as.Date (h$time)
        h <- dplyr::group_by (h, date) |>
            dplyr::filter (dplyr::row_number () == 1L)
        if (step_days > 1L) {
            index <- which (-diff (h$date) < step_days)
            if (length (index) > 0L) {
                h <- h [-(index), ]
            }
        }
    }

    return (h)
}
