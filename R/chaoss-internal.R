chaoss_internal_num_commits <- function (path, end_date = Sys.Date ()) {

    log <- git_log_in_period (path, end_date, get_repometrics_period ())

    return (nrow (log))
}

git_log_in_period_internal <- function (path, end_date = Sys.Date (), period = 90) {
    checkmate::assert_character (path, len = 1L)
    checkmate::assert_directory (path)
    checkmate::assert_date (end_date)

    h <- gert::git_log (repo = path, max = 1e6)
    if (nrow (h) == 0) {
        return (h)
    }
    dates <- as.Date (h$time)
    today_minus_period <- as.Date (end_date - period)
    index <- which (dates >= today_minus_period)
    h <- h [index, ]

    if (dates [1] > end_date) {
        h <- h [which (dates <= end_date), ]
    }

    return (h)
}
git_log_in_period <- memoise::memoise (git_log_in_period_internal)
