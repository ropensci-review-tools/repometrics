rm_data_num_commits_internal <- function (path, end_date = Sys.Date ()) {

    log <- git_log_in_period (path, end_date)

    return (nrow (log))
}

rm_metric_num_commits <- function (path, end_date = Sys.Date ()) {
    rm_data_num_commits_internal (path, end_date)
}

rm_metric_num_commits_url <- function () {
    "metric-code-changes-commits"
}

#' CHAOSS Metric for commit frequency, which is actually assessed here as
#' average number of commits per week, so a direct count rather than frequency.
#' @noRd
rm_data_commit_freq_internal <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding notes:
    timestamp <- NULL

    checkmate::assert_date (end_date)

    start_date <- end_date - get_repometrics_period ()

    log <- git_log_in_period (path, end_date) |>
        dplyr::mutate (date = as.Date (timestamp)) |>
        dplyr::mutate (
            week = paste0 (strftime (date, "%y"), ".", strftime (date, "%W"))
        ) |>
        dplyr::group_by (week) |>
        dplyr::summarise (ncommits = dplyr::n ())

    if (nrow (log) == 0L) {
        return (mn_med_sum (0))
    }

    week <- round (as.numeric (log$week), digits = 2)
    index <- seq (min (week), max (week), by = 0.01)
    index_weeks <- round (index - floor (index), digits = 2)
    # This is 53, because end of year is start of 53rd week:
    index_in_53 <- which (index_weeks >= 0 & index_weeks <= 0.53)
    index <- sprintf ("%.2f", index [index_in_53])

    commits <- rep (0, length (index))
    index_log <- match (log$week, index)
    commits [index_log] <- log$ncommits

    return (mn_med_sum (commits))
}

rm_metric_commit_count <- function (path, end_date = Sys.Date ()) {
    dat <- rm_data_commit_freq_internal (path, end_date)
    return (dat [["mean"]])
}

rm_metric_commit_count_url <- function () {
    "metric-code-changes-commits"
}
