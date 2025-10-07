#' Time to fist response, taken from both PRs and issues.
#'
#' \url{https://chaoss.community/kb/metric-time-to-first-response/}.
#' @noRd
rm_data_response_time_internal <- function (path, end_date = Sys.Date ()) {

    # In R/cm-metrics-pr-reviews:
    dur_prs <- rm_metric_pr_response_durations (path, end_date = end_date)
    # In R/cm-metrics-issue-response:
    dur_issues <- rm_metric_issue_response_time (path, end_date = end_date)

    durations <- as.integer (c (dur_prs, dur_issues))

    return (mn_med_sum (durations))
}

rm_metric_response_time <- function (path, end_date = Sys.Date ()) {
    rm_data_response_time_internal (path, end_date)
}

rm_metric_response_time_url <- function () {
    "metric-time-to-first-response"
}
