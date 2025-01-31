#' Time to fist response, taken from both PRs and issues.
#'
#' \url{https://chaoss.community/kb/metric-time-to-first-response/}.
#' @noRd
cm_metric_response_time <- function (path, end_date = Sys.Date ()) {

    # In R/cm-metrics-pr-reviews:
    dur_prs <- cm_metric_pr_response_durations (path, end_date = end_date)
    # In R/cm-metrics-issue-response:
    dur_issues <- cm_metric_issue_response_time (path, end_date = end_date)

    durations <- as.integer (c (dur_prs, dur_issues))

    return (mn_med_sum (durations))
}
