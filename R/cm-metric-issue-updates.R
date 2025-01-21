#' CHAOSS metric for updated issues
#'
#' \url{https://chaoss.community/kb/metrics-model-community-activity/}
#' "The number of issues updated over a certain period of time."
#' @noRd
cm_metric_issue_updates <- function (path, end_date = Sys.Date (), nyears = 1) {

    checkmate::assert_date (end_date)
    checkmate::assert_numeric (nyears, lower = 0L)

    start_date <- as.Date (end_date - round (nyears * 365.25))

    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    issue_cmts <- rm_data_issue_comments_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)

    issue_nums <- sort (unique (c (issues$number, issue_cmts$issue_number)))
    length (issue_nums)
}
