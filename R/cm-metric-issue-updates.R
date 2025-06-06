#' CHAOSS metric for updated issues
#'
#' \url{https://chaoss.community/kb/metrics-model-community-activity/}
#' "The number of issues updated over a certain period of time."
#' @noRd
cm_data_issue_updates <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding note:
    created_at <- NULL

    checkmate::assert_date (end_date)

    start_date <- end_date - get_repometrics_period ()

    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    issue_cmts <- rm_data_issue_comments_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)

    issue_nums <- sort (unique (c (issues$number, issue_cmts$issue_number)))
    length (issue_nums)
}

cm_metric_issue_updates <- function (path, end_date = Sys.Date ()) {
    cm_data_issue_updates (path, end_date)
}

cm_metric_issue_updates_url <- function () {
    "metric-issues-active"
}

cm_data_issue_cmt_count <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding note:
    created_at <- issue_number <- NULL

    checkmate::assert_date (end_date)

    start_date <- end_date - get_repometrics_period ()

    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    issue_cmts <- rm_data_issue_comments_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date) |>
        dplyr::filter (issue_number %in% issues$number) |>
        dplyr::group_by (issue_number) |>
        dplyr::summarise (ncomments = dplyr::n ())

    cmts <- rep (0, nrow (issues))
    index <- match (issue_cmts$issue_number, issues$number)
    cmts [index] <- issue_cmts$ncomments
    if (length (cmts) == 0) {
        cmts <- 0
    }

    return (mn_med_sum (cmts))
}

cm_metric_issue_cmt_count <- function (path, end_date = Sys.Date ()) {
    cm_data_issue_cmt_count (path, end_date) [["mean"]]
}

cm_metric_issue_cmt_count_url <- function () {
    "metric-issues-active"
}

cm_data_issues_closed <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding note:
    closed_at <- NULL

    checkmate::assert_date (end_date)

    start_date <- end_date - get_repometrics_period ()

    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::filter (closed_at >= start_date & closed_at <= end_date)

    nrow (issues)
}

cm_metric_issues_closed <- function (path, end_date = Sys.Date ()) {
    cm_data_issues_closed (path, end_date)
}

cm_metric_issues_closed_url <- function () {
    "metric-issues-closed"
}
