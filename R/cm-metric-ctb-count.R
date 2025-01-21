#' CHAOSS metric for contributor count
#'
#' \url{https://chaoss.community/kb/metrics-model-community-activity/}
#' "The number of active code commit authors, pr authors, review participants,
#' issue authors, and issue comments participants over a certain period of
#' time."
#' @noRd
cm_metric_ctb_count <- function (path, end_date = Sys.Date ()) {

    checkmate::assert_date (end_date)

    start_date <- end_date - get_repometrics_period ()

    log <- gitlog_unique_contributors (path, start_date, end_date)
    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    issue_cmts <- rm_data_issue_comments_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    prs <- rm_data_prs_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)

    c (
        code = nrow (log),
        pr_authors = nrow (prs),
        issue_authors = nrow (issues),
        issue_cmt_authors = nrow (issue_cmts)
    )
}
