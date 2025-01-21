#' CHAOSS metric for contributor count
#'
#' \url{https://chaoss.community/kb/metrics-model-community-activity/}
#' "The number of active code commit authors, pr authors, review participants,
#' issue authors, and issue comments participants over a certain period of
#' time."
#' @noRd
cm_metric_ctb_count <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding note:
    created_at <- NULL

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

#' CHAOSS metric for committer count
#'
#' \url{https://chaoss.community/kb/metric-committers/}.
#' "D0_count: Contributors who have given the project a star, or are watching
#'    or have forked the repository.
#'  D1_count: Contributors who have created issues, made comments on an issue,
#'    or performed a code review.
#'  D2_count: Contributors who have created a merge request and successfully
#'    merged code."
#' @noRd
cm_metric_committer_count <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding note:
    starred_at <- created <- org_repo <- created_at <- NULL

    checkmate::assert_date (end_date)

    start_date <- end_date - get_repometrics_period ()

    stargazers <- rm_data_repo_stargazers (path) |>
        dplyr::filter (starred_at >= start_date & starred_at <= end_date)
    forks <- rm_data_repo_forks (path) |>
        dplyr::filter (created >= start_date & created <= end_date) |>
        dplyr::mutate (login = gsub ("\\/.*$", "", org_repo))
    ctbs0 <- unique (c (forks$login, stargazers$login))

    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    issue_cmts <- rm_data_issue_comments_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    ctbs1 <- unique (c (issues$user_login, issue_cmts$user_login))

    prs <- rm_data_prs_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    ctbs2 <- unique (prs$user_login, prs$merged_by)

    c (
        watchers = length (ctbs0),
        issues = length (ctbs1),
        prs = length (ctbs2)
    )
}
