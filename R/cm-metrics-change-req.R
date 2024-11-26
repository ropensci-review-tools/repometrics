#' From \url{https://chaoss.community/kb/metrics-model-collaboration-development-index/},
#' the "Code Commit linked with Change Request" (which is not hyperlinked to
#' anywhere else), is defined as "Percentage of new code commits linked with
#' change request in last 90 days." The interpretation of "Change Request" is
#' defined at \url{https://chaoss.community/kb/metric-change-requests/} as
#' "proposals for modifications to a project's source code that have been
#' submitted for review ... Examples include GitHub 'Pull Request', GitLab
#' 'Merge Requests' and Gerrit 'code reviews'." These are here analysed as
#' merge commits in the git log, even though these may not have been actual PRs
#' on GitHub or similar, and may not have been reviewed.
#'
#' @return NA if no commits made in given period, otherwise the proportion of
#' commits which came from merged branches.
#' @noRd
cm_metric_change_req <- function (path, end_date = Sys.Date ()) {

    log <- git_log_in_period (path, end_date, get_repometrics_period ())
    prs <- cm_data_prs_from_gh_api (path)
    prs <- prs [which (prs$merged), ]
    closed_dates <- as.Date (prs$closed_at)
    start_date <- end_date - get_repometrics_period ()
    index <- which (closed_dates >= start_date & closed_dates <= end_date)
    prs <- prs [index, ]

    num_commits <- vapply (prs$commit_oids, function (i) {
        length (strsplit (i, ",") [[1]])
    }, integer (1L), USE.NAMES = FALSE)

    res <- sum (num_commits) / nrow (log)

    return (res)
}

#' The "Ratio of Code Commits linked with Change Requests" CHAOSS metric. This
#' is defined as, "Percentage of new code commits linked with change requests
#' in the last 90 days."
#' \url{https://chaoss.community/kb/metrics-model-collaboration-development-index/}.
#' @noRd
cm_metric_prop_commits_in_change_req <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding notes:
    number <- action <- NULL

    or <- org_repo_from_path (path)

    gh_dat <- github_issues_prs_query (org = or [1], repo = or [2])

    # Reduce to PR open-close events:
    gh_prs <- dplyr::filter (gh_dat, !is.na (number)) |>
        dplyr::group_by (number) |>
        dplyr::filter (action == "closed")

    start_date <- as.Date (end_date - get_repometrics_period ())
    index <- which (as.Date (gh_prs$merged_at) >= start_date)

    num_commits_from_prs <- sum (gh_prs$commits [index])

    log <- git_log_in_period (path, end_date, get_repometrics_period ())

    ifelse (nrow (log) == 0, 0, num_commits_from_prs / nrow (log))
}
