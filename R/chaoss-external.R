has_gh_ci_tests <- function (path) {

    or <- org_repo_from_path (path)

    ci_data <- github_repo_workflow_query (or [1], or [2])
    h <- gert::git_log (repo = path, max = 1e6)
    any (ci_data$sha %in% h$commit)
}

#' The "Ratio of Code Commits linked with Change Requests" CHAOSS metric. This
#' is defined as, "Percentage of new code commits linked with change requests
#' in the last 90 days."
#' \url{https://chaoss.community/kb/metrics-model-collaboration-development-index/}.
#' @noRd
prop_commits_in_change_req <- function (path, end_date = Sys.Date ()) {

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
