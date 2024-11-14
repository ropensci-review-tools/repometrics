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
chaoss_internal_change_req <- function (path, end_date = Sys.Date ()) {

    log <- git_log_in_period (path, end_date, get_repometrics_period ())
    res <- NA_integer_
    if (nrow (log) > 0L) {
        res <- length (which (log$merge)) / nrow (log)
    }
    return (res)
}
