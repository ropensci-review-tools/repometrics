#' From
#' \url{https://chaoss.community/kb/metrics-model-collaboration-development-index/},
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
#' @return A vector of three named numeric values, each measured over the
#' defined time period:
#' \enumerate{
#' \item n_opened = Number of change requests opened.
#' \item n_merged = Number of change requests merged.
#' \item prop_merged = Proportion of all change requests which were merged.
#' \item prop_code_from_prs = Proportion of all code committed by change
#' request.
#' }
#' @noRd
cm_data_change_req <- function (path, end_date = Sys.Date ()) {

    ret <- c (
        n_opened = 0, n_closed = 0, prop_merged = 0, prop_code_from_prs = 0
    )

    log <- git_log_in_period (path, end_date)
    if (nrow (log) == 0) {
        return (ret)
    }

    prs <- get_prs_in_period (path, end_date)

    if (nrow (prs) > 0L) {
        ret <- c (
            n_opened = nrow (prs),
            n_closed = length (which (prs$merged)),
            prop_merged = length (which (prs$merged)) / nrow (prs),
            prop_code_from_prs = sum (prs$num_commits) / nrow (log)
        )
    }

    return (ret)
}

cm_metric_change_req_n_opened <- function (path, end_date = Sys.Date ()) {
    dat <- cm_data_change_req (path, end_date)
    return (as.integer (dat [["n_opened"]]))
}

cm_metric_change_req_n_closed <- function (path, end_date = Sys.Date ()) {
    dat <- cm_data_change_req (path, end_date)
    return (as.integer (dat [["n_closed"]]))
}

cm_metric_change_req_prop_merged <- function (path, end_date = Sys.Date ()) {
    dat <- cm_data_change_req (path, end_date)
    dat [["prop_merged"]]
}

cm_metric_change_req_prop_code <- function (path, end_date = Sys.Date ()) {
    dat <- cm_data_change_req (path, end_date)
    dat [["prop_code_from_prs"]]
}

cm_metric_change_req_n_opened_url <- function () {
    "metric-change-requests"
}

cm_metric_change_req_n_closed_url <- function () {
    "metric-change-requests"
}

cm_metric_change_req_prop_merged_url <- function () {
    "metric-change-requests"
}

cm_metric_change_req_prop_code_url <- function () {
    "metric-change-requests"
}
