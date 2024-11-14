#' Extract total CRAN downloads for nominated package over period defined by
#' `options("repometrics_period")`.
#'
#' @param pkg_name Name of package. For packages not on CRAN, the 'cranlogs'
#' API returns download counts of 0.
#' @param end_date The date up to which download counts are to be aggregated.
#' @return A single integer counting the number of downloads.
#' @noRd
cran_downloads <- function (pkg_name, end_date = Sys.Date ()) {

    checkmate::assert_character (pkg_name, len = 1L)
    checkmate::assert_date (end_date)
    period <- get_repometrics_period ()
    start_date <- as.Date (end_date - period)
    interval <- paste (start_date, sep = ":", end_date)

    base_url <- "http://cranlogs.r-pkg.org/"
    daily_url <- paste0 (base_url, "downloads/total/")
    req_url <- paste0 (daily_url, interval, "/", pkg_name)

    req <- httr2::request (req_url)
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    body <- httr2::resp_body_json (resp)
    return (body [[1]]$downloads)
}

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
prop_commits_in_change_req <- function (path, end_date = Sys.Date ()) {

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
