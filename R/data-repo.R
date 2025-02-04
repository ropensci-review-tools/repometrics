#' Collate 'repometrics' data for a local R package
#'
#' @inheritParams repo_pkgstats_history
#' @return A list with two main items:
#' \enumerate{
#' \item "pkgstats" Containing summary data from apply `pkgstats` routines
#' across the git history of the repository.
#' \item "cm" Containing data used to derive "CHAOSS metrics", primarily from
#' GitHub data.
#' }
#'
#' @export
repometrics_data_repo <- function (path, step_days = 1L, num_cores = -1L) {

    if (is_verbose ()) {
        cli::cli_alert_info ("Extracting package statistics ...")
    }
    pkgstats <- repo_pkgstats_history (
        path,
        step_days = step_days,
        num_cores = num_cores
    )
    if (is_verbose ()) {
        cli::cli_alert_success ("Done!")
    }

    if (is_verbose ()) {
        cli::cli_alert_info ("Extracting GitHub data ...")
    }
    rm <- rm_data_repo (path)
    rm$contributors <-
        get_all_contribs (rm$contribs_from_log, rm$contribs_from_gh_api)
    if (is_verbose ()) {
        cli::cli_alert_success ("Done!")
    }

    list (pkgstats = pkgstats, rm = rm)
}

#' Calculate all repository data used in CHAOSS metrics
#' \url{https://chaoss.community/kb-metrics-and-metrics-models/}.
#'
#' @param path Path to local source repository.
#' @return A list of the following `data.frame` objects:
#' \enumerate{
#' \item `contribs_from_gh_api` with details of all code contributors from
#' GitHub
#' \item `contribs_from_log` with details of all code contributors from the
#' local git log
#' \item `dependencies` A simple `data.frame` of all package dependencies
#' \item `gh_repo_workflow` with details of all workflows run on GitHub,
#' including status of most recent runs
#' \item `gitlog` with one row for each git commit, and associated statistics
#' \item `issue_comments_from_gh_api` with details of all comments from all
#' repository issues on GitHub
#' \item `issues_from_gh_api` with details of all issues on GitHub
#' \item `libyears` The CHAOSS metric described at
#' \url{https://chaoss.community/kb/metric-libyears/}, measuring the relative
#' age of a project's dependencies, with lower values indicating more
#' up-to-date projects. This is the only item which is not a `data.frame`,
#' rather a named numerical vector of mean and median "libyears"
#' \item `prs_from_gh_api` with details of all pull requests on GitHub
#' \item `releases_from_gh_api` with details of all repository releases on
#' GitHub
#' \item `repo_from_gh_api` A `data.frame` of a single line, with several key
#' attributes of the repository on GitHub.
#' }
#' @noRd
rm_data_repo <- function (path) {

    checkmate::assert_directory_exists (path)

    data_fns <- get_rm_data_fns ()

    if (all_rm_data_fns_memoised (data_fns, path) || !is_verbose ()) {
        res <- lapply (data_fns, function (i) {
            do.call (i, list (path = path))
        })
    } else {
        res <- pbapply::pblapply (data_fns, function (i) {
            do.call (i, list (path = path))
        })
    }
    names (res) <- gsub ("^rm\\_data\\_", "", data_fns)

    return (res)
}

get_rm_data_fns <- function () {

    pkg_fns <- ls (envir = asNamespace ("repometrics"))
    data_fns <- grep ("^rm\\_data\\_", pkg_fns, value = TRUE)
    data_fns <- data_fns [which (!grepl ("\\_internal$", data_fns))]
    data_fns <- data_fns [which (!data_fns == "rm_data_repo")]

    return (data_fns)
}

all_rm_data_fns_memoised <- function (data_fns, path) {
    is_memoised <- vapply (data_fns, function (i) {
        tryCatch (
            memoise::has_cache (get (i)) (path),
            error = function (e) FALSE
        )
    }, logical (1L))

    length (which (is_memoised)) > (length (data_fns) / 2)
}
