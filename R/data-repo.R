#' Collate 'repometrics' data for a local R package.
#'
#' This function collates all data for a local R package or repository needed
#' to create a dashboard with the \link{repometrics_dashboard} function. It
#' combines data from both the \link{repometrics_data_repo} and
#' \link{repometrics_data_user} functions.
#'
#' @param path Path to local repository containing an R package.
#' @param step_days Analyse package at intervals of this number of days. The
#' last commit for each day is chosen. For example, `step_days = 7L` will
#' return weekly statistics. Values of zero or less will analyse all commits,
#' including potentially multiple daily commits.
#' @param num_cores Number of cores to use in multi-core processing. Has no
#' effect on Windows operating systems, on which calculations are always
#' single-core only. Negative values are subtracted from number of available
#' cores, determined as `parallel::detectCores()`, so default of `num_cores =
#' -1L` uses `detectCores() - 1L`. Positive values use precisely that number,
#' restricted to maximum available cores, and a value of zero will use all
#' available cores.
#' @param ended_at Parameter used in some aspects of resultant data to limit
#' the end date of data collection. Defaults to `Sys.Date ()`.
#' @param nyears Parameter <= 1 determining fraction of a year over which data
#' up until `end_date` are collected.
#'
#' @return A list of three forms of data:
#' \enumerate{
#' \item "pkgstats" containing statistics on the historical development of
#' package code, derived from the \pkg{pkgstats} package;
#' \item "rm" containing data from GitHub on the repository, including data on
#' contributors, issues, pull requests, and people watching and starring the
#' repository.
#' \item "contributors" as a named list of data on every individual contributor
#' to the repository, whether by code contributions or GitHub issues or
#' discussions.
#' }
#'
#' @family data
#' @export
repometrics_data <- function (path, step_days = 1L, num_cores = -1L,
                              ended_at = Sys.Date (), nyears = 1) {

    data <- repometrics_data_repo (
        path = path, step_days = step_days, num_cores = num_cores
    )

    ctbs <- data$rm$contribs_from_gh_api$login
    data_ctbs <- lapply (ctbs, function (ctb) {
        tryCatch (
            repometrics_data_user (
                login = ctb,
                ended_at = ended_at,
                nyears = nyears
            ),
            error = function (e) NULL
        )
    })
    lens <- vapply (data_ctbs, length, integer (1L))
    gh_errors <- ctbs [which (lens == 0L)]
    if (length (gh_errors) > 0L) {
        cli::cli_alert_warning (paste0 (
            "Data for the following GitHub logins ",
            "were unable to be obtained:\n  [{gh_errors}].\n",
            "Try re-running function again to ensure all ",
            "data are collected."
        ))
    }
    names (data_ctbs) <- ctbs

    data$contributors <- data_ctbs

    return (data)
}

#' Collate 'repometrics' data for a local R package.
#'
#' This forms part of the data collated by the main \link{repometrics_data}
#' function, along with detailed data on individual contributors extracted by
#' the \link{repometrics_data_user} function.
#'
#' @inheritParams repometrics_data
#'
#' @return A list with two main items:
#' \enumerate{
#' \item "pkgstats" Containing summary data from apply `pkgstats` routines
#' across the git history of the repository.
#' \item "cm" Containing data used to derive "CHAOSS metrics", primarily from
#' GitHub data.
#' }
#'
#' @family data
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
