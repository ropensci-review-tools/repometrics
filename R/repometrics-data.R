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
repometrics_data <- function (path, step_days = 1L, num_cores = -1L) {

    cli::cli_alert_info ("Extracting package statistics ...")
    pkgstats <- repo_pkgstats_history (
        path,
        step_days = step_days,
        num_cores = num_cores
    )
    cli::cli_alert_success ("Done!")

    cli::cli_alert_info ("Extracting GitHub data ...")
    rm <- rm_data (path)
    rm$contributors <- get_all_contribs (rm$contribs_from_log, rm$contribs_from_gh_api)
    cli::cli_alert_success ("Done!")

    list (pkgstats = pkgstats, rm = rm)
}
