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

    pkgstats <- repo_pkgstats_history (
        path,
        step_days = step_days,
        num_cores = num_cores
    )
    cm <- cm_data (path)

    list (pkgstats = pkgstats, cm = cm)
}
