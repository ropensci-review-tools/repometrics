#' Extract CHAOSS metric "Contributor Absence Factor", or "bus factor"
#'
#' \url{https://chaoss.community/kb/metric-contributor-absence-factor/}.
#'
#' This returns a vector of three values, quantifying the number of
#' contributors needed to exceed 50% of all contributions in terms of (commits,
#' files changed, lines changes). In terms of the CHAOSS`metric, higher values
#' for these numbers are better than lower values.
#'
#' @param path Local path to repository
#' @param end_date Date at which metric is to be calculated.
#' @noRd
rm_data_contrib_absence_internal <- function (path, end_date = Sys.Date ()) {

    checkmate::assert_date (end_date)

    start_date <- end_date - get_repometrics_period ()

    log <- gitlog_unique_contributors (path, start_date, end_date)

    gitlog_absence_factor (log)
}

rm_metric_contrib_absence_commits <- function (path, end_date = Sys.Date ()) {
    dat <- rm_data_contrib_absence_internal (path, end_date)
    dat [["ncommits"]]
}

rm_metric_contrib_absence_commits_url <- function () {
    "metric-contributor-absence-factor"
}

# These others are not used, as they are highly correlated with main measure
# anyway:
#
# rm_metric_contrib_absence_files <- function (path, end_date = Sys.Date ()) {
#     dat <- rm_data_contrib_absence_internal (path, end_date)
#     dat [["nfiles_changed"]]
# }
#
# rm_metric_contrib_absence_lines <- function (path, end_date = Sys.Date ()) {
#     dat <- rm_data_contrib_absence_internal (path, end_date)
#     dat [["lines_changed"]]
# }

gitlog_absence_factor <- function (log) {

    # Count number of unique contributors needed to exceed 50%:
    absence_factor <- function (log, what = "ncommits") {
        res <- dplyr::arrange (log, dplyr::desc (get (what))) |>
            dplyr::mutate (prop = cumsum (get (what) / sum (get (what))))
        length (which (res$prop < 0.5)) + 1L
    }

    numeric_cols <- vapply (
        as.list (log),
        is.numeric,
        logical (1L),
        USE.NAMES = TRUE
    )
    numeric_cols <- names (numeric_cols) [which (numeric_cols)]
    # These are ["ncommits", "nfiles_changed", "lines_changed"]
    vapply (
        numeric_cols,
        function (i) absence_factor (log, what = i),
        integer (1L),
        USE.NAMES = TRUE
    )
}
