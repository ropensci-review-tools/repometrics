#' Extract CHAOSS metric "Contributor Absence Factor"
#'
#' \url{https://chaoss.community/kb/metric-contributor-absence-factor/}.
#'
#' @param path Local path to repository
#' @param end_date Date at which metric is to be calculated.
#' @param nyears Number of preceding years over which metric is to be calculated.
#' @noRd
cm_metric_contrib_absence <- function (path, end_date = Sys.Date (), nyears = 1) {

    checkmate::assert_date (end_date)
    checkmate::assert_numeric (nyears, lower = 0L)

    start_date <- as.Date (end_date - round (nyears * 365.25))

    log <- gitlog_unique_contributors (path, start_date, end_date)

    gitlog_absence_factor (log)
}

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
    vapply (
        numeric_cols,
        function (i) absence_factor (log, what = i),
        integer (1L),
        USE.NAMES = TRUE
    )
}
