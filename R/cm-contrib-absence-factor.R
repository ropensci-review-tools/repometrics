#' Extract CHAOSS metric "Contributor Absence Factor"
#'
#' \url{https://chaoss.community/kb/metric-contributor-absence-factor/}.
#'
#' @param path Local path to repository
#' @param pkg_date Date at which metric is to be calculated.
#' @param nyears Number of preceding years over which metric is to be calculated.
#' @noRd
cm_metric_contrib_absence <- function (path, pkg_date = Sys.Date (), nyears = 1) {

    checkmate::assert_date (pkg_date)
    checkmate::assert_numeric (nyears, lower = 0L)

    start_date <- as.Date (pkg_date - round (nyears * 365.25))

    log <- gitlog_unique_contributors (path, start_date, pkg_date)

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

gitlog_unique_contributors <- function (path, start_date, end_date) {

    log <- rm_data_gitlog (path) |>
        dplyr::mutate (date = as.Date (timestamp)) |>
        dplyr::filter (date >= start_date & date <= end_date) |>
        dplyr::filter (aut_name != "GitHub") |>
        dplyr::group_by (aut_name, aut_email) |>
        dplyr::summarise (
            ncommits = dplyr::n (),
            nfiles_changed = sum (nfiles_changed),
            lines_added = sum (lines_added),
            lines_removed = sum (lines_removed)
        )

    log$index <- index_partial_duplicates (log) # in utils-author-matches.R
    # Then use that index to group all unique contributors:
    dplyr::group_by (log, index) |>
        dplyr::summarise (
            aut_name = dplyr::first (aut_name),
            aut_email = dplyr::first (aut_email),
            ncommits = sum (ncommits),
            nfiles_changed = sum (nfiles_changed),
            lines_changed = sum (lines_added + lines_removed)
        ) |>
        dplyr::select (-index)

}
