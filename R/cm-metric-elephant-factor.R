#' The CHAOSS Metric "Elephant Factor", which is the number of distint
#' organizations needed to reach 50 of contributions. This is akin to the
#' "cm_metric_contrib_absence()", but for organizations instead of individuals.
#'
#' \url{https://chaoss.community/kb/metric-elephant-factor/}.
#'
#' @noRd
cm_metric_elephant_factor <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding note:
    name <- company <- ncommits <- nfiles_changed <- lines_changed <- NULL

    checkmate::assert_date (end_date)

    start_date <- end_date - get_repometrics_period ()

    log <- gitlog_unique_contributors (path, start_date, end_date)

    # Then reduce log down to unique organizations instead of unique
    # contributors:
    gh_contribs <- rm_data_contribs_from_gh_api (path) |>
        dplyr::select (name, company) |>
        dplyr::rename (aut_name = name)

    log <- dplyr::left_join (log, gh_contribs, by = "aut_name")
    log_na <- dplyr::filter (log, is.na (company))
    log <- dplyr::filter (log, !is.na (company)) |>
        dplyr::group_by (company) |>
        dplyr::summarise (
            ncommits = sum (ncommits),
            nfiles_changed = sum (nfiles_changed),
            lines_changed = sum (lines_changed)
        ) |>
        dplyr::bind_rows (log_na)

    gitlog_absence_factor (log)
}
