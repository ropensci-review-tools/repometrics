#' CHAOSS metric for "burstiness"
#'
#' This is calculated over the full git log, then reduced to period at the end,
#' to enable application of rolling windows across full range of commit dates.
#'
#' @param band_len The length in days of the rolling window filters.
#' @param band_width The number of standard deviations used to identify
#' outliers in burstiness.
#'
#' \url{https://chaoss.community/kb/metric-burstiness/}.
#' @noRd
cm_metric_burstiness <- function (path, end_date, band_len = 31L, band_width = 2) {

    # Suppress no visible binding notes:
    timestamp <- ncommits <- mn <- sd <- upper <- lower <- outlier <- NULL

    requireNamespace ("zoo", quietly = TRUE)

    start_date <- end_date - get_repometrics_period ()

    log <- rm_data_gitlog (path) |>
        dplyr::mutate (date = as.Date (timestamp)) |>
        dplyr::group_by (date) |>
        dplyr::summarise (ncommits = dplyr::n ())

    dates <- seq (min (log$date), max (log$date), by = "days")

    # Expand log to full sequence of dates, and apply rolling windows. Where no
    # commits occur in entire period, `sd` is also zero, so these are converted
    # to NA to enable those entire periods to be removed in final filter line.
    log_full <- tidyr::complete (log, date = dates) |>
        tidyr::fill () |>
        dplyr::arrange (by = date) |>
        tidyr::replace_na (list (ncommits = 0L)) |>
        dplyr::mutate (
            mn = zoo::rollapplyr (ncommits, 31, mean, fill = NA),
            sd = zoo::rollapplyr (ncommits, 31, stats::sd, fill = NA)
        ) |>
        dplyr::filter (!is.na (mn)) |>
        dplyr::mutate (sd = dplyr::na_if (sd, 0)) |>
        dplyr::mutate (
            upper = mn + band_width * sd,
            lower = mn - band_width * sd
        ) |>
        dplyr::mutate (outlier = (ncommits >= upper | ncommits <= lower)) |>
        dplyr::filter (!is.na (outlier))

    log_in_period <- dplyr::filter (
        log_full,
        date >= start_date & date <= end_date
    )

    ret <- NA_real_
    if (nrow (log_in_period) > 0L) {
        ret <- length (which (log_in_period$outlier)) / nrow (log_in_period)
    }

    return (ret)
}
