#' CHAOSS metric for release frequency, actually measured here as interval in
#' days between releases.
#'
#' @noRd
cm_metric_release_freq <- function (path, end_date = Sys.Date ()) {

    # suppress no visible binding notes:
    published_at <- NULL

    start_date <- end_date - get_repometrics_period ()

    releases <- rm_data_releases_from_gh_api (path) |>
        dplyr::mutate (published_at = as.Date (published_at)) |>
        dplyr::filter (published_at <= end_date)

    res <- NA_real_
    if (nrow (releases) > 1) {
        res <- difftime (end_date, max (releases$published_at))
        releases <- dplyr::filter (releases, published_at >= start_date)
        res <- c (res, diff (rev (releases$published_at), units = "days"))
    }

    c (
        mean = as.integer (mean (res)),
        median = as.integer (stats::median (res))
    )
}

cm_metric_recent_releases <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding note:
    published_at <- NULL

    start_date <- end_date - get_repometrics_period ()

    releases <- rm_data_releases_from_gh_api (path) |>
        dplyr::mutate (published_at = as.Date (published_at)) |>
        dplyr::filter (published_at >= start_date & published_at <= end_date)

    nrow (releases)
}
