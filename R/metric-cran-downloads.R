#' Extract total CRAN downloads for nominated package over period defined by
#' `options("repometrics_period")`.
#'
#' @param path Local path to repository.
#' @param end_date The date up to which download counts are to be aggregated.
#' @return A single integer counting the number of downloads.
#' @noRd
rm_data_cran_downloads_internal <- function (path, end_date = Sys.Date ()) {

    checkmate::assert_directory_exists (path)
    checkmate::assert_date (end_date)

    period <- get_repometrics_period ()
    start_date <- as.Date (end_date - period)
    pkg_name <- pkg_name_from_path (path)

    cran_dl <- cran_downloads_daily (pkg_name, end_date) |>
        dplyr::filter (date >= start_date & date <= end_date)

    return (sum (cran_dl$downloads))
}

rm_metric_cran_downloads <- function (path, end_date = Sys.Date ()) {
    rm_data_cran_downloads_internal (path, end_date)
}

rm_metric_cran_downloads_url <- function () {
    "metric-number-of-downloads"
}

#' Download the full daily log over `nyears`, and use memoised return value to
#' filter to desired period for subsequent calls.
#' @noRd
cran_downloads_internal <- function (pkg_name = NULL,
                                     end_date = Sys.Date (),
                                     nyears = 10) {

    checkmate::assert_character (pkg_name, len = 1L)
    checkmate::assert_integerish (nyears, min = 1L)

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"

    period <- 365.25 * nyears
    if (is_test_env) {
        period <- 2
    }

    start_date <- as.Date (end_date - period)
    interval <- paste (start_date, sep = ":", end_date)

    base_url <- "http://cranlogs.r-pkg.org/"
    daily_url <- paste0 (base_url, "downloads/daily/")
    req_url <- paste0 (daily_url, interval, "/", pkg_name)

    req <- httr2::request (req_url)
    resp <- tryCatch (
        httr2::req_perform (req),
        error = function (e) NULL
    )
    if (is.null (resp)) {
        return (data.frame (date = as.Date (character (0L)), downloads = integer (0L)))
    }
    httr2::resp_check_status (resp)

    body <- httr2::resp_body_json (resp)

    downloads <- body [[1]]$downloads
    dates <- vapply (downloads, function (i) i$day, character (1L))
    downloads <- vapply (downloads, function (i) i$downloads, integer (1L))

    res <- data.frame (date = dates, downloads = downloads)
    res$date <- as.Date (res$date)

    return (res)
}
cran_downloads_daily <- memoise::memoise (cran_downloads_internal)
