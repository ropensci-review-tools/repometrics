#' Extract total CRAN downloads for nominated package over period defined by
#' `options("githist_period")`.
#'
#' @param pkg_name Name of package. For packages not on CRAN, the 'cranlogs'
#' API returns download counts of 0.
#' @param end_date The date up to which download counts are to be aggregated.
#' @return A single integer counting the number of downloads.
#' @noRd
cran_downloads <- function (pkg_name, end_date = Sys.Date ()) {

    checkmate::assert_character (pkg_name, len = 1L)
    checkmate::assert_date (end_date)
    period <- get_githist_period ()
    start_date <- as.Date (end_date - period)
    interval <- paste (start_date, sep = ":", end_date)

    base_url <- "http://cranlogs.r-pkg.org/"
    daily_url <- paste0 (base_url, "downloads/total/")
    req_url <- paste0 (daily_url, interval, "/", pkg_name)

    req <- httr2::request (req_url)
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    body <- httr2::resp_body_json (resp)
    return (body [[1]]$downloads)
}
