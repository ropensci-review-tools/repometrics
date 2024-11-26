#' Extract total CRAN downloads for nominated package over period defined by
#' `options("repometrics_period")`.
#'
#' @param path Local path to repository.
#' @param end_date The date up to which download counts are to be aggregated.
#' @param n_per_page Not used here, but needed so all functions can safely be
#' called with this parameter.
#' @return A single integer counting the number of downloads.
#' @noRd
cm_metric_cran_downloads <- function (path,
                                      end_date = Sys.Date (),
                                      n_per_page) {

    checkmate::assert_directory_exists (path)
    checkmate::assert_date (end_date)

    pkg_name <- pkg_name_from_path (path)

    period <- get_repometrics_period ()
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
