#' CHAOSS metric "Time to Close"
#'
#' \url{https://chaoss.community/kb/metric-time-to-close/}
#' "How much time passes between creating and closing an operation such as an
#' issue, change request, or support ticket?"
#' @noRd
cm_metric_time_to_close <- function (path, end_date = Sys.Date ()) {

    issues <- get_issues_in_period (path, end_date, closed_only = TRUE) |>
        dplyr::filter (state == "closed")

    times_to_close <- NA_integer_
    if (nrow (issues) > 0) {
        created_at <- as.Date (issues$created_at)
        closed_at <- as.Date (issues$closed_at)
        times_to_close <- difftime (closed_at, created_at, units = "days") |>
            as.integer ()
    }

    c (
        mean = mean (times_to_close),
        median = stats::median (times_to_close)
    )
}
