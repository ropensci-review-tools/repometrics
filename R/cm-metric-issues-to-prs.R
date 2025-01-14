cm_metric_issues_to_prs <- function (path, end_date = Sys.Date ()) {

    prs <- get_prs_in_period (path, end_date) # in cm-metrics-change-req.R
    issues <- get_issues_in_period (path, end_date)

    ret <- 1
    if (nrow (prs) > 0) {
        ret <- nrow (issues) / nrow (prs)
    }

    return (ret)
}

get_issues_in_period <- function (path,
                                  end_date = Sys.Date (),
                                  closed_only = FALSE) {

    issues <- rm_data_issues_from_gh_api (path)
    closed_dates <- as.Date (issues$closed_at)
    start_date <- end_date - get_repometrics_period ()

    if (closed_only) {
        index <- which (closed_dates >= start_date & closed_dates <= end_date)
    } else {
        created_dates <- as.Date (issues$created_at)
        index <- which (created_dates >= start_date & closed_dates <= end_date)
    }

    return (issues [index, ])
}
