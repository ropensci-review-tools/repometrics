cm_metric_num_commits <- function (path, end_date = Sys.Date ()) {

    log <- git_log_in_period (path, end_date, get_repometrics_period ())

    return (nrow (log))
}
