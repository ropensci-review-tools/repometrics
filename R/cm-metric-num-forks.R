cm_metric_num_forks <- function (path, end_date = Sys.Date ()) {

    forks <- rm_data_repo_forks (path)

    start_date <- end_date - get_repometrics_period ()
    index <- which (forks$created >= start_date & forks$created <= end_date)

    return (c (num_in_period = length (index), num_total = nrow (forks)))
}
