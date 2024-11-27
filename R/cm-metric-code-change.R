cm_metric_code_change_lines <- function (path,
                                         end_date = Sys.Date (),
                                         exclude_whitespace = TRUE) {

    log <- git_log_in_period (path, end_date)

    changes <- sum (log$lines_added) + sum (log$lines_removed)
    ws <- sum (log$whitespace_added) + sum (log$whitespace_removed)
    if (!exclude_whitespace) {
        ws <- 0L
    }

    return (changes - ws)
}
