cm_metric_num_contributors <- function (path, end_date = Sys.Date ()) {

    log <- git_log_in_period (path, end_date)

    # Remove any duplicates of either names or emails, but excluding
    # non-entries:
    rm_dup_rows <- function (x) {
        x <- gsub ("\\s+", "", x)
        index <- seq_along (x)
        index_out <- which (duplicated (x) & nzchar (x))
        if (length (index_out) > 0) {
            index <- index [-(index_out)]
        }
        return (index)
    }
    index1 <- rm_dup_rows (log$aut_name)
    index2 <- rm_dup_rows (log$aut_email)

    # Then extract only instances where neither handles nor emails are
    # duplicated:
    index_table <- table (c (index1, index2))
    index <- as.integer (names (index_table) [which (index_table == 2L)])

    return (length (index))
}
