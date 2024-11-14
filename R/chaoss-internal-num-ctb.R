chaoss_internal_num_contributors <- function (path, end_date = Sys.Date ()) {

    log <- git_log_in_period (path, end_date, get_repometrics_period ())

    auths_un <- unique (log$author)

    # separate handles from emails:
    emails <- regmatches (auths_un, gregexpr ("<.*>", auths_un))
    emails <- vapply (emails, function (i) {
        ifelse (length (i) == 0L, "", gsub ("<|>", "", i))
    }, character (1L))
    handles <- gsub ("<.*$", "", auths_un)

    # Remove any duplicates of either, but excluding non-entries:
    rm_dup_rows <- function (x) {
        x <- gsub ("\\s+", "", x)
        index <- seq_along (x)
        index_out <- which (duplicated (x) & nzchar (x))
        if (length (index_out) > 0) {
            index <- index [-(index_out)]
        }
        return (index)
    }
    index1 <- rm_dup_rows (handles)
    index2 <- rm_dup_rows (emails)

    # Then extract only instances where neither handles nor emails are
    # duplicated:
    index_table <- table (c (index1, index2))
    index <- as.integer (names (index_table) [which (index_table == 2L)])

    auths_un <- auths_un [index]

    return (length (auths_un))
}
