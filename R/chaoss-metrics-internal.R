chaoss_internal_num_commits <- function (path) {
    checkmate::assert_character (path, len = 1L)
    checkmate::assert_directory (path)

    period <- get_githist_period ()

    h <- gert::git_log (repo = path, max = 1e6)
    dates <- as.Date (h$time)
    today_minus_period <- as.Date (Sys.time ()) - period
    index <- which (dates >= today_minus_period)

    return (length (index))
}
