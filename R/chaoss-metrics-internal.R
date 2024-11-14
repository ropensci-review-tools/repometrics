chaoss_internal_num_commits <- function (path, end_date = Sys.Date ()) {

    log <- git_log_in_period (path, end_date, get_repometrics_period ())

    return (nrow (log))
}

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

git_log_in_period_internal <- function (path, end_date = Sys.Date (), period = 90) {
    checkmate::assert_character (path, len = 1L)
    checkmate::assert_directory (path)
    checkmate::assert_date (end_date)

    h <- gert::git_log (repo = path, max = 1e6)
    if (nrow (h) == 0) {
        return (h)
    }
    dates <- as.Date (h$time)
    today_minus_period <- as.Date (end_date - period)
    index <- which (dates >= today_minus_period)
    h <- h [index, ]

    if (dates [1] > end_date) {
        h <- h [which (dates <= end_date), ]
    }

    return (h)
}
git_log_in_period <- memoise::memoise (git_log_in_period_internal)

#' Return names of CI services from corresponding configuration files contained
#' in local repo.
#' @noRd
repo_has_ci_files <- function (path) {

    flist <- fs::dir_ls (path, type = "file", all = TRUE, recurse = TRUE)
    ptns <- c (
        "\\.appveyor\\.yml",
        "\\.github\\/workflows",
        "\\.gitlab\\-ci\\.yml",
        "\\.circleci\\/config\\.yml",
        "codefresh\\.yml",
        "drone\\.yml",
        "\\.travis\\.yml"
    )
    has_it <- vapply (ptns, function (i) any (grepl (i, flist)), logical (1L))
    nms <- gsub ("\\\\.|\\\\.y.*$|\\\\/.*$|\\\\-.*$", "", ptns)

    return (nms [which (has_it)])
}
