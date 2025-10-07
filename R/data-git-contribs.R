#' Get contributors from the git log
#'
#' @param path Local path to repository
#' @noRd
rm_data_contribs_from_log <- function (path) {

    # Suppress no visible binding notes:
    handle <- NULL

    log <- rm_data_gitlog (path)

    gh_handle <- unique (log$aut_name)
    gh_email <- log$aut_email [match (gh_handle, log$aut_name)]

    # Remove any duplicates of either, but excluding non-entries.
    # Also need to catch 'gsub' errors for non-UTF8 strings:
    rm_dup_rows <- function (x) {
        x <- tryCatch (gsub ("\\s+", "", x), error = function (e) x)
        index <- seq_along (x)
        index_out <- which (duplicated (x) & nzchar (x))
        if (length (index_out) > 0) {
            index <- index [-(index_out)]
        }
        return (index)
    }
    index1 <- rm_dup_rows (gh_handle)
    index2 <- rm_dup_rows (gh_email)

    # Then extract only instances where neither handles nor emails are
    # duplicated:
    index_table <- table (c (index1, index2))
    index <- as.integer (names (index_table) [which (index_table == 2L)])

    rm_handles <- c ("GitHub", "GitHub Action")
    data.frame (
        handle = gh_handle,
        email = gh_email
    ) [index, ] |> dplyr::filter (!handle %in% rm_handles)
}

gitlog_unique_contributors <- function (path, start_date, end_date) {

    # Suppress no visible binding note:
    timestamp <- aut_name <- aut_email <- nfiles_changed <- lines_added <-
        lines_removed <- index <- ncommits <- NULL

    log <- rm_data_gitlog (path) |>
        dplyr::mutate (date = as.Date (timestamp)) |>
        dplyr::filter (date >= start_date & date <= end_date) |>
        dplyr::filter (aut_name != "GitHub") |>
        dplyr::group_by (aut_name, aut_email) |>
        dplyr::summarise (
            ncommits = dplyr::n (),
            nfiles_changed = sum (nfiles_changed),
            lines_added = sum (lines_added),
            lines_removed = sum (lines_removed)
        )

    log$index <- index_partial_duplicates (log) # in utils-author-matches.R
    # Then use that index to group all unique contributors:
    dplyr::group_by (log, index) |>
        dplyr::summarise (
            aut_name = dplyr::first (aut_name),
            aut_email = dplyr::first (aut_email),
            ncommits = sum (ncommits),
            nfiles_changed = sum (nfiles_changed),
            lines_changed = sum (lines_added + lines_removed)
        ) |>
        dplyr::select (-index)

}
