#' Extract CHAOSS metric "Contributor Absence Factor"
#'
#' \url{https://chaoss.community/kb/metric-contributor-absence-factor/}.
#'
#' @param path Local path to repository
#' @param pkg_date Date at which metric is to be calculated.
#' @param nyears Number of preceding years over which metric is to be calculated.
#' @noRd
cm_metric_contrib_absence <- function (path, pkg_date = Sys.Date (), nyears = 1) {

    checkmate::assert_date (pkg_date)
    checkmate::assert_numeric (nyears, lower = 0L)

    start_date <- as.Date (pkg_date - round (nyears * 365.25))

    log <- rm_data_gitlog (path) |>
        dplyr::mutate (date = as.Date (timestamp)) |>
        dplyr::filter (date >= start_date & date <= pkg_date) |>
        dplyr::filter (aut_name != "GitHub") |>
        dplyr::group_by (aut_name, aut_email) |>
        dplyr::summarise (
            ncommits = dplyr::n (),
            nfiles_changed = sum (nfiles_changed),
            lines_added = sum (lines_added),
            lines_removed = sum (lines_removed)
        )

    # Reconcile partially duplicated names or emails:
    names_dup <- find_duplicated_strings (log$aut_name)
    index_names <- apply (names_dup, 1, function (i) {
        which (log$aut_name %in% i [1:2])
    }, simplify = FALSE)
    emails_dup <- find_duplicated_strings (log$aut_email)
    index_emails <- apply (emails_dup, 1, function (i) {
        which (log$aut_email %in% i [1:2])
    }, simplify = FALSE)

    index_dup <- lapply (index_names, function (i) {
        also_in_email <- which (vapply (
            index_emails,
            function (j) any (j %in% i),
            logical (1L)
        ))
        if (length (also_in_email) > 0) {
            i <- unique (c (i, index_emails [[also_in_email]]))
        }
        return (i)
    })

    # use that index list of duplicate entries to construct a single index with
    # duplicates as repeated values:
    log$index <- seq_len (nrow (log))
    for (i in index_dup) {
        log$index [i [-1]] <- log$index [i [1]]
    }
    # Then use that to group all unique contributors:
    log <- dplyr::group_by (log, index) |>
        dplyr::summarise (
            aut_name = dplyr::first (aut_name),
            aut_email = dplyr::first (aut_email),
            ncommits = sum (ncommits),
            nfiles_changed = sum (nfiles_changed),
            lines_changed = sum (lines_added + lines_removed)
        ) |>
        dplyr::select (-index)

    absence_factor <- function (log, what = "ncommits") {
        res <- dplyr::arrange (log, dplyr::desc (get (what))) |>
            dplyr::mutate (prop = cumsum (get (what) / sum (get (what))))
        length (which (res$prop < 0.5)) + 1L
    }

    numeric_cols <- vapply (
        as.list (log),
        is.numeric,
        logical (1L),
        USE.NAMES = TRUE
    )
    numeric_cols <- names (numeric_cols) [which (numeric_cols)]
    vapply (
        numeric_cols,
        function (i) absence_factor (log, what = i),
        integer (1L),
        USE.NAMES = TRUE
    )
}
