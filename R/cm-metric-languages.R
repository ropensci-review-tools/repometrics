#' Extract CHAOSS metric "Programming Language Distribution"
#'
#' \url{https://chaoss.community/?p=3430}.
#'
#' @param path Local path to repository
#' @noRd
cm_metric_languages <- function (path) {

    # Suppress no visible binding note:
    language <- nfiles <- ncode <- NULL

    loc_stats <- utils::getFromNamespace ("loc_stats", "pkgstats")

    s <- tryCatch (
        loc_stats (path),
        error = function (e) NULL
    )
    if (is.null (s)) {
        return (NULL)
    }

    dplyr::group_by (s, language) |>
        dplyr::summarise (nfiles = sum (nfiles), ncode = sum (ncode)) |>
        dplyr::mutate (
            nfiles_pc = nfiles / sum (nfiles),
            ncode_pc = ncode / sum (ncode)
        )
}
