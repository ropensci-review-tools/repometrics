#' Extract CHAOSS metric "Programming Language Distribution"
#'
#' \url{https://chaoss.community/?p=3430}.
#'
#' @param path Local path to repository
#' @param end_date Not used here, but specified for consistent interface to all
#' metric fns.
#' @noRd
cm_data_languages <- function (path, end_date = NULL) {

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

    # Consider all "Rmd" files to be "R" files:
    s <- dplyr::mutate (
        s,
        language = dplyr::case_when (
            language == "Rmd" ~ "R",
            TRUE ~ language
        )
    )

    dplyr::group_by (s, language) |>
        dplyr::summarise (nfiles = sum (nfiles), ncode = sum (ncode)) |>
        dplyr::mutate (
            nfiles_pc = ifelse (sum (nfiles) == 0, 0, nfiles / sum (nfiles)),
            ncode_pc = ifelse (sum (ncode) == 0, 0, ncode / sum (ncode))
        )
}

cm_metric_languages <- function (path, end_date = NULL) {
    dat <- cm_data_languages (path, end_date)
    lang_dist_mn <- mean (dat$ncode_pc) # lower is better
    # Re-scale this so that 4 languages translates to a value of 1:
    lang_dist_mn <- ifelse (lang_dist_mn == 0, 0, 0.25 / lang_dist_mn)
    return (lang_dist_mn)
}
