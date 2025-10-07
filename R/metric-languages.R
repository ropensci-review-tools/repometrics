#' Extract CHAOSS metric "Programming Language Distribution"
#'
#' \url{https://chaoss.community/?p=3430}.
#'
#' @param path Local path to repository
#' @param end_date Not used here, but specified for consistent interface to all
#' metric fns.
#' @noRd
rm_data_languages_internal <- function (path, end_date = NULL) {

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
        dplyr::filter (ncode > 0) |>
        dplyr::mutate (
            nfiles_pc = nfiles / sum (nfiles),
            ncode_pc = ncode / sum (ncode)
        )
}

# The mean value of the percetage of code per language. As this is a
# standardised measure, this is simply equal to 1 / number-of-languages, but
# full calculation used regardless to allow potential improvement in the
# future.
rm_metric_languages <- function (path, end_date = NULL) {
    dat <- rm_data_languages_internal (path, end_date)
    lang_dist_mn <- mean (dat$ncode_pc) # lower is better
    # Re-scale this so that 4 languages translates to a value of 1:
    lang_dist_mn <- ifelse (lang_dist_mn == 0, 0, 0.25 / lang_dist_mn)
    return (lang_dist_mn)
}

rm_metric_languages_url <- function () {
    "chaoss.community/?p=3430"
}
