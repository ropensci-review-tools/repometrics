#' Does software have an OpenSSF Best Practices badge?
#'
#' This extends beyond the single CHAOSS recommendation of OpenSSF to also
#' include the core infrastructure best practices badge.
#'
#' \url{https://chaoss.community/kb/metric-open-source-security-foundation-openssf-best-practices-badge/}
#'
#' This metric does not use the `end_date` parameter (but includes it to
#' provide a consistent interface).
#'
#' @noRd
cm_metric_best_practices <- function (path, end_date = NULL) {
    readme_path <- fs::dir_ls (path, regexp = "readme\\.md", ignore.case = TRUE)
    if (length (readme_path) == 0L) {
        return (FALSE)
    }
    readme <- readr::read_lines (readme_path, progress = FALSE)

    ptn_ci <- "bestpractices\\.coreinfrastructure\\.org"
    ptn_ossf <- "bestpractices\\.dev"
    ptn <- paste0 (c (ptn_ci, ptn_ossf), collapse = "|")
    return (any (grepl (ptn, readme)))
}
