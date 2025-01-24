#' Metric fo rnumber of files with declared licenses
#'
#' \url{https://chaoss.community/kb/metric-license-coverage/}
#' @noRd
#'
cm_metric_licenses_declared <- function (path, dirs = c ("R", "src", "inst/extdata")) {

    requireNamespace ("readr")

    if (any (grepl ("/", dirs, fixed = TRUE))) {
        dirs <- gsub ("/", .Platform$file.sep, dirs, fixed = TRUE)
    }
    # pre-pend platform file.sep:
    file_sep_char1 <- substr (dirs, 1, 1)
    index <- which (file_sep_char1 != .Platform$file.sep)
    dirs [index] <- paste0 (.Platform$file.sep, dirs [index])
    # Then append platform file.sep:
    file_sep_char_n <- substr (dirs, nchar (dirs), nchar (dirs))
    index <- which (file_sep_char_n != .Platform$file.sep)
    dirs [index] <- paste0 (dirs [index], .Platform$file.sep)
    ptn = paste0 (dirs, collapse = "|")

    flist <- fs::dir_ls (path, regexp = ptn, recurse = TRUE)
    flist <- flist [which (tolower (fs::path_ext (flist)) %in% included_exts)]

    if (length (flist) == 0L) {
        return (0)
    }

    max_lines <- 20
    lic_ptn <- paste0 (included_licenses, collapse = "|")
    has_license <- vapply (flist, function (f) {
        f_hdr <- readr::read_lines (
            f,
            n_max = max_lines,
            progress = FALSE
        )
        has_license <- any (grepl ("license", f_hdr, ignore.case = TRUE))
        has_r_licenses <- any (grepl (lic_ptn, f_hdr, ignore.case = FALSE))
        return (has_license && has_r_licenses)
    }, logical (1L))

    return (length (which (has_license)) / length (flist))
}

included_exts <- c ("r", "q", "qmd", "rmd", "c", "cpp", "h", "js", "py")

# https://www.r-project.org/Licenses/
included_licenses <-
    c ("GNU", "(A|a)rtistic (L|l)icense", "BSD", "MIT", "(C|c)reative (C|c)ommons")
