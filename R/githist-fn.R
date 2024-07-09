#' Apply \pkg{pkgstats} across the git history of a package
#'
#' @param path Path to local repository containing an R package.
#' @export
githist <- function (path) {
    checkmate::assert_character (path, len = 1L)
    checkmate::assert_directory (path)

    path_cp <- fs::dir_copy (path, fs::path_temp ())

    h <- gert::git_log (repo = path_cp, max = 1e6)

    res <- pbapply::pblapply (seq_len (nrow (h)), function (i) {
        g <- gert::git_reset_soft (ref = h$commit [i], repo = path_cp)
        run_one_pkgstats (path = path_cp)
    })
}
