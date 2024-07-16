#' Start quarto dashboard with results of main \link{githist} function.
#'
#' @param results Results of main \link{githist} function applied to one
#' package.
#' @return Nothing; quarto dashboard will automatically open on function call.
#' @export
gh_dashboard <- function (results) {

    requireNamespace ("quarto")
    requireNamespace ("withr")

    path_src <- system.file ("extdata", "quarto", package = "githist")
    dir <- fs::dir_copy (path_src, fs::path_temp ())
    saveRDS (results, fs::path (dir, "results.Rds"))

    withr::with_dir (dir, {
        quarto::quarto_preview ()
    })
}
