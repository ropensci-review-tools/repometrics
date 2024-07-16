#' Start quarto dashboard with results of main \link{githist} function.
#'
#' @param results Results of main \link{githist} function applied to one
#' package.
#' @return Nothing; quarto dashboard will automatically open on function call.
#' @export
gh_dashboard <- function (results) {

    requireNamespace ("brio")
    requireNamespace ("quarto")
    requireNamespace ("withr")

    path_src <- system.file ("extdata", "quarto", package = "githist")
    dir <- fs::dir_copy (path_src, fs::path_temp ())
    saveRDS (results, fs::path (dir, "results.Rds"))

    pkg_name <- results$desc_data$package [1]
    quarto_insert_pkg_name (dir, pkg_name)

    withr::with_dir (dir, {
        quarto::quarto_preview ()
    })
}

quarto_insert_pkg_name <- function (dir, pkg_name) {

    f_index <- fs::path (dir, "index.qmd")
    index_qmd <- brio::read_lines (f_index)
    i <- grep ("^title\\:", index_qmd)
    index_qmd [i] <- paste0 ("title: The {", pkg_name, "} package")
    brio::write_lines (index_qmd, f_index)

    f_yaml <- fs::path (dir, "_quarto.yml")
    # yaml package can't properly handle quotations for quarto yaml
    y <- brio::read_lines (f_yaml)
    i <- grep ("^(\\s+?)title", y)
    y [i] <- gsub ("Package", pkg_name, y [i])
    brio::write_lines (y, f_yaml)
}
