#' Start quarto dashboard with results of main \link{repo_pkgstats_history}
#' function.
#'
#' @param results Results of main \link{repo_pkgstats_history} function applied
#' to one package.
#' @param action One of "preview", to start and open a live preview of the
#' dashboard website, or "render" to render a static version without previewing
#' or opening.
#' @return (Invisibly) Path to main "index.html" document of quarto site. Note
#' that the site must be served with `action = "preview"`, and will not work by
#' simply opening this "index.html" file.
#' @export
ghist_dashboard <- function (results, action = "preview") {

    check_dashboard_arg (results)

    requireNamespace ("brio")
    requireNamespace ("quarto")
    requireNamespace ("withr")

    action <- match.arg (action, c ("preview", "render"))
    quarto_action <- paste0 ("quarto::quarto_", action)

    results <- daily_average (results, "stats")
    results <- daily_average (results, "desc_data")
    results <- daily_average (results, "loc")

    path_src <- system.file ("extdata", "quarto", package = "repometrics")
    path_dest <- fs::path (fs::path_temp (), "quarto")
    dir <- fs::dir_copy (path_src, path_dest, overwrite = TRUE)
    saveRDS (results, fs::path (dir, "results.Rds"))

    pkg_name <- results$desc_data$package [1]
    quarto_insert_pkg_name (dir, pkg_name)

    withr::with_dir (dir, {
        do.call (eval (parse (text = quarto_action)), list ())
    })
}

daily_average <- function (results, what = "stats") {

    results [[what]]$date <- lubridate::ymd (strftime (results [[what]]$date, "%y-%m-%d"))

    cols <- c ("package", "date", "language", "dir", "measure")
    cols <- cols [which (cols %in% names (results [[what]]))]

    results [[what]] <- dplyr::group_by (results [[what]], dplyr::across (dplyr::all_of (cols))) |>
        dplyr::select_if (is.numeric) |>
        dplyr::summarise_all (mean, na.rm = TRUE) |>
        dplyr::ungroup ()

    return (results)
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

check_dashboard_arg <- function (results) {

    checkmate::assert_list (results, len = 3L, names = "named")
    checkmate::assert_names (names (results), identical.to = c ("desc_data", "loc", "stats"))

    ncols <- vapply (results, ncol, integer (1L))
    ncols_expected <- c (desc_data = 9L, loc = 15L, stats = 8L)
    if (!identical (ncols, ncols_expected)) {
        cli::cli_abort (paste0 (
            "'results' has wrong number of columns; ",
            "should be [{ncols_expected}] but is ",
            "[{ncols}]"
        ))
    }

    nrows <- vapply (results, nrow, integer (1L))
    if (!all (nrows > 0L)) {
        cli::cli_abort ("'results' contains empty tables.")
    }
}
