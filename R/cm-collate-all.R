#' List all implemented CHAOSS metrics
#'
#' This function returns a list of internal functions defined within the
#' 'repometrics' package. These internal functions are not intended to be
#' called directly, rather this list is provided for information only, to
#' enable users to know which metrics are implemented.
#'
#' @return A `data.frame` with two columns:
#' \enumerate{
#' \item "fn_names", with the internal function names of all implemented CHAOSS
#' metrics.
#' \item "url", with the URL to the CHAOSS community web page describing that
#' metric.
#' }
#'
#' @note Metrics have been adapted in this package, and so may not precisely
#' reflect the descriptions provided in the CHAOSS community web pages linked
#' to in the URLs from this function. Adaptations have in particular been
#' implemented to align metrics with their usage in aggregate "models".
#'
#' @examples
#' metrics <- rm_chaoss_metrics_list ()
#' @family auxiliary
#' @export
rm_chaoss_metrics_list <- function () {

    fn_names <- chaoss_metrics_fn_names ()
    url_fns <- paste0 (fn_names, "_url")
    urls <- vapply (url_fns, function (u) do.call (u, list ()), character (1L))
    urls <- paste0 (rm_metric_base_url (), unname (urls))

    data.frame (fn_name = fn_names, url = urls)
}

chaoss_metrics_fn_names <- function () {

    ptn <- "^rm\\_metric\\_"
    pkg_fns <- ls (envir = asNamespace ("repometrics"))
    fns <- grep (ptn, pkg_fns, value = TRUE)
    fns <- fns [which (!grepl ("\\_internal|\\_url$", fns))]

    return (fns)

}

collate_all_metrics <- function (path, end_date = Sys.Date ()) {

    metric_fns <- rm_chaoss_metrics_list ()$fn_name

    pars <- list (path = path, end_date = end_date)
    extra_pars <- list (
        rm_metric_burstiness = list (band_len = 31L, band_width = 2),
        rm_metric_code_change_lines = list (exclude_whitespace = TRUE),
        rm_metric_license_coverage =
            list (dirs = c ("R", "src", "inst/extdata"))
    )

    metrics_data <- lapply (metric_fns, function (f) {

        these_pars <- pars
        if (f %in% names (extra_pars)) {
            these_pars <- c (pars, extra_pars [[f]])
        }
        do.call (f, these_pars)
    })
    names (metrics_data) <- gsub ("^rm\\_metric\\_", "", metric_fns)

    return (metrics_data)
}

collate_all_models <- function (path,
                                end_date = Sys.Date (),
                                metrics_data = NULL) {

    mod_dat <- load_model_json_data ()
    model_names <- names (mod_dat$models)

    if (is.null (metrics_data)) {
        pars <- list (path = path, end_date = end_date)
    } else {
        pars <- list (metrics_data = metrics_data)
    }

    model_data <- vapply (
        model_names,
        function (m) do.call (calculate_one_model, c (pars, model_name = m)),
        numeric (1L)
    )

    return (model_data)
}

metrics_over_end_dates <- function (path, end_date = Sys.Date (), num_years = 3) {

    end_dates <- get_end_date_seq (end_date = end_date, num_years = num_years)

    metrics_data <- lapply (
        end_dates,
        function (d) collate_all_metrics (path, end_date = d)
    )
    names (metrics_data) <- as.character (end_dates)
    attr (metrics_data, "period") <- get_repometrics_period ()

    return (metrics_data)
}

models_over_end_dates <- function (path, end_date = Sys.Date (), num_years = 3) {

    end_dates <- get_end_date_seq (end_date = end_date, num_years = num_years)

    models_data <- lapply (
        end_dates,
        function (d) collate_all_models (path, end_date = d)
    )
    models_data <- data.frame (do.call (rbind, models_data)) |>
        dplyr::mutate (date = end_dates, .before = 1)

    return (models_data)
}

rm_metric_base_url <- function () {
    "https://chaoss.community/kb/"
}
