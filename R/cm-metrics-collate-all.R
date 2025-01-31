get_cm_metric_fns <- function () {

    pkg_fns <- ls (envir = asNamespace ("repometrics"))
    metric_fns <- grep ("^cm\\_metric", pkg_fns, value = TRUE)

    return (metric_fns)
}

collate_all_metrics <- function (path, end_date = Sys.Date ()) {

    metric_fns <- get_cm_metric_fns ()

    pars <- list (path = path, end_date = end_date)
    extra_pars <- list (
        cm_metric_burstiness = list (band_len = 31L, band_width = 2),
        cm_metric_code_change_lines = list (exclude_whitespace = TRUE),
        cm_metric_license_coverage =
            list (dirs = c ("R", "src", "inst/extdata"))
    )

    metrics_data <- lapply (metric_fns, function (f) {

        these_pars <- pars
        if (f %in% names (extra_pars)) {
            these_pars <- c (pars, extra_pars [[f]])
        }
        do.call (f, these_pars)
    })
    names (metrics_data) <- gsub ("^cm\\_metric\\_", "", metric_fns)

    return (metrics_data)
}
