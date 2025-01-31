get_cm_fns <- function (what = "metric") {

    what <- match.arg (what, c ("metric", "model"))
    ptn <- paste0 ("^cm\\_", what, "\\_")

    pkg_fns <- ls (envir = asNamespace ("repometrics"))
    fns <- grep (ptn, pkg_fns, value = TRUE)

    return (fns)
}

collate_all_metrics <- function (path, end_date = Sys.Date ()) {

    metric_fns <- get_cm_fns ("metric")

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

collate_all_models <- function (path, end_date = Sys.Date ()) {

    model_fns <- get_cm_fns ("model")
    pars <- list (path = path, end_date = end_date)

    model_data <- vapply (
        model_fns,
        function (f) do.call (f, pars),
        numeric (1L)
    )
    names (model_data) <- gsub ("^cm\\_model\\_", "", model_fns)

    return (model_data)
}
