get_cm_fns <- function (what = "metric") {

    what <- match.arg (what, c ("metric", "model"))
    ptn <- paste0 ("^cm\\_", what, "\\_")

    pkg_fns <- ls (envir = asNamespace ("repometrics"))
    fns <- grep (ptn, pkg_fns, value = TRUE)
    fns <- fns [which (!grepl ("internal", fns))]

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

collate_all_models <- function (path,
                                end_date = Sys.Date (),
                                metrics_data = NULL) {

    model_fns <- get_cm_fns ("model")

    if (is.null (metrics_data)) {
        pars <- list (path = path, end_date = end_date)
    } else {
        pars <- list (metrics_data = metrics_data)
    }

    model_data <- vapply (
        model_fns,
        function (f) do.call (f, pars),
        numeric (1L)
    )

    names (model_data) <- gsub ("^cm\\_model\\_", "", model_fns)

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
