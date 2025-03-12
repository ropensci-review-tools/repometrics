load_model_json_data <- function () {
    f <- system.file (
        fs::path ("extdata", "chaoss-models", "chaoss-models.json"),
        package = "repometrics"
    )
    jsonlite::read_json (f, simplify = TRUE)
}

calculate_one_model <- function (path, end_date = Sys.Date (), metrics_data = NULL, model_name = NULL) {

    # Suppress no visible binding note:
    name <- NULL

    mod_dat <- load_model_json_data ()
    model_metrics <- mod_dat$models [[grep (model_name, names (mod_dat$models), fixed = TRUE)]]
    these_metrics <- dplyr::filter (mod_dat$metrics, name %in% model_metrics)
    fn_names <- paste0 ("cm_metric_", these_metrics$name)

    # metrics_values in the following are summed because some metrics like
    # committer count have multiple fields:
    if (is.null (metrics_data)) {
        pars <- list (path = path, end_date = end_date)
        metrics_values <- vapply (fn_names, function (f) {
            sum (do.call (f, pars))
        }, numeric (1L))
    } else {
        metrics_values <- unlist (lapply (
            these_metrics$name,
            function (n) sum (metrics_data [[n]])
        ))
    }

    index <- which (these_metrics$scale == "log" & !is.na (metrics_values))
    if (length (index) > 0L) {
        metrics_values [index] [metrics_values [index] < 0.1] <- 0.1
        metrics_values [index] <- log10 (metrics_values [index])
    }
    index <- which (these_metrics$better == "lower")
    if (length (index) > 0L) {
        metrics_values [index] <- -metrics_values [index]
    }

    metrics_values <- metrics_values [which (!is.na (metrics_values))]

    return (sum (metrics_values))
}
