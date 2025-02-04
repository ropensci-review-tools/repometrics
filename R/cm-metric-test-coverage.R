cm_metric_test_coverage_internal <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding notes:
    name <- coverage <- NULL

    requireNamespace ("readr", quietly = TRUE)

    or <- org_repo_from_path (path)
    wf_tests <- rm_data_gh_repo_workflow (path) |>
        dplyr::filter (grepl ("test|coverage", name))

    res0 <- data.frame (
        id = integer (0),
        created = double (0),
        coverage = double (0),
        row.names = NULL
    )
    if (nrow (wf_tests) == 0) {
        return (res0)
    }

    # This returns the most recent test coverage value:
    cov <- NA_real_
    i <- 1L
    while (is.na (cov) & i <= nrow (wf_tests)) {
        cov <- coverage_from_one_log (wf_tests$logs_url [i])
        i <- i + 1L
    }
    res <- data.frame (
        id = wf_tests$id [i - 1],
        created = wf_tests$created [i - 1],
        coverage = cov,
        row.names = NULL
    )

    return (res)
}
cm_metric_test_coverage <- memoise::memoise (cm_metric_test_coverage_internal)
