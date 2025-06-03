rm_data_r_universe <- function (path) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    if (!is_test_env) {

        pkg_name <- pkg_name_from_path (path)
        universe <- get_r_univ_universe (pkg_name)

    } else {

        pkg_name <- "repometrics"
        universe <- "ropensci-review-tools"
    }

    get_r_univ_pkg_data (pkg_name, universe)
}

get_r_univ_universe <- function (pkg_name) {

    packages <- httr2::request ("https://r-universe.dev/api/search") |>
        httr2::req_url_query (q = pkg_name) |>
        httr2::req_user_agent ("R-universe docs") |>
        httr2::req_perform () |>
        httr2::resp_body_json ()

    pkg_names <- vapply (
        packages$results,
        function (i) i$Package,
        character (1L)
    )

    if (!pkg_name %in% pkg_names) {
        cli::cli_abort ("Package '{pkg_name}' not found on r-universe")
    }

    pkg_data <- packages$results [[which (pkg_names == pkg_name)]]
    return (pkg_data$`_user`)
}

get_r_univ_pkg_data <- function (pkg_name, universe) {

    url <- paste0 ("https://", universe, ".r-universe.dev/api/packages/", pkg_name)
    pkg_data_full <- httr2::request (url) |>
        httr2::req_user_agent ("R-universe docs") |>
        httr2::req_perform () |>
        httr2::resp_body_json ()

    created <- pkg_data_full$`_created`
    jobs <- do.call (rbind, lapply (pkg_data_full$`_jobs`, data.frame))
    binaries <- lapply (pkg_data_full$`_binaries`, function (b) {
        res <- data.frame (b)
        if (!"distro" %in% names (res)) {
            res <- dplyr::mutate (res, distro = NA_character_, .after = "date")
        }
        if (!"check" %in% names (res)) {
            res <- dplyr::mutate (res, check = NA_character_, .after = "status")
        }
        return (res)
    })
    binaries <- do.call (rbind, binaries)

    list (
        package = pkg_name,
        universe = universe,
        created = created,
        jobs = jobs,
        binaries = binaries
    )
}
