rm_data_r_univ <- function (path) {

    pkg_name <- pkg_name_from_path (path)
    universe <- get_r_univ_universe (pkg_name)
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
