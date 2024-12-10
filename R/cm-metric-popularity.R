cm_metric_popularity <- function (path, end_date = Sys.Date ()) {

    # suppress no visible binding notes:
    timestamp <- aut_email <- aut_name <- created <- starred_at <- NULL

    revdeps <- rm_data_dependencies_downstream (path)

    forks <- rm_data_repo_forks (path) |>
        dplyr::filter (created <= end_date)

    stars <- rm_data_repo_stargazers (path) |>
        dplyr::filter (starred_at <= end_date)

    log <- rm_data_gitlog (path) |>
        dplyr::mutate (timestamp = as.Date (timestamp)) |>
        dplyr::filter (timestamp <= end_date)
    log_ctbs <- unique (log [, c ("aut_name", "aut_email")]) |>
        dplyr::filter (!duplicated (aut_email)) |>
        dplyr::filter (!duplicated (aut_name))


    c (
        revdeps = length (revdeps),
        contribs = nrow (log_ctbs),
        forks = nrow (forks),
        stars = nrow (stars)
    )
}
