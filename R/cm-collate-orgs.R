repometrics_collate_org_data <- function (org_paths, end_date = Sys.Date, num_years = 3) {

    pkgs <- fs::dir_ls (org_paths, type = "directory")
    data <- lapply (seq_along (pkgs), function (i) {
        cli::cli_alert_info ("[{i} / {length(pkgs)}]: {pkgs[i]}")
        path_i <- pkgs [i]
        pkg_i <- basename (path_i)
        f_tmp <- fs::path (fs::path_temp (), paste0 (pkg_i, ".Rds"))
        if (fs::file_exists (f_tmp)) {
            dat_i <- readRDS (f_tmp)
        } else {
            dat_i <- list (
                repo = repometrics_data_repo (path_i),
                metrics = metrics_over_end_dates (
                    path_i,
                    end_date = end_date,
                    num_years = num_years
                ),
                models = models_over_end_dates (
                    path_i,
                    end_date = end_date,
                    num_years = num_years
                )
            )
            saveRDS (dat_i, f_tmp)
        }
        return (dat_i)
    })

    pkgs_repos <- lapply (data, function (i) i$repo)
    pkgs_metrics <- lapply (data, function (i) i$metrics)
    pkgs_models <- lapply (data, function (i) i$models)

    or <- vapply (pkgs, function (i) {
        paste0 (org_repo_from_path (i), collapse = "/")
    }, character (1L), USE.NAMES = FALSE)
    names (pkgs_repos) <- names (pkgs_metrics) <- names (pkgs_models) <- or

    pkgs_models <- lapply (seq_along (pkgs_models), function (i) {
        pkgs_models [[i]] |>
            dplyr::mutate (package = names (pkgs_models) [i], .before = 1)
    })
    pkgs_models <- do.call (rbind, pkgs_models)

    rm_tmp_pkg_files (or)

    data <- list (
        repos = pkgs_repos,
        metrics = pkgs_metrics,
        models = pkgs_models
    )

    return (data)
}

rm_tmp_pkg_files <- function (pkgs) {
    pkgs <- gsub ("^.*\\/", "", unique (pkgs))
    f_tmp_list <- fs::path (fs::path_temp (), paste0 (pkgs, ".Rds"))
    f_tmp_list <- f_tmp_list [which (fs::file_exists (f_tmp_list))]
    if (length (f_tmp_list) > 0L) {
        fs::file_delete (f_tmp_list)
    }
}
