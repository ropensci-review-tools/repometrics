#' Start quarto dashboard with results of main \link{repometrics_data_repo}
#' function.
#'
#' @param data_repo Data on repository as returned from
#' \link{repometrics_data_repo} function applied to one package.
#' @param data_users Data on repository developers ("users" in GitHub terms), as
#' returned from \link{repometrics_data_user} function applied to one package.
#' @param action One of "preview", to start and open a live preview of the
#' dashboard website, or "render" to render a static version without previewing
#' or opening.
#' @return (Invisibly) Path to main "index.html" document of quarto site. Note
#' that the site must be served with `action = "preview"`, and will not work by
#' simply opening this "index.html" file.
#' @export
repometrics_dashboard <- function (data_repo, data_users, action = "preview") {

    check_dashboard_arg (data_repo)
    data_repo$pkgstats <- timestamps_to_dates (data_repo$pkgstats)

    requireNamespace ("brio")
    requireNamespace ("jsonlite")
    requireNamespace ("quarto")
    requireNamespace ("withr")

    action <- match.arg (action, c ("preview", "render"))
    quarto_action <- paste0 ("quarto::quarto_", action)

    path_src <- system.file ("extdata", "quarto", package = "repometrics")
    path_dest <- fs::path (fs::path_temp (), "quarto")
    dir <- fs::dir_copy (path_src, path_dest, overwrite = TRUE)
    saveRDS (data_repo, fs::path (dir, "results-repo.Rds"))
    saveRDS (data_users, fs::path (dir, "results-users.Rds"))

    dat_user_network <- get_user_network (dat_users)
    jsonlite::write_json (dat_user_network, fs::path (dir, "results-user-network.json"))

    pkg_name <- data_repo$pkgstats$desc_data$package [1]
    quarto_insert_pkg_name (dir, pkg_name)

    withr::with_dir (dir, {
        do.call (eval (parse (text = quarto_action)), list ())
    })
}

get_user_network <- function (data_users) {

    rels <- user_relation_matrices (data_users)
    index <- which (!grepl ("^login", names (rels)))
    relmat <- apply (as.matrix (rels [, index]), 2, function (i) i / sum (i))
    relvec <- 20 * rowSums (relmat) / ncol (relmat)
    reldf <- cbind (rels [, 1:2], value = relvec)
    names (reldf) <- c ("source", "target", "value")

    netdat <- list (
        nodes = data.frame (
            id = unique (c (rels$login1, rels$login2)),
            group = 1L
        ),
        links = reldf
    )

    return (netdat)
}

timestamps_to_dates <- function (data) {

    lapply (data, function (i) {

        i$date <- as.Date (i$date)

        if (any (duplicated (i$date))) {

            cols <- c ("package", "date", "language", "dir", "measure")
            cols <- cols [which (cols %in% names (i))]

            i <- dplyr::group_by (i, dplyr::across (dplyr::all_of (cols))) |>
                dplyr::select_if (is.numeric) |>
                dplyr::summarise_all (mean, na.rm = TRUE) |>
                dplyr::ungroup ()
        }

        return (dplyr::arrange (i, by = dplyr::desc (date)))
    })
}

quarto_insert_pkg_name <- function (dir, pkg_name) {

    f_index <- fs::path (dir, "index.qmd")
    index_qmd <- brio::read_lines (f_index)
    i <- grep ("^title\\:", index_qmd)
    index_qmd [i] <- paste0 ("title: The {", pkg_name, "} package")
    index_qmd <- gsub (
        "the XXX repository",
        paste ("the", pkg_name, "repository"),
        index_qmd,
        fixed = TRUE
    )
    brio::write_lines (index_qmd, f_index)

    f_yaml <- fs::path (dir, "_quarto.yml")
    # yaml package can't properly handle quotations for quarto yaml
    y <- brio::read_lines (f_yaml)
    i <- grep ("^(\\s+?)title", y)
    y [i] <- gsub ("Package", pkg_name, y [i])
    brio::write_lines (y, f_yaml)
}

check_dashboard_arg <- function (data) {

    checkmate::assert_list (data, len = 2L, names = "named")
    checkmate::assert_names (names (data), identical.to = c ("pkgstats", "rm"))
    checkmate::assert_names (names (data$pkgstats), identical.to = c ("desc_data", "loc", "stats"))
    nms <- c (
        "contribs_from_gh_api", "contribs_from_log", "dependencies",
        "dependencies_downstream", "gh_repo_workflow", "gitlog",
        "issue_comments_from_gh_api", "issues_from_gh_api", "libyears",
        "prs_from_gh_api", "releases_from_gh_api", "repo_forks",
        "repo_from_gh_api", "repo_stargazers", "contributors"
    )
    checkmate::assert_names (names (data$rm), identical.to = nms)

    # ------ pkgstats structure ------
    ncols <- vapply (data$pkgstats, ncol, integer (1L))
    ncols_expected <- c (desc_data = 9L, loc = 15L, stats = 8L)
    if (!identical (ncols, ncols_expected)) {
        cli::cli_abort (paste0 (
            "'data' has wrong number of columns; ",
            "should be [{ncols_expected}] but is ",
            "[{ncols}]"
        ))
    }

    nrows <- vapply (data$pkgstats, nrow, integer (1L))
    if (!all (nrows > 0L)) {
        cli::cli_abort ("'data' contains empty tables.")
    }

    # ------ rm structure ------
    classes <- vapply (data$rm, class, character (1L))
    index <- which (classes == "data.frame")
    if (length (index) != (length (classes) - 1L)) {
        cli::cli_abort ("Chaoss metrics data have wrong length; should be {length(index)}.")
    }

    ncols <- vapply (data$rm [index], ncol, integer (1L))
    ncols_expected <- c (
        "contribs_from_gh_api" = 17L,
        "contribs_from_log" = 2L,
        "dependencies" = 3L,
        "gh_repo_workflow" = 7L,
        "gitlog" = 10L,
        "issue_comments_from_gh_api" = 9L,
        "issues_from_gh_api" = 24L,
        "libyears" = 6L,
        "prs_from_gh_api" = 23L,
        "releases_from_gh_api" = 10L,
        "repo_forks" = 2L,
        "repo_from_gh_api" = 18L,
        "repo_stargazers" = 2L,
        "contributors" = 3L
    )
    if (!identical (ncols, ncols_expected)) {
        cli::cli_abort (paste0 (
            "'data' has wrong number of columns; ",
            "should be [{ncols_expected}] but is ",
            "[{ncols}]"
        ))
    }

    nrows <- vapply (data$rm [index], nrow, integer (1L))
    # These tables may be empty
    maybe_empty <- c ("dependencies", "releases_from_gh_api", "repo_forks")
    nrows <- nrows [which (!names (nrows) %in% maybe_empty)]
    if (!all (nrows > 0L)) {
        cli::cli_abort ("'data' contains empty tables.")
    }
}
