#' Start quarto dashboard with results of main \link{repometrics_data_repo}
#' function.
#'
#' @param data Data on repository and all contributors as returned from
#' \link{repometrics_data} function applied to one package.
#' @param action One of "preview", to start and open a live preview of the
#' dashboard website, or "render" to render a static version without previewing
#' or opening.
#' @param ctb_threshold An optional single numeric value between 0 and 1. If
#' specified, contributions are arranged in cumulative order, and the
#' contributor data reduced to only those who contribute to this proportion of
#' all contributions.
#' @param max_ctbs Optional maximum number of contributors to be included. This
#' is an alternative way to reduce number of contributors presented in
#' dashboard, and may only be specified if `ctb_threshold` is left at default
#' value of `NULL`.
#'
#' @return (Invisibly) Path to main "index.html" document of quarto site. Note
#' that the site must be served with `action = "preview"`, and will not work by
#' simply opening this "index.html" file.
#'
#' @family dashboard
#' @export
repometrics_dashboard <- function (data, action = "preview",
                                   ctb_threshold = NULL, max_ctbs = NULL) {

    if (!is.null (ctb_threshold)) {
        checkmate::assert_numeric (
            ctb_threshold,
            len = 1L,
            lower = 0,
            upper = 1
        )
        if (!is.null (max_ctbs)) {
            cli::cli_abort (
                "Only one of 'ctb_threshold' or 'max_ctbs' may be specified."
            )
        }
    }
    if (!is.null (max_ctbs)) {
        checkmate::assert_integerish (
            max_ctbs,
            len = 1L,
            lower = 1,
            upper = length (data$contributors)
        )
    }

    data_ctbs <- data$contributors
    data$contributors <- NULL
    if (!is.null (ctb_threshold) || !is.null (max_ctbs)) {
        data_ctbs <- reduce_data_users (data_ctbs, ctb_threshold, max_ctbs)
    }

    check_dashboard_arg (data)
    data$pkgstats <- timestamps_to_dates (data$pkgstats)

    requireNamespace ("brio")
    requireNamespace ("jsonlite")
    requireNamespace ("quarto")
    requireNamespace ("withr")

    action <- match.arg (action, c ("preview", "render"))
    quarto_action <- paste0 ("quarto::quarto_", action)

    path_src <- system.file ("extdata", "quarto", package = "repometrics")
    path_dest <- fs::path (fs::path_temp (), "quarto")
    dir <- fs::dir_copy (path_src, path_dest, overwrite = TRUE)
    saveRDS (data, fs::path (dir, "results-repo.Rds"))
    saveRDS (data_ctbs, fs::path (dir, "results-users.Rds"))

    dat_user_network <- get_user_network (data, data_ctbs)
    jsonlite::write_json (
        dat_user_network,
        fs::path (dir, "results-user-network.json")
    )

    pkg_name <- data$pkgstats$desc_data$package [1]
    quarto_insert_pkg_name (dir, pkg_name)

    withr::with_dir (dir, {
        do.call (eval (parse (text = quarto_action)), list ())
    })
}

reduce_data_users <- function (data_users,
                               ctb_threshold = NULL,
                               max_ctbs = NULL) {

    classes <- vapply (data_users [[1]], class, character (1L))
    index <- which (classes == "data.frame")
    # Those are "commit_cmt", "commits", "issue_cmts", "issues"
    rowcounts <- t (vapply (data_users, function (u) {
        vapply (u [index], nrow, integer (1L))
    }, integer (length (index))))
    n <- sort (rowSums (rowcounts), decreasing = TRUE)

    if (!is.null (max_ctbs)) {
        these_ctbs <- names (n) [seq_len (max_ctbs)]
        index <- sort (match (these_ctbs, names (data_users)))
    } else {
        ncum <- cumsum (n) / sum (n)
        ctbs_trimmed <- names (ncum) [which (ncum <= ctb_threshold)]
        index <- sort (match (ctbs_trimmed, names (data_users)))
    }
    data_users <- data_users [index]

    return (data_users)
}

# `range` is used to scale values, and restrict to sufficiently large values.
# Total range is first re-scaled to maximum of `range[2]`, then values below
# `range[1]` are removed.
get_user_network <- function (data_repo, data_users, range = c (1, 20)) {

    # Suppress no visible binding notes:
    repo <- num_commits <- user <- value <- target <- NULL

    rels <- user_relation_matrices (data_users) # in R/analyse-users.R
    index <- which (!grepl ("^login", names (rels)))
    relmat <- apply (as.matrix (rels [, index]), 2, function (i) i / sum (i))
    if (!is.matrix (relmat)) {
        relmat <- matrix (relmat, nrow = 1L)
    }
    relmat [which (is.na (relmat))] <- 0
    relvec <- rowSums (relmat) / ncol (relmat)
    reldf <- cbind (rels [, 1:2], value = relvec)
    names (reldf) <- c ("source", "target", "value")
    reldf$type <- "person"

    reldf$value <- reldf$value * range [2] / max (reldf$value)
    reldf <- reldf [which (reldf$value >= range [1]), ]

    netdat <- list (
        nodes = data.frame (
            id = unique (c (reldf$source, reldf$target)),
            group = "person"
        ),
        links = reldf
    )

    # Then append num. commits to node data:
    login <- contributions <- id <- NULL
    user_commits <- data_repo$rm$contribs_from_gh_api |>
        dplyr::select (login, contributions) |>
        dplyr::rename (id = login)
    netdat$nodes <- dplyr::left_join (
        netdat$nodes,
        user_commits,
        by = dplyr::join_by (id)
    ) |>
        dplyr::mutate (
            contributions = range [2] * contributions / max (contributions)
        )

    # Then expand to include repos as well as users:
    dat_user_repo_network <- get_user_repo_network (data_users)
    nodes <- dat_user_repo_network$repos |>
        dplyr::rename (id = repo, contributions = num_commits) |>
        dplyr::mutate (group = "repo", .after = id)
    netdat$nodes <- rbind (netdat$nodes, nodes)

    links <- dat_user_repo_network$users |>
        dplyr::rename (source = user, target = repo, value = num_commits) |>
        dplyr::mutate (
            value = range [2] * value / max (value),
            type = "person_repo"
        ) |>
        dplyr::filter (target %in% nodes$id & source %in% netdat$nodes$id)
    netdat$links <- dplyr::bind_rows (netdat$links, links)

    # Remove any nodes which then have no links:
    netdat$nodes <- netdat$nodes |>
        dplyr::filter (id %in% c (netdat$links$source, netdat$links$target))

    # Finally add focal repo to those data (if not already there)
    this_repo <- data_repo$rm$repo_from_gh_api$full_name
    if (this_repo %in% netdat$nodes$id) {
        netdat$nodes$group [which (netdat$nodes$id == this_repo)] <- "this_repo"
    } else {
        netdat$nodes <- dplyr::bind_rows (
            netdat$nodes,
            data.frame (
                id = this_repo,
                group = "this_repo",
                contributions = range [2]
            )
        )
    }
    if (!this_repo %in% netdat$links$target) {
        ctbs <- data_repo$rm$contribs_from_gh_api |>
            dplyr::select (login, contributions) |>
            dplyr::rename (value = contributions, source = login) |>
            dplyr::mutate (
                target = this_repo,
                value = range [2] * value / max (value),
                type = "person_this_repo"
            ) |>
            dplyr::filter (source %in% netdat$nodes$id)
        netdat$links <- dplyr::bind_rows (netdat$links, ctbs)
    }

    # And add an "org" column to enable filtering:
    org_node <- gsub ("\\/.*$", "", netdat$nodes$id)
    org_node [which (org_node == netdat$nodes$id)] <- ""
    netdat$nodes$org <- org_node

    org_links <- gsub ("\\/.*$", "", netdat$links$target)
    org_links [which (org_links == netdat$links$target)] <- ""
    netdat$links$org <- org_links

    return (netdat)
}

get_user_repo_network <- function (data_users,
                                   rm_personal = TRUE,
                                   range = c (1, 20)) {

    # Suppress no visible binding notes:
    repo <- num_commits <- user <- repo <- NULL

    commits_users <- lapply (data_users, function (i) {
        dplyr::group_by (i$commits, repo) |>
            dplyr::summarise (num_commits = sum (num_commits)) |>
            dplyr::mutate (user = i$general$user$login)
    })
    commits_users <- do.call (rbind, commits_users)

    if (rm_personal) {

        repo_org <- gsub ("\\/.*$", "", commits_users$repo)
        commits_users <- dplyr::filter (commits_users, user != repo_org)
    }

    commits_repos <- dplyr::group_by (commits_users, repo) |>
        dplyr::summarise (num_commits = sum (num_commits)) |>
        dplyr::mutate (
            num_commits = num_commits * range [2] / max (num_commits)
        ) |>
        dplyr::filter (num_commits >= range [1]) |>
        dplyr::arrange (dplyr::desc (num_commits))

    return (list (users = commits_users, repos = commits_repos))
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

        dplyr::arrange (i, by = dplyr::desc (date))
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

    f_network <- fs::path (dir, "network.qmd")
    network_qmd <- brio::read_lines (f_network)
    network_qmd <- gsub (
        "the XXX repository",
        paste0 ("the `", pkg_name, "` repository"),
        network_qmd,
        fixed = TRUE
    )
    brio::write_lines (network_qmd, f_network)
}

check_dashboard_arg <- function (data) {

    checkmate::assert_list (data, len = 2L, names = "named")
    checkmate::assert_names (names (data), identical.to = c ("pkgstats", "rm"))
    checkmate::assert_names (
        names (data$pkgstats),
        identical.to = c ("desc_data", "loc", "stats", "ext_calls")
    )
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
    ncols_expected <- c (desc_data = 9L, loc = 15L, stats = 8L, ext_calls = 4L)
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
        cli::cli_abort (
            "Chaoss metrics data have wrong length; should be {length(index)}."
        )
    }

    ncols <- vapply (data$rm [index], ncol, integer (1L))
    ncols_expected <- c (
        "contribs_from_gh_api" = 17L,
        "contribs_from_log" = 2L,
        "dependencies" = 3L,
        "gh_repo_workflow" = 8L,
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
