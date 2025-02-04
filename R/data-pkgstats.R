repo_pkgstats_history_internal <- function (path, step_days = 1L, num_cores = -1L) {

    checkmate::assert_character (path, len = 1L)
    checkmate::assert_directory (path)
    checkmate::assert_int (step_days, lower = 0L)
    checkmate::assert_int (num_cores)

    num_cores <- set_num_cores (num_cores)

    log <- rm_data_gitlog (path)
    log <- filter_git_log (log, step_days)

    if (num_cores == 1L) {

        res <- extract_pkgstats_data_single (log, path)

    } else {

        res <- extract_pkgstats_data_multi (log, path, num_cores)

    }

    collate_pkgstats (res)
}

#' Apply \pkg{pkgstats} across the git history of a package
#'
#' @param path Path to local repository containing an R package.
#' @param step_days Analyse package at intervals of this number of days. The
#' last commit for each day is chosen. For example, `step_days = 7L` will
#' return weekly statistics. Values of zero or less will analyse all commits,
#' including potentially multiple daily commits.
#' @param num_cores Number of cores to use in multi-core processing. Has no
#' effect on Windows operating systems, on which calculations are always
#' single-core only. Negative values are subtracted from number of available
#' cores, determined as `parallel::detectCores()`, so default of `num_cores =
#' -1L` uses `detectCores() - 1L`. Positive values use precisely that number,
#' restricted to maximum available cores, and a value of zero will use all
#' available cores.
#'
#' @return `NULL` if `path` is not an R package, or if no \pkg{pkgstats}
#' results are able to be extracted. Otherwise, a list of three items:
#' \itemize{
#' \item desc_data Containing data from `DESCRIPTION` files, along with data on
#' numbers of functions.
#' \item loc Containing data on "lines-of-code" for all languages and
#' sub-directories within package.
#' \item stats Containing statistics on (mean, medium, and sum) of various
#' properties of each function in package.
#' }
#'
#' @export
repo_pkgstats_history <- memoise::memoise (repo_pkgstats_history_internal)

filter_git_log <- function (log, step_days) {

    if (step_days >= 1L) {
        log$date <- as.Date (log$timestamp)
        log <- dplyr::group_by (log, date) |>
            dplyr::filter (dplyr::row_number () == 1L)
        if (step_days > 1L) {
            index <- which (-diff (log$date) < step_days)
            if (length (index) > 0L) {
                log <- log [-(index), ]
            }
        }
    }

    return (log)
}


extract_pkgstats_data_single <- function (log, path) {

    path_cp <- fs::path (fs::path_temp (), basename (path))
    clean_after <- FALSE
    if (fs::path (fs::path_dir (path)) != fs::path_temp () &&
        !fs::dir_exists (path_cp)) {
        path_cp <- fs::dir_copy (path, fs::path_temp ())
        clean_after <- TRUE
    }

    if (is_verbose ()) {
        res <- pbapply::pblapply (seq_len (nrow (log)), function (i) {
            flist <- reset_repo (path_cp, log$hash [i]) # nolint
            run_one_pkgstats (path = path_cp, pkg_date = log$timestamp [i])
        })
    } else {
        res <- lapply (seq_len (nrow (log)), function (i) {
            flist <- reset_repo (path_cp, log$hash [i]) # nolint
            run_one_pkgstats (path = path_cp, pkg_date = log$timestamp [i])
        })
    }

    if (clean_after) {
        fs::dir_delete (path_cp)
    }

    return (res)
}

extract_pkgstats_data_multi <- function (log, path, num_cores) {

    cl <- parallel::makeCluster (num_cores)
    parallel::clusterExport (
        cl,
        c ("log", "path", "run_one_pkgstats"),
        envir = environment ()
    )
    if (!is_verbose ()) {
        opb <- pbapply::pboptions (type = "none")
    }
    res <- pbapply::pblapply (seq_len (nrow (log)), function (i) {
        path_cp <- fs::dir_copy (path, fs::path_temp ())
        flist <- reset_repo (path_cp, log$hash [i]) # nolint
        s <- run_one_pkgstats (path = path_cp, pkg_date = log$timestamp [i])
        fs::dir_delete (path_cp)
        return (s)
    }, cl = cl)
    parallel::stopCluster (cl)

    if (!is_verbose ()) {
        pbapply::pboptions (opb)
    }

    return (res)

    return (res)
}

reset_repo <- function (path, hash) {

    g <- gert::git_reset_hard (ref = hash, repo = path) # nolint
    flist <- fs::dir_ls (path, recurse = TRUE, type = "file")
    # Reduce to paths relative to 'path' itself:
    flist <- fs::path_rel (flist, path)
    flist_git <- gert::git_ls (path)
    flist_out <- flist [which (!flist %in% flist_git$path)]
    if (length (flist_out) > 0L) {
        tryCatch (
            fs::file_delete (flist_out),
            error = function (e) NULL
        )
    }

    return (fs::dir_ls (path, recurse = TRUE))
}

run_one_pkgstats <- function (path, pkg_date) {

    s <- tryCatch (
        pkgstats::pkgstats (path),
        error = function (e) NULL
    )
    if (is.null (s)) {
        return (s)
    }

    index <- which (
        s$objects$kind == "function" &
            !grepl ("^anonFunc", s$objects$fn_name) &
            !is.na (s$objects$npars)
    )
    # Empty results do not contain all columns:
    nms <- c (
        "fn_name", "language", "loc", "npars",
        "has_dots", "exported", "num_doclines"
    )

    nms <- nms [which (nms %in% names (s$objects))]
    fns <- s$objects [index, ] |> dplyr::select (dplyr::all_of (nms))
    doclines <-
        mn_med_sum (fns$num_doclines [which (!is.na (fns$num_doclines))])
    npars <- mn_med_sum (fns$npars)
    loc <- mn_med_sum (fns$loc)

    fn_nms <- data.frame (exported = logical (0L))
    if (nrow (fns) > 0L) {
        fn_nms <- unique (fns [, c ("fn_name", "exported")])
    }

    package <- ext_calls <- NULL # Suppress 'no visible binding' note.
    if (!is.null (s$external_calls)) {
        ext_calls <- s$external_calls |>
            dplyr::select ("call", "package") |>
            dplyr::group_by (package) |>
            dplyr::count (package) |>
            dplyr::filter (package != s$desc$package)
    }

    base_calls <- null2na_int (ext_calls$n [ext_calls$package == "base"])
    n_ext_pkgs <- null2na_int (nrow (ext_calls)) - 1L
    ext_calls <- mn_med_sum (ext_calls$n [ext_calls$package != "base"])

    s$loc <- cbind (
        package = s$desc$package,
        version = s$desc$version,
        date = pkg_date,
        s$loc
    )

    list (
        package = s$desc$package,
        version = s$desc$version,
        date = pkg_date,
        n_aut = s$desc$aut,
        n_ctb = s$desc$ctb,
        n_fns_tot = nrow (fn_nms),
        n_fns_exp = length (which (fn_nms$exported)),
        n_ext_pkgs = n_ext_pkgs,
        base_calls = base_calls,
        loc = s$loc,
        stats = data.frame (
            package = s$desc$package,
            version = s$desc$version,
            date = pkg_date,
            doclines = doclines,
            npars = npars,
            loc = loc,
            ext_calls = ext_calls
        )
    )
}

collate_pkgstats <- function (x) {

    index <- which (!vapply (x, is.null, logical (1L)))
    if (length (index) > 0L) {
        x <- x [index]
    } else {
        return (NULL)
    }

    nms <- names (x [[1]])
    nms2df <- nms [seq_len (which (nms == "loc") - 1L)]
    desc_data <- lapply (nms2df, function (i) {
        unlist (lapply (x, function (j) j [[i]]))
    })
    desc_data <- data.frame (do.call (cbind, desc_data))
    names (desc_data) <- nms2df
    desc_data$date <- vapply (
        x,
        function (i) strftime (i$date, "%y-%m-%d %H:%M:%S"),
        "character"
    )
    desc_data$date <- strptime (desc_data$date, format = "%y-%m-%d %H:%M:%S")

    nms_int <- nms2df [-seq_len (which (nms2df == "date"))]
    for (n in nms_int) {
        desc_data [[n]] <- as.integer (desc_data [[n]])
    }

    loc <- do.call (rbind, lapply (x, function (i) i$loc))
    stats <- do.call (rbind, lapply (x, function (i) i$stats))
    stats$measure <- gsub ("[0-9]+$", "", rownames (stats))
    rownames (stats) <- NULL

    # Lazy convert all to tibbles, which `res$loc` is from `dplyr`:
    class (desc_data) <- class (stats) <- class (loc)

    list (
        desc_data = desc_data,
        loc = loc,
        stats = stats
    )
}
