#' Apply \pkg{pkgstats} across the git history of a package
#'
#' @param path Path to local repository containing an R package.
#' @param n If given, only analyses the preceding 'n' commits in the git
#' history.
#' @param step_size Analyse package for each `step_size` commits. For example,
#' with `step_size = 2`, analyses will be run on every second commit, instead
#' of default running on every commit. The value of `n` then analyses the
#' first `n` entries taken at intervals of `step_size`.
#' @param num_cores Number of cores to use in multi-core processing. Has no
#' effect on Windows operating systems, on which calculations are always
#' single-core only. Negative values are subtracted from number of available
#' cores, determined as `parallel::detectCores()`, so default of `num_cores =
#' -1L` uses `detectCores() - 1L`. Positive values use precisely that number,
#' restricted to maximum available cores, and a value of zero will use all
#' available cores.
#'
#' @return A list of three items:
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
githist <- function (path, n = NULL, step_size = 1L, num_cores = -1L) {
    checkmate::assert_character (path, len = 1L)
    checkmate::assert_directory (path)
    checkmate::assert_int (step_size, lower = 1L)
    if (!is.null (n)) {
        checkmate::assert_int (n, lower = 1L)
    }
    checkmate::assert_int (num_cores)

    num_cores <- set_num_cores (num_cores)

    h <- gert::git_log (repo = path, max = 1e6)
    if (step_size > 1L) {
        h <- h [seq (1, nrow (h), by = step_size), ]
    }
    if (!is.null (n)) {
        h <- h [seq_len (n), ]
    }

    if (num_cores == 1L) {

        res <- extract_pkgstats_data_single (h, path)

    } else {

        res <- extract_pkgstats_data_multi (h, path, num_cores)

    }

    collate_pkgstats (res)
}

extract_pkgstats_data_single <- function (log, path) {

    path_cp <- fs::path (fs::path_temp (), basename (path))
    clean_after <- FALSE
    if (fs::path (fs::path_dir (path)) != fs::path_temp () &&
        !fs::dir_exists (path_cp)) {
        path_cp <- fs::dir_copy (path, fs::path_temp ())
        clean_after <- TRUE
    }

    res <- pbapply::pblapply (seq_len (nrow (log)), function (i) {
        g <- gert::git_reset_hard (ref = log$commit [i], repo = path_cp)
        run_one_pkgstats (path = path_cp, pkg_date = log$time [i])
    })

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
    res <- pbapply::pblapply (seq_len (nrow (log)), function (i) {
        path_cp <- fs::dir_copy (path, fs::path_temp ())
        g <- gert::git_reset_hard (ref = log$commit [i], repo = path_cp)
        s <- run_one_pkgstats (path = path_cp, pkg_date = log$time [i])
        fs::dir_delete (path_cp)
        return (s)
    }, cl = cl)
    parallel::stopCluster (cl)

    return (res)

    return (res)
}

collate_pkgstats <- function (x) {
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
