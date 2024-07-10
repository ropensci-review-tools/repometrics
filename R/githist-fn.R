#' Apply \pkg{pkgstats} across the git history of a package
#'
#' @param path Path to local repository containing an R package.
#' @param n If given, only analyses the preceding 'n' commits in the git
#' history.
#' @export
githist <- function (path, n = NULL) {
    checkmate::assert_character (path, len = 1L)
    checkmate::assert_directory (path)
    if (!is.null (n)) {
        checkmate::assert_int (n, lower = 1L)
    }

    path_cp <- path
    clean_after <- FALSE
    if (fs::path_dir (path) != fs::path_temp ()) {
        path_cp <- fs::dir_copy (path, fs::path_temp ())
        clean_after <- TRUE
    }

    h <- gert::git_log (repo = path_cp, max = 1e6)
    if (!is.null (n)) {
        h <- h [seq_len (n), ]
    }

    res <- pbapply::pblapply (seq_len (nrow (h)), function (i) {
        g <- gert::git_reset_hard (ref = h$commit [i], repo = path_cp)
        pkg_date <- h$time [i]
        run_one_pkgstats (path = path_cp, pkg_date = pkg_date)
    })

    if (clean_after) {
        fs::dir_delete (path_cp)
    }

    collate_pkgstats (res)
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
