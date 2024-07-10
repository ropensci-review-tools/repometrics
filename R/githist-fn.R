#' Apply \pkg{pkgstats} across the git history of a package
#'
#' @param path Path to local repository containing an R package.
#' @export
githist <- function (path) {
    checkmate::assert_character (path, len = 1L)
    checkmate::assert_directory (path)

    path_cp <- fs::dir_copy (path, fs::path_temp ())

    h <- gert::git_log (repo = path_cp, max = 1e6)

    res <- pbapply::pblapply (seq_len (nrow (h)), function (i) {
        g <- gert::git_reset_hard (ref = h$commit [i], repo = path_cp)
        pkg_date <- h$time [i]
        run_one_pkgstats (path = path_cp, pkg_date = pkg_date)
    })

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
    desc_data$date <- lapply (x, function (i) strftime (i$date, "%y-%m-%d %H:%M:%S"))

    nms_int <- nms2df [-seq_len (which (nms2df == "date"))]
    for (n in nms_int) {
        desc_data [[n]] <- as.integer (desc_data [[n]])
    }

    loc <- do.call (rbind, lapply (x, function (i) i$loc))
    stats <- do.call (rbind, lapply (x, function (i) i$stats))
    stats$measure <- gsub ("[0-9]+$", "", rownames (stats))
    rownames (stats) <- NULL

    list (
        desc_data = desc_data,
        loc = loc,
        stats = stats
    )
}
