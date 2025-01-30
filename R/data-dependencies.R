#' Extract package dependencies from "DESCRIPTION" file.
#'
#' @param path Local path to repository
#' @noRd
rm_data_dependencies <- function (path) {

    desc_path <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    checkmate::assert_file_exists (desc_path)

    desc <- read.dcf (desc_path)

    fields <- c ("Imports", "Suggests", "Remotes", "LinkingTo")

    deps <- lapply (fields, function (f) {
        if (f %in% colnames (desc)) {
            name <- gsub ("\\n", "", strsplit (desc [, f], ",") [[1]])
            version <- vapply (name, function (i) {
                vers <- regmatches (i, regexpr ("\\s\\(.*$", i))
                null2na_char (gsub ("^\\s|\\(|\\)", "", vers))
            }, character (1L), USE.NAMES = FALSE)
            name <- gsub ("\\s*", "", name)
            name <- gsub ("\\(.*$", "", name)
            n <- length (name)

            cbind (name, type = rep (f, n), version)
        }
    })

    data.frame (do.call (rbind, deps))
}

#' Reverse or downstream dependencies from CRAN db.
#'
#' Note that this all works even for packages which aren't on CRAN.
#' @noRd
rm_data_dependencies_downstream <- function (path) { # nolint

    cran_db <- cran_pkg_db ()
    pkg_name <- pkg_name_from_path (path)
    i <- which (cran_db$Package == pkg_name)
    revdep_cols <- grep ("reverse", names (cran_db), ignore.case = TRUE)
    revdeps <- unname (do.call (c, cran_db [i, revdep_cols]))
    revdeps <-
        unlist (lapply (revdeps, function (i) strsplit (i, ",\\s*") [[1]]))
    revdeps <- revdeps [which (!is.na (revdeps))]
    if (is.null (revdeps)) {
        revdeps <- character (0L)
    }

    return (revdeps)
}

#' Data for CHAOSS "libyears" metric
#'
#' This is assessed against date of latest release from both GitHub and CRAN.
#'
#' @param path Local path to repository
#' @noRd
rm_data_libyears <- function (path) {

    deps <- rm_data_dependencies (path)
    cran_db <- data.frame (cran_pkg_db ())
    index <- match (deps$name, cran_db$Package)
    deps$cran_version <- cran_db$Version [index]
    deps$published <- as.Date (cran_db$Published [index])
    deps <- deps [which (!is.na (deps$published)), ]

    rel_gh <- rel_cran <- NA_real_
    rel <- rm_data_releases_from_gh_api (path, latest_only = TRUE)
    if (nrow (rel) > 0L) {
        rel_gh <- rel$published_at
    }
    pkg_in_db <- match (pkg_name_from_path (path), cran_db$Package)
    if (length (pkg_in_db) > 0) {
        rel_cran <- cran_db$Date.Publication [pkg_in_db [1]]
    }
    rel_date <- c (rel_gh, rel_cran)
    rel_date <- as.Date (strftime (rel_date, format = "%Y-%m-%d"))
    if (all (is.na (rel_date))) {
        rel_date <- rel_date [1]
    } else {
        rel_date <- max (rel_date, na.rm = TRUE)
    }

    dt <- NA_real_
    if (!is.na (rel_date)) {
        dt <- difftime (deps$published, rel_date, units = "days")
    }
    if (nrow (deps) == 0L) {
        dt <- numeric (0L)
    }
    deps$libyears <- as.numeric (dt) / 365.25 # In years

    return (deps)

}

cran_pkg_db <- memoise::memoise (tools::CRAN_package_db)
