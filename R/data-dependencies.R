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
            name <- gsub ("(\\s|\\().*$", "", name)
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
rm_data_dependencies_downstream <- function (path) {

    cran_db <- cran_pkg_db ()
    pkg_name <- pkg_name_from_path (path)
    i <- which (cran_db$Package == pkg_name)
    revdep_cols <- grep ("reverse", names (cran_db), ignore.case = TRUE)
    revdeps <- unname (do.call (c, cran_db [i, revdep_cols]))
    revdeps <- unlist (lapply (revdeps, function (i) strsplit (i, ",\\s*") [[1]]))
    revdeps <- revdeps [which (!is.na (revdeps))]
    if (is.null (revdeps)) {
        revdeps <- character (0L)
    }

    return (revdeps)
}

#' Data for CHAOSS "libyears" metric
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

    rel <- rm_data_releases_from_gh_api (path, latest_only = TRUE)
    dt <- NA_real_
    if (nrow (rel) > 0L) {
        rel_date <- as.Date (strftime (rel$published_at, format = "%Y-%m-%d"))
        dt <- difftime (deps$published, rel_date, units = "days")

    }
    deps$libyears <- as.numeric (dt) / 365.25 # In years

    return (deps)

}

cran_pkg_db <- memoise::memoise (tools::CRAN_package_db)
