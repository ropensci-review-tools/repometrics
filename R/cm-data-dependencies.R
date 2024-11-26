#' Extract package dependencies from "DESCRIPTION" file.
#'
#' @param path Local path to repository
#' @param n_per_page Not used here, but needed so all functions can safely be
#' called with this parameter.
#' @noRd
cm_data_dependencies <- function (path, n_per_page = 30L) {

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

#' Extract CHAOSS "libyears" metric
#'
#' @param path Local path to repository
#' @param n_per_page Not used here, but needed so all functions can safely be
#' called with this parameter.
#' @noRd
cm_data_libyears <- function (path, n_per_page = 30L) {

    deps <- cm_data_dependencies (path)
    cran_db <- data.frame (cran_pkg_db ())
    index <- match (deps$name, cran_db$Package)
    deps$cran_version <- cran_db$Version [index]
    deps$published <- as.Date (cran_db$Published [index])
    deps <- deps [which (!is.na (deps$published)), ]

    rel <- cm_data_releases_from_gh_api (path, latest_only = TRUE)
    rel_date <- as.Date (strftime (rel$published_at, format = "%Y-%m-%d"))

    dt <- difftime (deps$published, rel_date, units = "days")
    dt <- as.numeric (dt) / 365.25 # In years

    c (mean = mean (dt), median = stats::median (dt))
}

cran_pkg_db <- memoise::memoise (tools::CRAN_package_db)
