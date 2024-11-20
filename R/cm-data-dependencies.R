cm_data_dependencies <- function (path) {

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
