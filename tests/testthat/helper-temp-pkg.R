generate_test_pkg <- function (add_url = TRUE) {

    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    if (add_url) {
        desc_path <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
        url <- "https://github.com/ropensci-review-tools/goodpractice"
        desc <- c (
            readLines (desc_path),
            paste0 ("URL: ", url)
        )
        writeLines (desc, desc_path)
    }

    return (path)
}
