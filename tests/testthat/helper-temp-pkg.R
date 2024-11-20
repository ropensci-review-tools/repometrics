generate_test_pkg <- function () {

    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    return (fs::path_common (flist))
}
