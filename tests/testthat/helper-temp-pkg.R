generate_test_pkg <- function () {

    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    pkg_dest <- fs::path (fs::path_temp (), "testpkg")
    if (fs::dir_exists (pkg_dest)) {
        stop ("test package already exists", call. = FALSE)
    }
    flist <- unzip (pkg, exdir = fs::path_temp ())
    return (fs::path_common (flist))
}
