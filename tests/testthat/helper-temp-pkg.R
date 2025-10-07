generate_test_pkg <- function () {

    on_r_univ <- nzchar (Sys.getenv ("MY_UNIVERSE"))

    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    pkg_dest <- fs::path (fs::path_temp (), "testpkg")
    if (fs::dir_exists (pkg_dest)) {
        if (on_r_univ) {
            # For some reason, directories are not properly cleaned up on
            # r-univ machines, so this fn always fails.
            fs::dir_delete (pkg_dest)
        } else {
            stop ("test package already exists", call. = FALSE)
        }
    }
    flist <- unzip (pkg, exdir = fs::path_temp ())

    # Make README file with fake badge for test coverage test:
    readme <- c (
        "",
        paste0 (
            "[![codecov]",
            "(https://codecov.io/gh/ropensci-review-tools/repometrics/",
            "branch/main/graph/badge.svg)]",
            "(https://app.codecov.io/gh/ropensci-review-tools/repometrics)"
        ),
        "",
        "# repometrics",
        ""
    )
    f <- fs::path (pkg_dest, "README.md")
    writeLines (readme, f)

    return (fs::path_common (flist))
}
