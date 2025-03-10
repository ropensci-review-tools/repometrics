generate_test_pkg <- function () {

    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    pkg_dest <- fs::path (fs::path_temp (), "testpkg")
    if (fs::dir_exists (pkg_dest)) {
        stop ("test package already exists", call. = FALSE)
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
