
test_that ("dashboard and input errors", {

    pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    res0 <- res <- githist (path)

    res$desc_data <- NULL
    expect_error (
        ghist_dashboard (res, action = "build"),
        "Assertion on 'results' failed: Must have length 3"
    )

    res <- res0
    names (res) [1] <- "desc"
    expect_error (
        ghist_dashboard (res, action = "build"),
        "Assertion on 'names\\(results\\)' failed\\: Names must be a identical to set"
    )

    res <- res0
    expect_error (
        ghist_dashboard (res, action = "build"),
        "'arg' should be one of"
    )

    res$stats <- res$stats [, -1]
    expect_error (
        ghist_dashboard (res, action = "render"),
        "'results' has wrong number of columns"
    )
    res <- res0
    res$stats <- res$stats [-seq_len (nrow (res$stats)), ]
    expect_error (
        ghist_dashboard (res, action = "render"),
        "'results' contains empty tables."
    )
})
