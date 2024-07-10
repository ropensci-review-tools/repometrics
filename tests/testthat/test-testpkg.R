test_that ("testpkg", {
    pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    res <- githist (path)

    expect_type (res, "list")
    expect_length (res, 3L)
    expect_identical (names (res), c ("desc_data", "loc", "stats"))
    expect_true (all (vapply (res, nrow, integer (1L)) > 1L))
})
