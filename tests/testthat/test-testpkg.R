test_that ("testpkg and input errors", {
    expect_error (githist (1))
    expect_error (githist ("a"))

    pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    expect_error (githist (path, step_size = 1:2))
    expect_error (githist (path, step_size = "1"))

    expect_output (
        res <- githist (path)
    )

    expect_type (res, "list")
    expect_length (res, 3L)
    expect_identical (names (res), c ("desc_data", "loc", "stats"))
    expect_true (all (vapply (res, nrow, integer (1L)) > 1L))
})

test_that ("githist parameters", {
    pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    res0 <- githist (path)
    n0 <- vapply (res0, nrow, integer (1L))
    fs::dir_delete (path)

    flist <- unzip (pkg, exdir = fs::path_temp ())
    res1 <- githist (path, n = 2L)
    n1 <- vapply (res1, nrow, integer (1L))
    fs::dir_delete (path)

    flist <- unzip (pkg, exdir = fs::path_temp ())
    res2 <- githist (path, n = 2L, step_size = 2L)
    n2 <- vapply (res2, nrow, integer (1L))
    fs::dir_delete (path)

    expect_true (all (n0 > n1))
    expect_true (all (n0 > n2))
    expect_true (nrow (res0$desc_data) > 2L)
    expect_equal (nrow (res1$desc_data), 2L)
    expect_equal (nrow (res2$desc_data), 2L)

    d1 <- abs (diff (res1$desc_data$date, units = "s"))
    d2 <- abs (diff (res2$desc_data$date, units = "s"))
    expect_true (mean (d1) < mean (d2))
})
