test_that ("testpkg and input errors", {
    expect_error (githist (1))
    expect_error (githist ("a"))

    pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    expect_error (githist (path, step_size = 1:2, num_cores = 1L))
    expect_error (githist (path, step_size = "1", num_cores = 1L))

    res <- githist (path, num_cores = 1L, step_days = 0L)
    fs::dir_delete (path)

    expect_type (res, "list")
    expect_length (res, 3L)
    expect_identical (names (res), c ("desc_data", "loc", "stats"))
    expect_true (all (vapply (res, nrow, integer (1L)) > 1L))
})

test_that ("githist parameters", {
    pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    res0 <- githist (path, step_days = 0L, num_cores = 1L)
    fs::dir_delete (path)

    flist <- unzip (pkg, exdir = fs::path_temp ())
    res1 <- githist (path, n = 2L, step_days = 0L, num_cores = 1L)
    fs::dir_delete (path)

    n0 <- vapply (res0, nrow, integer (1L))
    n1 <- vapply (res1, nrow, integer (1L))
    expect_true (all (n0 > n1))
    # `n = 2L` selects dates [1:2] from original data:
    expect_identical (res0$desc_data$date [1:2], res1$desc_data$date)

    flist <- unzip (pkg, exdir = fs::path_temp ())
    res2 <- githist (path, n = 2L, step_days = 1L, num_cores = 1L)
    fs::dir_delete (path)

    n2 <- vapply (res2, nrow, integer (1L))
    expect_true (all (n0 > n2))
    expect_true (all (n1 > n2))
    expect_equal (
        length (res2$desc_dat$date),
        length (unique (res2$desc_data$date))
    )

    # Finally, test step_days > 1, which has no effect anyway, as all commits
    # are on same day
    flist <- unzip (pkg, exdir = fs::path_temp ())
    res3 <- githist (path, n = 2L, step_days = 2L, num_cores = 1L)
    fs::dir_delete (path)

    n3 <- vapply (res3, nrow, integer (1L))
    expect_equal (n2, n3)
})
