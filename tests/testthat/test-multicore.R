test_that ("multicore", {
    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    res1 <- githist (path, num_cores = 1L)
    # Becuase of hard reset of history, entire pkg dir needs to be deleted and
    # re-created:
    fs::dir_delete (path)

    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])
    res2 <- githist (path, num_cores = 2L)
    fs::dir_delete (path)

    expect_identical (res1, res2)
})
