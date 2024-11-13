test_that ("chaoss internal num_commits", {
    pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    n <- chaoss_internal_num_commits (path, end_date = as.Date ("2024-08-01"))
    expect_equal (n, 4L)

    fs::dir_delete (path)
})
