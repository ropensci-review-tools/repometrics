test_that ("testpkg and input errors", {
    expect_error (repo_pkgstats_history (1))
    expect_error (repo_pkgstats_history ("a"))

    path <- generate_test_pkg ()

    expect_error (repo_pkgstats_history (path, date_interval = 1:2, num_cores = 1L))
    expect_error (repo_pkgstats_history (path, date_interval = "1", num_cores = 1L))

    res <- repo_pkgstats_history (path, num_cores = 1L, date_interval = "day")
    fs::dir_delete (path)

    expect_type (res, "list")
    expect_length (res, 4L)
    expect_identical (names (res), c ("desc_data", "loc", "stats", "ext_calls"))
    expect_true (all (vapply (res, nrow, integer (1L)) > 0L))
})

test_that ("repo_pkgstats_history parameters", {

    path <- generate_test_pkg ()
    res0 <- repo_pkgstats_history (path, date_interval = "day", num_cores = 1L)
    fs::dir_delete (path)

    n0 <- vapply (res0, nrow, integer (1L))

    path <- generate_test_pkg ()
    res1 <- repo_pkgstats_history (path, date_interval = "week", num_cores = 1L)
    fs::dir_delete (path)

    n1 <- vapply (res1, nrow, integer (1L))
    expect_true (all (n0 == n1))
})
