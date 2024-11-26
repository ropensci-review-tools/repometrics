test_that ("multicore", {

    path <- generate_test_pkg ()
    res1 <- repo_pkgstats_history (path, num_cores = 1L)
    # Becuase of hard reset of history, entire pkg dir needs to be deleted and
    # re-created:
    fs::dir_delete (path)

    path <- generate_test_pkg ()
    res2 <- repo_pkgstats_history (path, num_cores = 2L)
    fs::dir_delete (path)

    expect_identical (res1, res2)
})
