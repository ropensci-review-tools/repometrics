test_that ("author matches", { # R/cm-metric-cran-downloads.R

    dat <- mock_cm_data ()

    ctbs_gh <- dat$contribs_from_gh_api
    ctbs_log <- dat$contribs_from_log

    expect_equal (ncol (ctbs_log), 2L)
    expect_equal (names (ctbs_log), c ("handle", "email"))

    ctbs_log <- get_all_contribs (ctbs_log, ctbs_gh)
    expect_equal (ncol (ctbs_log), 3L)
    expect_equal (names (ctbs_log), c ("name", "email", "gh_handle"))
})
