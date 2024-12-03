test_that ("author matches", { # R/cm-metric-cran-downloads.R

    dat <- mock_cm_data ()

    ctbs_gh <- dat$contribs_from_gh_api
    ctbs_log <- dat$contribs_from_log

    expect_equal (ncol (ctbs_log), 2L)
    expect_equal (names (ctbs_log), c ("handle", "email"))
    expect_equal (nrow (ctbs_log), 1L)
    expect_true ("mpadge" %in% ctbs_log$handle)
    expect_false ("hfrick" %in% ctbs_log$handle)

    # Fake extend log data
    ctbs_log <- rbind (
        ctbs_log,
        data.frame (handle = "hannafrick", email = "hanna@here")
    )

    # Fake extend gh data:
    n <- nrow (ctbs_gh)
    ctbs_gh <- ctbs_gh [c (1:n, n), ]
    ctbs_gh [n + 1, ] <- NA
    ctbs_gh$login [n + 1] <- "mpadge"

    ctbs_log <- get_all_contribs (ctbs_log, ctbs_gh)
    expect_equal (ncol (ctbs_log), 3L)
    expect_equal (names (ctbs_log), c ("name", "email", "gh_handle"))
    expect_equal (nrow (ctbs_log), 2L)
    expect_true ("mpadge" %in% ctbs_log$gh_handle)
    expect_true ("hfrick" %in% ctbs_log$gh_handle)
})
