end_date <- as.Date ("2024-08-01")

test_that ("cm metric cran_downloads", { # R/cm-metric-cran-downloads.R

    mock_cm_data ()

    path <- generate_test_pkg () # has URL of "goodpractice"
    desc_path <- fs::path (path, "DESCRIPTION")
    desc <- readLines (desc_path)
    desc [1] <- "Package: goodpractice"
    writeLines (desc, desc_path)

    end_date <- as.Date ("2024-01-01")
    dl <- cm_metric_cran_downloads (path = path, end_date = end_date)
    expect_type (dl, "integer")
    expect_length (dl, 1L)
    expect_equal (dl, 2308)

    fs::dir_delete (path)
})

test_that ("cm metric has_ci", { # R/cm-metric-has-ci.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    path <- generate_test_pkg ()

    chk <- repo_has_ci_files (path)
    expect_length (chk, 0L)

    d <- fs::dir_create (fs::path (path, ".github", "workflows"))
    f <- fs::path (d, "workflow.yaml")
    writeLines ("a", f)

    chk <- repo_has_ci_files (path)
    expect_length (chk, 1L)
    expect_equal (chk, "github")

    # Test cli::cli_alert_warning output:
    expect_snapshot (chk <- cm_metric_has_ci (path))
    expect_true (chk)

    fs::dir_delete (path)
})

test_that ("cm metric has CI internal", { # R/cm-metric-has-ci.R

    path <- generate_test_pkg ()
    has_ci <- repo_has_ci_files (path)
    # R-universe includes on CI file, so this test fails there:
    # https://github.com/r-universe/workflows/blob/v1/.github/workflows/build.yml#L11
    on_r_univ <- nzchar (Sys.getenv ("MY_UNIVERSE"))
    if (!on_r_univ) {
        expect_length (has_ci, 0L) # No CI files
    }

    url <- "https://github.com/ropensci-review-tools/goodpractice"
    expect_identical (url, pkg_gh_url_from_path (path))
    org_repo <- org_repo_from_path (path)
    expect_identical (org_repo, c ("ropensci-review-tools", "goodpractice"))

    desc_path <- fs::dir_ls (path, regexp = "DESCRIPTION$", type = "file")
    desc <- readLines (desc_path)
    i <- grep ("^URL", desc)
    desc [i] <- "URL: https://ropensci-review-tools.github.io"
    writeLines (desc, desc_path)

    expect_equal (pkg_gh_url_from_path (path), character (0L))
    expect_false (org_repo_from_path (path))

    writeLines (desc [-i], desc_path)
    expect_null (pkg_gh_url_from_path (path))
    expect_false (org_repo_from_path (path))

    fs::dir_delete (path)
})

test_that ("cm metrics num_commits num_contribs", {
    # R/cm-metrics-num-ctb.R and R/cm-metrics-num-commits.R

    path <- generate_test_pkg ()

    n <- cm_metric_num_commits (path, end_date = end_date)
    expect_equal (n, 4L)

    n <- cm_metric_num_contributors (path, end_date = end_date)
    expect_equal (n, 1L)

    fs::dir_delete (path)
})

test_that ("cm metric change req frequency", { # R/cm-metrics-change-req.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    dat <- cm_metric_change_req (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (dat, "double")
    expect_length (dat, 1L)
    expect_equal (dat, 0.)
})

test_that ("cm metric issues-to-prs", { # R/cm-metric-issues-to-prs.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    x <- cm_metric_issues_to_prs (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (x, "double")
    expect_length (x, 1L)
    expect_true (x > 0)
})

test_that ("cm metric pr-reviews", { # R/cm-metric-pr-review.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    revs <- cm_metric_pr_reviews (path, end_date = end_date)
    fs::dir_delete (path)

    expect_s3_class (revs, "data.frame")
    expect_equal (nrow (revs), 1L)
    expect_equal (ncol (revs), 12L)
    nms <- c (
        "approved_ratio", "rejected_ratio", "approval_duration",
        "n_comments_per_approved", "n_comments_per_rejected",
        "n_comments_per_other", "n_commenters_per_approved",
        "n_commenters_per_rejected", "n_commenters_per_other",
        "n_iterations_per_approved", "n_iterations_per_rejected",
        "n_iterations_per_other"
    )
    expect_equal (names (revs), nms)
})

test_that ("cm metric num forks", { # R/cm-metrics-num-forks.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    forks <- cm_metric_num_forks (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (forks, "integer")
    expect_length (forks, 2L)
    expect_named (forks)
    expect_equal (names (forks), c ("num_in_period", "num_total"))
    expect_true (forks [["num_total"]] > 0)
})

test_that ("cm metric code change lines", { # R/cm-metrics-code-change.R

    path <- generate_test_pkg ()
    x1 <- cm_metric_code_change_lines (path, end_date = end_date)
    x2 <- cm_metric_code_change_lines (
        path,
        end_date = end_date,
        exclude_whitespace = FALSE
    )
    fs::dir_delete (path)

    for (i in c (x1, x2)) {
        expect_type (i, "integer")
        expect_length (i, 1L)
        expect_true (i > 0)
    }
    expect_true (x2 > x1)
})

test_that ("cm metric review duration", { # R/cm-metrics-pr-reviews.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    revs <- cm_metric_pr_review_duration (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (revs, "double")
    expect_length (revs, 4L)
    expect_named (revs)
    nms <- c ("cycle_dur_mn", "cycle_dur_md", "review_dur_mn", "review_dur_md")
    expect_equal (names (revs), nms)
})

test_that ("cm metric issue response time", { # R/cm-metrics-issue-response.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    res <- cm_metric_issue_response_time (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 2L)
    expect_named (res)
    nms <- c ("mean", "median")
    expect_equal (names (res), nms)
})

test_that ("cm metric defect resolution duration", {
    # R/cm-metrics-issue-response.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    res <- cm_metric_defect_resolution_dur (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 2L)
    expect_named (res)
    nms <- c ("mean", "median")
    expect_equal (names (res), nms)
})

test_that ("cm metric label inclusivity", { # R/cm-metric-labels.R

    end_date <- as.Date ("2024-12-01")
    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    res <- cm_metric_label_inclusivity (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 3L)
    expect_named (res)
    nms <- c (
        "prop_labelled", "prop_labelled_friendly", "prop_friendly_overall"
    )
    expect_equal (names (res), nms)
})

test_that ("cm metric time to close", { # R/cm-metrics-issue-response.R

    end_date <- as.Date ("2024-12-01")
    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    res <- cm_metric_time_to_close (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 2L)
    expect_named (res)
    nms <- c ("mean", "median")
    expect_equal (names (res), nms)
})

test_that ("cm metric closure ratio", { # R/cm-metrics-issue-response.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    # Test prs only grab first two which are years old
    op <- getOption ("repometrics_period")
    options ("repometrics_period" = 10000)

    res <- cm_metric_pr_closure_ratio (path, end_date = end_date)

    options ("repometrics_period" = op)
    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 1L)
    expect_true (res >= 0)
})

test_that ("cm metric popularity", { # R/cm-metric-popularity.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()

    res <- cm_metric_popularity (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (res, "integer")
    expect_length (res, 4L)
    expect_named (res)
    nms <- c ("revdeps", "contribs", "forks", "stars")
    expect_equal (names (res), nms)
})

test_that ("cm metric libyears", { # R/cm-metric-libyears.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()

    res <- cm_metric_libyears (path)

    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 2L)
    expect_named (res)
    expect_equal (names (res), c ("mean", "median"))
    expect_true (all (res > 0))
})
