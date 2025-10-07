# Two end dates are used here, but these only affect mock results in the
# cran_downloads function. That needs to be hard-coded in 'helper-rm-data.R' to
# use the same value as here
end_date <- as.Date ("2024-08-01")

test_that ("cm metric cran_downloads", { # R/cm-metric-cran-downloads.R

    dat <- mock_rm_data ()

    path <- generate_test_pkg () # has URL of "goodpractice"
    desc_path <- fs::path (path, "DESCRIPTION")
    desc <- readLines (desc_path)
    desc [1] <- "Package: goodpractice"
    writeLines (desc, desc_path)

    dl <- rm_metric_cran_downloads (path = path, end_date = end_date)
    expect_type (dl, "integer")
    expect_length (dl, 1L)
    expect_true (dl > 100)

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
    expect_snapshot (chk <- rm_metric_has_ci (path))
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
    expect_length (pkg_gh_url_from_path (path), 0L)
    expect_false (org_repo_from_path (path))

    fs::dir_delete (path)
})

test_that ("cm metrics num_commits num_contribs", {
    # R/cm-metrics-num-ctb.R and R/cm-metrics-num-commits.R

    path <- generate_test_pkg ()

    n <- rm_metric_num_commits (path, end_date = end_date)
    expect_type (n, "integer")
    expect_length (n, 1L)
    expect_named (n, expected = NULL)
    expect_equal (n, 4L)

    n <- rm_metric_num_contributors (path, end_date = end_date)
    expect_type (n, "integer")
    expect_length (n, 1L)
    expect_named (n, expected = NULL)
    expect_equal (n, 1L)

    n <- rm_metric_commit_count (path, end_date = end_date)
    expect_type (n, "double")
    expect_length (n, 1L)
    expect_named (n, NULL)
    expect_true (n > 0)

    fs::dir_delete (path)
})

test_that ("cm metric change reqests", { # R/cm-metrics-change-req.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    op <- getOption ("repometrics_period")
    options ("repometrics_period" = 10000)

    pr_dat <- rm_data_change_req_internal (path, end_date = end_date)

    expect_type (pr_dat, "double")
    expect_length (pr_dat, 4L)
    expect_named (pr_dat, c ("n_opened", "n_closed", "prop_merged", "prop_code_from_prs"))
    expect_true (all (pr_dat > 0))

    pr_n_opened <- rm_metric_change_req_n_opened (path, end_date = end_date)
    pr_n_closed <- rm_metric_change_req_n_closed (path, end_date = end_date)
    pr_closure_ratio <- rm_metric_change_req_prop_merged (path, end_date = end_date)
    prop_code_from_prs <- rm_metric_change_req_prop_code (path, end_date = end_date)

    options ("repometrics_period" = op)
    fs::dir_delete (path)

    expect_type (pr_n_opened, "integer")
    expect_length (pr_n_opened, 1L)
    expect_true (pr_n_opened > 0)

    expect_type (pr_n_closed, "integer")
    expect_length (pr_n_closed, 1L)
    expect_true (pr_n_closed > 0)

    expect_type (pr_closure_ratio, "double")
    expect_length (pr_closure_ratio, 1L)
    expect_true (pr_closure_ratio > 0)
    expect_true (pr_closure_ratio <= 1)

    expect_type (prop_code_from_prs, "double")
    expect_length (prop_code_from_prs, 1L)
    expect_true (prop_code_from_prs > 0)
})

test_that ("cm metric issues-to-prs", { # R/cm-metric-issues-to-prs.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    x <- rm_metric_issues_to_prs (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (x, "double")
    expect_length (x, 1L)
    expect_true (x > 0)
})

test_that ("cm metric pr-reviews", { # R/cm-metric-pr-review.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    op <- getOption ("repometrics_period")
    options ("repometrics_period" = 10000)

    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    revs <- rm_data_pr_reviews_internal (path, end_date = end_date)
    cmts <- rm_metric_pr_cmt_count (path, end_date = end_date)
    prs_approved <- rm_metric_pr_reviews_approved (path, end_date = end_date)
    prs_rejected <- rm_metric_pr_revs_rejected (path, end_date = end_date)
    age <- rm_metric_pr_age (path, end_date = end_date)

    options ("repometrics_period" = op)
    fs::dir_delete (path)

    expect_s3_class (revs, "data.frame")
    expect_equal (nrow (revs), 1L)
    expect_equal (ncol (revs), 14L)
    nms <- c (
        "approved_count", "rejected_count",
        "approved_ratio", "rejected_ratio", "approval_duration",
        "n_comments_per_approved", "n_comments_per_rejected",
        "n_comments_per_other", "n_commenters_per_approved",
        "n_commenters_per_rejected", "n_commenters_per_other",
        "n_iterations_per_approved", "n_iterations_per_rejected",
        "n_iterations_per_other"
    )
    expect_named (revs, nms)

    expect_type (cmts, "double")
    expect_length (cmts, 4L)
    expect_named (cmts, c ("mean", "sd", "median", "sum"))
    expect_true (all (cmts [which (!is.na (cmts))] >= 0))

    expect_type (age, "double")
    expect_length (age, 1L)
    expect_named (age, NULL)
    expect_true (is.na (age))

    expect_type (prs_approved, "double")
    expect_length (prs_approved, 1L)
    expect_named (prs_approved, NULL)
    expect_type (prs_rejected, "integer")
    expect_length (prs_rejected, 1L)
    expect_named (prs_rejected, NULL)
})

test_that ("cm metric num forks, stars", { # R/cm-metrics-num-forks.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    forks <- rm_metric_num_forks (path, end_date = end_date)
    stars <- rm_metric_num_stars (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (forks, "integer")
    expect_length (forks, 1L)
    expect_named (forks, NULL)
    expect_true (forks >= 0L)
    expect_type (stars, "integer")
    expect_length (stars, 1L)
    expect_named (stars, NULL)
    expect_true (stars >= 0L)
})

test_that ("cm metric code change lines", { # R/cm-metrics-code-change.R

    path <- generate_test_pkg ()
    x1 <- rm_metric_code_change_lines (path, end_date = end_date)
    x2 <- rm_metric_code_change_lines (
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
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    rev_dur_mn <- rm_metric_pr_review_duration (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (rev_dur_mn, "double")
    expect_length (rev_dur_mn, 1L)
    expect_named (rev_dur_mn, NULL)
})

test_that ("cm metric issue numbers, durations, response times", {
    # all in R/cm-metrics-issue-response.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    dur_issues <- rm_metric_issue_response_time (path, end_date = end_date)
    resp_time <- rm_metric_response_time (path, end_date = end_date)
    num_issues <- rm_metric_issues_active (path, end_date = end_date)
    fs::dir_delete (path)

    # Vector of response durations:
    expect_type (dur_issues, "double")
    expect_true (length (dur_issues) >= 0L)

    # Overall summary statistic on repsonse times from both issues and PRs:
    expect_type (resp_time, "double")
    expect_length (resp_time, 4L)
    expect_named (resp_time, c ("mean", "sd", "median", "sum"))

    expect_type (num_issues, "integer")
    expect_length (num_issues, 1L)
    expect_named (num_issues, NULL)
    expect_true (num_issues >= 0L)
})

test_that ("cm metric defect resolution duration", {
    # R/cm-metrics-issue-response.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    res <- rm_metric_defect_resolution_dur (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 1L)
    expect_named (res, NULL)
})

test_that ("cm metric label inclusivity", { # R/cm-metric-labels.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    res <- rm_metric_label_inclusivity (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 1L)
    expect_named (res, NULL)
    expect_true (res >= 0)
})

test_that ("cm metric time to close", { # R/cm-metrics-issue-response.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    res <- rm_metric_time_to_close (path, end_date = end_date)
    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 1L)
    expect_named (res, NULL)
    expect_true (is.na (res))
})

test_that ("cm metric closure ratio", { # R/cm-metrics-issue-response.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()
    # Test prs only grab first two which are years old
    op <- getOption ("repometrics_period")
    options ("repometrics_period" = 10000)

    res <- rm_metric_pr_closure_ratio (path, end_date = end_date)

    options ("repometrics_period" = op)
    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 1L)
    expect_true (res >= 0)
})

test_that ("cm data popularity", { # R/cm-metric-popularity.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    res <- rm_data_popularity_internal (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (res, "integer")
    expect_length (res, 4L)
    expect_named (res, c ("revdeps", "contribs", "forks", "stars"))
})

test_that ("cm metric libyears", { # R/cm-metric-libyears.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    ly <- rm_metric_libyears (path)
    ndeps <- rm_metric_dependency_count (path)

    fs::dir_delete (path)

    expect_type (ly, "double")
    expect_length (ly, 1L)
    expect_named (ly, NULL)
    expect_true (ly > 0)

    expect_type (ndeps, "integer")
    expect_length (ndeps, 1L)
    expect_named (ndeps, NULL)
    expect_true (ndeps > 0)
})

test_that ("cm metric issue age", { # R/cm-metrics-issue-response.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    res <- rm_metric_issue_age (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (res, "integer")
    expect_length (res, 1L)
    expect_named (res, NULL)
    expect_true (is.na (res))
})

test_that ("cm metric release frequency", { # R/cm-metrics-release-freq.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    # Need to extend period to capture enough releases:
    op <- getOption ("repometrics_period")
    options ("repometrics_period" = 1000)
    rel_freq <- rm_metric_release_freq (path, end_date = end_date)
    rel_count <- rm_metric_recent_releases (path, end_date = end_date)
    options ("repometrics_period" = op)

    fs::dir_delete (path)

    expect_type (rel_freq, "integer")
    expect_length (rel_freq, 1L)
    expect_named (rel_freq, NULL)
    expect_true (rel_freq > 0L)

    expect_type (rel_count, "integer")
    expect_length (rel_count, 1L)
    expect_named (rel_count, expected = NULL)
    expect_true (rel_count > 0L)
})

test_that ("cm metric programming languages", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    dat <- rm_data_languages_internal (path)
    met <- rm_metric_languages (path) # re-scaled version of ncode_pc

    fs::dir_delete (path)

    expect_s3_class (dat, "data.frame")
    expect_true (nrow (dat) > 0L)
    expect_equal (ncol (dat), 5L)
    expect_identical (
        names (dat),
        c ("language", "nfiles", "ncode", "nfiles_pc", "ncode_pc")
    )
    expect_equal (dat$language [1], "R")
    expect_type (dat$nfiles, "integer")
    expect_type (dat$ncode, "integer")
    expect_type (dat$nfiles_pc, "double")
    expect_type (dat$ncode_pc, "double")

    expect_type (met, "double")
    expect_length (met, 1L)
    expect_true (met > 0 && met < 1)
})

test_that ("cm metric bus and elephant", { # R/cm-metric-has-ci.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    dat <- rm_data_contrib_absence_internal (path, end_date = end_date)
    res1 <- rm_metric_contrib_absence_commits (path, end_date = end_date)
    res2 <- rm_metric_elephant_factor (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (dat, "integer")
    expect_length (dat, 3L)
    expect_named (dat, c ("ncommits", "nfiles_changed", "lines_changed"))
    expect_true (all (dat > 0L))

    expect_type (res1, "integer")
    expect_length (res1, 1L)
    expect_named (res1, NULL)
    expect_true (res1 > 0L)
    expect_equal (res1, dat [["ncommits"]])

    expect_type (res2, "integer")
    expect_length (res2, 1L)
    expect_named (res2, NULL)
    expect_true (res2 > 0L)
})

end_date <- as.Date ("2024-12-01")

test_that ("cm metric ctb and committer count", { # R/cm-metric-ctb-count.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    # data has [code, pr_authors, issue_authors, issue_cmt_authors]
    data_ctb <- rm_data_ctb_count_internal (path, end_date = end_date)
    counts_ctb <- rm_metric_ctb_count (path, end_date = end_date) # code only
    counts_cmt <- rm_metric_committer_count (path, end_date = end_date)
    counts_watchers <- rm_metric_watcher_count (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (data_ctb, "integer")
    expect_length (data_ctb, 4L)
    expect_named (
        data_ctb,
        c ("code", "pr_authors", "issue_authors", "issue_cmt_authors")
    )
    expect_true (all (data_ctb >= 0L))
    expect_true (sum (data_ctb) > 0L)

    expect_type (counts_ctb, "integer")
    expect_length (counts_ctb, 1L)
    expect_named (counts_ctb, NULL)
    expect_true (counts_ctb >= 0L)

    expect_type (counts_cmt, "integer")
    expect_length (counts_cmt, 3L)
    expect_named (counts_cmt, c ("watchers", "issues", "prs"))
    expect_true (all (counts_cmt >= 0L))
    expect_true (sum (counts_cmt) > 0L)

    expect_type (counts_watchers, "integer")
    expect_length (counts_watchers, 1L)
    expect_true (counts_watchers >= 0L)
})

test_that ("cm metric issue updates and comments", { # R/cm-metric-issue-updates.R

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    num_updates <- rm_metric_issue_updates (path, end_date = end_date)
    issues_closed <- rm_metric_issues_closed (path, end_date = end_date)
    num_issue_cmts <- rm_metric_issue_cmt_count (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (num_updates, "integer")
    expect_length (num_updates, 1L)
    expect_named (num_updates, expected = NULL)
    expect_true (num_updates > 0L)

    expect_type (issues_closed, "integer")
    expect_length (issues_closed, 1L)
    expect_named (issues_closed, expected = NULL)
    expect_true (issues_closed > 0L)

    expect_type (num_issue_cmts, "double") # mean value
    expect_length (num_issue_cmts, 1L)
    expect_named (num_issue_cmts, NULL)
    expect_true (num_issue_cmts >= 0)
})

test_that ("cm metric maintainer count", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    maintainers <- rm_metric_maintainer_count (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (maintainers, "integer")
    expect_length (maintainers, 1L)
    expect_named (maintainers, NULL)
    expect_true (maintainers >= 0L)
})

test_that ("cm metric licenses declared + best practices", {

    path <- generate_test_pkg ()

    lic_dat <- rm_data_licenses_declared_internal (path)

    expect_type (lic_dat, "character")
    expect_named (lic_dat, NULL)
    expect_true (length (lic_dat) >= 1)
    lic_ptn <- paste0 (included_licenses, collapse = "|")
    expect_true (all (grepl (lic_ptn, lic_dat)))

    lic <- rm_metric_licenses_declared (path)
    n <- rm_metric_license_coverage (path)
    bp <- rm_metric_best_practices (path)

    fs::dir_delete (path)

    expect_type (lic, "logical")
    expect_length (lic, 1L)
    expect_named (lic, NULL)
    expect_true (lic)

    expect_type (n, "double")
    expect_length (n, 1L)
    expect_named (n, NULL)
    expect_true (n >= 0)

    expect_type (bp, "logical")
    expect_length (bp, 1L)
    expect_named (bp, NULL)
    expect_false (bp)
})

test_that ("cm metric burstiness", {

    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    b <- rm_metric_burstiness (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (b, "double")
    expect_length (b, 1L)
    expect_named (b, NULL)
    # Test data have only one commit day, so no bustiness can be measured:
    expect_true (is.na (b))
})

test_that ("cm metric test coverage", {

    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    cov <- rm_metric_test_coverage (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (cov, "double")
    expect_length (cov, 1L)
    # expect_true (cov >= 0 && cov <= 100)
})

test_that ("cm metric collate all", {

    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    metrics_data <- collate_all_metrics (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (metrics_data, "list")
    expect_length (metrics_data, 47L)
    metric_fns <- rm_chaoss_metrics_list ()$fn_name
    expect_identical (names (metrics_data), gsub ("^rm\\_metric\\_", "", metric_fns))

    lens <- vapply (metrics_data, length, integer (1L), USE.NAMES = FALSE)
    lens_expected <- as.integer (c (
        1, 1, 1, 1, 1, 1, 1, 1, 3, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 4, 0, 1, 1,
        1, 1, 1, 4, 1, 1, 1
    ))
    expect_equal (lens, lens_expected)
})
