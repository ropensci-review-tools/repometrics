end_date <- as.Date ("2024-08-01")

test_that ("cm metrics num_commits num_contribs", {

    path <- generate_test_pkg ()

    n <- cm_metric_num_commits (path, end_date = end_date)
    expect_equal (n, 4L)

    n <- cm_metric_num_contributors (path, end_date = end_date)
    expect_equal (n, 1L)

    fs::dir_delete (path)
})

test_that ("cm metric has CI internal", {

    path <- generate_test_pkg ()
    has_ci <- repo_has_ci_files (path)
    expect_length (has_ci, 0L) # No CI files

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

test_that ("cm metric has_ci", {

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

test_that ("cm metric cran_downloads", {

    path <- generate_test_pkg () # has URL of "goodpractice"
    desc_path <- fs::path (path, "DESCRIPTION")
    desc <- readLines (desc_path)
    desc [1] <- "Package: goodpractice"
    writeLines (desc, desc_path)

    end_date <- as.Date ("2024-01-01")
    dl <- with_mock_dir ("cran_dl", {
        cm_metric_cran_downloads (path = path, end_date = end_date)
    })
    expect_type (dl, "integer")
    expect_length (dl, 1L)
    expect_equal (dl, 2308)

    fs::dir_delete (path)
})
