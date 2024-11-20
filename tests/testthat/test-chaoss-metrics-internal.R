end_date <- as.Date ("2024-08-01")

test_that ("chaoss internal num_commits", {

    path <- generate_test_pkg (add_url = FALSE)

    n <- chaoss_internal_num_commits (path, end_date = end_date)
    expect_equal (n, 4L)

    n <- chaoss_internal_num_contributors (path, end_date = end_date)
    expect_equal (n, 1L)

    fs::dir_delete (path)
})

test_that ("chaoss has CI internal", {

    path <- generate_test_pkg (add_url = FALSE)
    has_ci <- repo_has_ci_files (path)
    expect_length (has_ci, 0L) # No CI files

    url <- pkg_gh_url_from_path (path)
    expect_null (url)
    fs::dir_delete (path)

    path <- generate_test_pkg (add_url = TRUE)
    url <- "https://github.com/ropensci-review-tools/goodpractice"
    expect_identical (url, pkg_gh_url_from_path (path))

    org_repo <- org_repo_from_path (path)
    expect_identical (org_repo, c ("ropensci-review-tools", "goodpractice"))

    fs::dir_delete (path)
})

test_that ("chaoss internal change requests", {

    path <- generate_test_pkg (add_url = FALSE)

    x <- chaoss_internal_change_req (path, end_date = end_date)
    expect_equal (x, 0)
    x <- chaoss_internal_change_req (path, end_date = Sys.Date ())
    expect_equal (x, NA_integer_) # no commits, so NA returned

    fs::dir_delete (path)
})
