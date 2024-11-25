end_date <- as.Date ("2024-08-01")

test_that ("chaoss internal num_commits", {

    path <- generate_test_pkg ()

    n <- chaoss_internal_num_commits (path, end_date = end_date)
    expect_equal (n, 4L)

    n <- chaoss_internal_num_contributors (path, end_date = end_date)
    expect_equal (n, 1L)

    fs::dir_delete (path)
})

test_that ("chaoss has CI internal", {

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
