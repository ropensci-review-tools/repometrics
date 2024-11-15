test_that ("chaoss external util fns", {
    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    pkg_name <- pkg_name_from_path (path)
    expect_equal (pkg_name, "testpkg")

    fs::dir_delete (path)
})

test_that ("chaoss external cran_downloads", {

    pkg_name <- "goodpractice"
    end_date <- as.Date ("2024-01-01")
    dl <- with_mock_dir ("cran_dl", {
        cran_downloads (pkg_name = pkg_name, end_date = end_date)
    })
    expect_type (dl, "integer")
    expect_length (dl, 1L)
    expect_equal (dl, 2308)
})

test_that ("chaoss has CI external", {
    org <- "ropensci-review-tools"
    repo <- "repometrics"
    ci_data <- with_mock_dir (
        "gh_workflow",
        github_repo_workflow_query (org, repo, n = 2L)
    )

    expect_s3_class (ci_data, "data.frame")
    expect_equal (nrow (ci_data), 2L)
    expect_equal (ncol (ci_data), 7L)
    expect_equal (
        names (ci_data),
        c ("name", "id", "sha", "title", "status", "conclusion", "created")
    )
})

test_that ("chaoss external commits in prs", {

    # This tests this internal function called by "prop_commits_in_prs". The
    # latter function ultimately returns nothing, so this test is needed to
    # confirm the intermediate data structure.
    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    org <- "ropensci-review-tools"
    repo <- "goodpractice"

    dat <- with_mock_dir (
        "gh_pr_qry",
        github_issues_prs_query (org, repo)
    )

    expect_s3_class (dat, "data.frame")
    expect_equal (nrow (dat), 2L) # 2 hard-coded in 'gh-queries.R' for test env
    expect_equal (ncol (dat), 13L)
    nms <- c (
        "id", "type", "login", "action", "number", "commits",
        "num_comments", "num_review_comments", "additions", "deletions",
        "changed_files", "created_at", "merged_at"
    )
    expect_equal (names (dat), nms)
    expect_true (all (c ("created", "closed") %in% dat$action))
})

test_that ("chaoss external prop commits in change req", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    desc_path <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    url <- "https://github.com/ropensci-review-tools/goodpractice"
    desc <- c (
        readLines (desc_path),
        paste0 ("URL: ", url)
    )
    writeLines (desc, desc_path)

    end_date <- as.Date ("2024-01-01")
    prop_commits <- with_mock_dir ("gh_pr_qry", {
        prop_commits_in_change_req (path = path, end_date = end_date)
    })
    expect_identical (prop_commits, 0.)

    fs::dir_delete (path)
})
