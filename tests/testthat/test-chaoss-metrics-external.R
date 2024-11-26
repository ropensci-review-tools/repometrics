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
