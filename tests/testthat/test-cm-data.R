test_that ("cm data full", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()

    dat <- cm_data (path)

    fs::dir_delete (path)

    expect_type (dat, "list")
    expect_length (dat, 13L)
    nms <- c (
        "contribs_from_gh_api", "contribs_from_log", "dependencies",
        "gh_repo_workflow", "gitlog", "issue_comments_from_gh_api",
        "issues_from_gh_api", "libyears", "prs_from_gh_api",
        "releases_from_gh_api", "repo_forks", "repo_from_gh_api",
        "repo_stargazers"
    )
    expect_equal (names (dat), nms)

    data_fns <- get_cm_data_fns ()
    data_fn_nms <- gsub ("^cm\\_data\\_", "", data_fns)
    expect_identical (names (dat), data_fn_nms)
})

# The individual components of all data are tested in test-cm-data-github.R
