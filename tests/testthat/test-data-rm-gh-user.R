# Extends from 'cm-data-github.R' to test the user-specific data

test_that ("rm user data internal structures", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data (repo = FALSE)

    expect_type (dat, "list")
    expect_length (dat, 2L)
    expect_named (dat, c ("gaborcsardi", "hfrick"))

    dat1 <- dat [[1]]

    expect_length (dat1, 7L)
    nms <- c (
        "general", "commit_cmt", "commits", "followers", "following",
        "issue_cmts", "issues"
    )
    expect_named (dat1, nms)

    expect_type (dat1$general, "list")
    expect_length (dat1$general, 2L)
    nms <- c ("user", "orgs")
    expect_named (dat1$general, nms)

    expect_s3_class (dat1$general$user, "data.frame")
    expect_equal (nrow (dat1$general$user), 1L)
    expect_equal (ncol (dat1$general$user), 12L)
    nms <- c (
        "login", "name", "email", "location", "company", "bio", "avatarUrl",
        "num_repositories", "repos_contributed_to", "num_issues_opened",
        "num_prs_opened", "num_starred_repos"
    )
    expect_named (dat1$general$user, nms)

    expect_s3_class (dat1$general$orgs, "data.frame")
    expect_true (nrow (dat1$general$orgs) > 1L)
    expect_equal (ncol (dat1$general$orgs), 6L)
    nms <- c (
        "name", "gh_org", "url", "web_url", "location", "num_members"
    )
    expect_named (dat1$general$orgs, nms)

    expect_type (dat1$followers, "character")
    expect_true (length (dat1$followers) > 1L)
    expect_type (dat1$following, "character")
    expect_true (length (dat1$following) >= 0L)

    expect_s3_class (dat1$commit_cmt, "data.frame")
    expect_true (nrow (dat1$commit_cmt) > 1L)
    expect_equal (ncol (dat1$commit_cmt), 4L)
    nms <- c ("org", "repo", "timestamp", "stargazers")
    expect_named (dat1$commit_cmt, nms)

    expect_s3_class (dat1$commits, "data.frame")
    expect_true (nrow (dat1$commits) > 0L)
    expect_equal (ncol (dat1$commits), 3L)
    nms <- c ("repo", "num_commits", "date")
    expect_named (dat1$commits, nms)

    expect_s3_class (dat1$issues, "data.frame")
    expect_true (nrow (dat1$issues) >= 0L)
    expect_equal (ncol (dat1$issues), 8L)
    nms <- c (
        "opened_at", "closed_at", "org_repo", "issue_num",
        "num_issue_comments", "num_issue_participants", "num_repo_languages",
        "repo_languages"
    )
    expect_named (dat1$issues, nms)

    expect_s3_class (dat1$issue_cmts, "data.frame")
    expect_true (nrow (dat1$issue_cmts) > 1L)
    expect_equal (ncol (dat1$issue_cmts), 5L)
    nms <- c ("org_repo", "issue_num", "created_at", "num_comments", "num_participants")
    expect_named (dat1$issue_cmts, nms)
})

test_that ("repometrics_data_user fn", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat_mocked <- mock_rm_data (repo = FALSE)

    login <- "gaborcsardi"
    end_date <- as.Date ("2024-01-01")

    dat <- repometrics_data_user (
        login = login,
        n_per_page = 1L,
        end_date = end_date,
        nyears = 1
    )
    expect_identical (dat, dat_mocked$gaborcsardi)
})
