# Extends from 'cm-data-github.R' to test the user-specific data

test_that ("cm data gh general", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()

    user <- dat$gh_user
    expect_type (user, "list")
    expect_length (user, 7L)
    nms <- c ("general", "followers", "following", "commit_cmt", "commits", "issues", "issue_cmts")
    expect_named (user, nms)

    expect_type (user$general, "list")
    expect_length (user$general, 2L)
    nms <- c ("user", "orgs")
    expect_named (user$general, nms)

    expect_s3_class (user$general$user, "data.frame")
    expect_equal (nrow (user$general$user), 1L)
    expect_equal (ncol (user$general$user), 10L)
    nms <- c (
        "login", "name", "email", "location", "company", "bio", "avatarUrl",
        "num_repositories", "repos_contributed_to", "num_starred_repos"
    )
    expect_named (user$general$user, nms)

    expect_s3_class (user$general$orgs, "data.frame")
    expect_true (nrow (user$general$orgs) > 10L)
    expect_equal (ncol (user$general$orgs), 6L)
    nms <- c (
        "name", "gh_org", "url", "web_url", "location", "num_members"
    )
    expect_named (user$general$orgs, nms)

    expect_type (user$followers, "character")
    expect_true (length (user$followers) > 1L)
    expect_type (user$following, "character")
    expect_true (length (user$following) > 1L)

    expect_s3_class (user$commit_cmt, "data.frame")
    expect_true (nrow (user$commit_cmt) > 1L)
    expect_equal (ncol (user$commit_cmt), 4L)
    nms <- c ("org", "repo", "timestamp", "stargazers")
    expect_named (user$commit_cmt, nms)

    expect_s3_class (user$commits, "data.frame")
    expect_true (nrow (user$commits) > 1L)
    expect_equal (ncol (user$commits), 2L)
    nms <- c ("repo", "num_commits")
    expect_named (user$commits, nms)

    expect_s3_class (user$issues, "data.frame")
    expect_true (nrow (user$issues) > 1L)
    expect_equal (ncol (user$issues), 8L)
    nms <- c (
        "opened_at", "closed_at", "org_repo", "issue_num",
        "num_issue_comments", "num_issue_participants", "num_repo_languages",
        "repo_languages"
    )
    expect_named (user$issues, nms)

    expect_s3_class (user$issue_cmts, "data.frame")
    expect_true (nrow (user$issue_cmts) > 1L)
    expect_equal (ncol (user$issue_cmts), 5L)
    nms <- c ("org_repo", "issue_num", "created_at", "num_comments", "num_participants")
    expect_named (user$issue_cmts, nms)
})
