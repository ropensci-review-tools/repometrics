test_that ("cm data gh contribs", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    path <- generate_test_pkg ()
    ctbs <- with_mock_dir ("gh_api_ctbs", {
        contribs_from_gh_api (path, n_per_page = 2L)
    })

    fs::dir_delete (path)

    expect_s3_class (ctbs, "data.frame")
    expect_equal (nrow (ctbs), 2L)
    expect_equal (ncol (ctbs), 17L)
    nms <- c (
        "login", "ctb_id", "avatar_url", "api_url", "gh_url", "contributions", "name", "company",
        "email", "location", "blog", "bio", "public_repos", "followers", "following", "created_at",
        "updated_at"
    )
    expect_equal (names (ctbs), nms)

    int_index <- c (2, 6, 13:15)
    char_index <- seq_along (nms) [-int_index]
    int_nms <- nms [int_index]
    char_nms <- nms [char_index]
    for (n in names (ctbs)) {
        type <- ifelse (n %in% char_nms, "character", "integer")
        expect_type (ctbs [[n]], type)
    }
})

test_that ("cm data gh repo", {

    path <- generate_test_pkg ()
    repo <- with_mock_dir ("gh_api_repo", {
        repo_from_gh_api (path)
    })

    fs::dir_delete (path)

    expect_s3_class (repo, "data.frame")
    expect_equal (nrow (repo), 1L)
    expect_equal (ncol (repo), 17L)
    nms <- c (
        "id", "name", "full_name", "owner", "url", "description", "is_fork",
        "created_at", "updated_at", "homepage", "size", "stargazers_count",
        "language", "forks_count", "open_issues_count", "topics",
        "default_branch"
    )
    expect_equal (names (repo), nms)

    int_index <- c (1, 11:12, 14:15)
    char_index <- seq_along (nms) [-int_index]
    int_nms <- nms [int_index]
    char_nms <- nms [char_index]
    for (n in names (repo)) {
        type <- ifelse (n %in% char_nms, "character", "integer")
        if (n == "is_fork") {
            type <- "logical"
        }
        expect_type (repo [[n]], type)
    }
})

test_that ("cm data gh issues", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    path <- generate_test_pkg ()
    issues <- with_mock_dir ("gh_api_issues", {
        issues_from_gh_api (path, n_per_page = 2L)
    })

    fs::dir_delete (path)

    expect_s3_class (issues, "data.frame")
    expect_equal (nrow (issues), 2L)
    expect_equal (ncol (issues), 24L)
    nms <- c (
        "number", "title", "id", "url", "user_login", "user_id", "labels",
        "state", "assignee", "comments", "created_at", "updated_at",
        "closed_at", "issue_body", "closed_by", "state_reason",
        "reaction_plus1", "reaction_minus1", "reaction_laugh",
        "reaction_hooray", "reaction_confused", "reaction_heart",
        "reaction_rocket", "reaction_eyes"
    )
    expect_equal (names (issues), nms)

    int_index <- c (1, 6, 10, grep ("^reaction", names (issues)))
    char_index <- seq_along (nms) [-int_index]
    int_nms <- nms [int_index]
    char_nms <- nms [char_index]
    for (n in names (issues)) {
        type <- ifelse (n %in% char_nms, "character", "integer")
        if (n == "id") {
            type <- "double"
        }
        expect_type (issues [[n]], type)
    }
})

test_that ("cm data gh issue comments", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    path <- generate_test_pkg ()
    cmts <- with_mock_dir ("gh_api_issue_cmts", {
        issue_comments_from_gh_api (path, n_per_page = 2L)
    })

    fs::dir_delete (path)

    expect_s3_class (cmts, "data.frame")
    expect_equal (nrow (cmts), 2L)
    expect_equal (ncol (cmts), 9L)
    nms <- c (
        "issue_url", "issue_number", "comment_url", "comment_id", "user_login",
        "user_id", "created_at", "updated_a", "issue_body"
    )
    expect_equal (names (cmts), nms)

    int_index <- c (2, 6)
    char_index <- seq_along (nms) [-int_index]
    int_nms <- nms [int_index]
    char_nms <- nms [char_index]
    for (n in names (cmts)) {
        type <- ifelse (n %in% char_nms, "character", "integer")
        if (n == "comment_id") {
            type <- "double"
        }
        expect_type (cmts [[n]], type)
    }
})

test_that ("cm data gh prs", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    path <- generate_test_pkg ()
    prs <- with_mock_dir ("gh_api_prs", {
        prs_from_gh_api (path, n_per_page = 2L)
    })

    fs::dir_delete (path)

    expect_s3_class (prs, "data.frame")
    expect_equal (nrow (prs), 2L)
    expect_equal (ncol (prs), 21L)
    nms <- c (
        "number", "user_login", "state", "merged", "merged_by", "merge_commit",
        "closed", "title", "review_decision", "created_at", "closed_at",
        "updated_at", "additions", "deletions", "changed_files", "commit_oids",
        "closing_issue_refs", "total_comments", "participants", "body",
        "comments"
    )
    expect_equal (names (prs), nms)

    int_nms <- c (
        "number", "additions", "deletions", "changed_files", "total_comments"
    )
    logical_nms <- c ("merged", "closed")
    list_nms <- c ("closing_issue_refs", "comments")
    non_char <- c (int_nms, logical_nms, list_nms)
    char_nms <- names (prs) [which (!names (prs) %in% non_char)]
    for (n in names (prs)) {
        if (n %in% int_nms) {
            type <- "integer"
        } else if (n %in% logical_nms) {
            type <- "logical"
        } else if (n %in% list_nms) {
            type <- "list"
        } else {
            type <- "character"
        }
        expect_type (prs [[n]], type)
    }
})
