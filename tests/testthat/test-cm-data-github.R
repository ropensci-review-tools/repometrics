# These test the individual components of the full data tested in
# 'test-cm-data.R'. These components are tested here in the order in which they
# are returned. Components which come directly from git, and not github, are
# not checked here, and are tested in 'test-cm-data-git.R'.
#
#' - [x] "contribs_from_gh_api"
#' - [x] "contribs_from_log"
#' - [ ] "dependencies"
#' - [x] "gh_repo_workflow"
#' - [ ] "gitlog"
#' - [x] "issue_comments_from_gh_api"
#' - [x] "issues_from_gh_api"
#' - [ ] "libyears"
#' - [x] "prs_from_gh_api"
#' - [x] "releases_from_gh_api"
#' - [x] "repo_from_gh_api"

test_that ("cm data gh contribs", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()

    path <- generate_test_pkg ()
    ctbs_api <- cm_data_contribs_from_gh_api (path, n_per_page = 2L)
    ctbs_log <- cm_data_contribs_from_log (path, n_per_page = 2L)
    fs::dir_delete (path)

    expect_s3_class (ctbs_api, "data.frame")
    expect_equal (nrow (ctbs_api), 2L)
    expect_equal (ncol (ctbs_api), 17L)
    nms <- c (
        "login", "ctb_id", "avatar_url", "api_url", "gh_url", "contributions", "name", "company",
        "email", "location", "blog", "bio", "public_repos", "followers", "following", "created_at",
        "updated_at"
    )
    expect_equal (names (ctbs_api), nms)

    int_index <- c (2, 6, 13:15)
    char_index <- seq_along (nms) [-int_index]
    int_nms <- nms [int_index]
    char_nms <- nms [char_index]
    for (n in names (ctbs_api)) {
        type <- ifelse (n %in% char_nms, "character", "integer")
        expect_type (ctbs_api [[n]], type)
    }

    expect_s3_class (ctbs_log, "data.frame")
    expect_equal (nrow (ctbs_log), 1L)
    expect_equal (ncol (ctbs_log), 2L)
    nms <- c ("handle", "email")
    expect_equal (names (ctbs_log), nms)
})

# dependencies are in 'test-cm-data-git.R'

test_that ("cm data gh workflow", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    wf <- cm_data_gh_repo_workflow (path, n_per_page = 2L)
    fs::dir_delete (path)

    expect_s3_class (wf, "data.frame")
    expect_equal (nrow (wf), 2L)
    expect_equal (ncol (wf), 7L)
    nms <- c ("name", "id", "sha", "title", "status", "conclusion", "created")
    expect_equal (names (wf), nms)

    dbls <- c ("id", "created")
    for (n in names (wf)) {
        type <- ifelse (n %in% dbls, "double", "character")
        expect_type (wf [[n]], type)
    }
})

# gitlog is in 'test-cm-data.git'

test_that ("cm data gh issue comments", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    cmts <- cm_data_issue_comments_from_gh_api (path, n_per_page = 2L)
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

test_that ("cm data gh issues", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    issues <- cm_data_issues_from_gh_api (path, n_per_page = 2L)
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

# libyears is in 'test-cm-data.git'

test_that ("cm data gh prs", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    prs <- cm_data_prs_from_gh_api (path, n_per_page = 2L)
    fs::dir_delete (path)

    expect_s3_class (prs, "data.frame")
    expect_equal (nrow (prs), 2L)
    expect_equal (ncol (prs), 23L)
    nms <- c (
        "number", "user_login", "state", "merged", "merged_by", "merge_commit",
        "closed", "title", "review_decision", "created_at", "closed_at",
        "updated_at", "num_commits", "additions", "deletions", "changed_files",
        "commit_oids", "closing_issue_refs", "total_comments", "participants",
        "body", "comments", "reviews"
    )
    expect_equal (names (prs), nms)

    int_nms <- c (
        "number", "num_commits", "additions",
        "deletions", "changed_files", "total_comments"
    )
    logical_nms <- c ("merged", "closed")
    list_nms <- c ("closing_issue_refs", "comments", "reviews")
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

test_that ("cm data gh releases", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    releases <- cm_data_releases_from_gh_api (path, n_per_page = 2L)
    fs::dir_delete (path)

    expect_s3_class (releases, "data.frame")
    expect_equal (nrow (releases), 2L)
    expect_equal (ncol (releases), 10L)
    nms <- c (
        "id", "author_login", "author_id", "tag_name", "target_commitish",
        "name", "draft", "prerelease", "created_at", "published_at"
    )
    expect_equal (names (releases), nms)

    int_nms <- c ("id", "author_id")
    logical_nms <- c ("draft", "prerelease")
    non_char <- c (int_nms, logical_nms)
    char_nms <- names (releases) [which (!names (releases) %in% non_char)]
    for (n in names (releases)) {
        if (n %in% int_nms) {
            type <- "integer"
        } else if (n %in% logical_nms) {
            type <- "logical"
        } else {
            type <- "character"
        }
        expect_type (releases [[n]], type)
    }
})

test_that ("cm data gh repo", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    mock_cm_data ()
    path <- generate_test_pkg ()
    repo <- cm_data_repo_from_gh_api (path)
    fs::dir_delete (path)

    expect_s3_class (repo, "data.frame")
    expect_equal (nrow (repo), 1L)
    expect_equal (ncol (repo), 18L)
    nms <- c (
        "id", "name", "full_name", "owner", "url", "description", "is_fork",
        "created_at", "updated_at", "homepage", "size", "stargazers_count",
        "subscribers_count", "language", "forks_count", "open_issues_count",
        "topics", "default_branch"
    )
    expect_equal (names (repo), nms)

    int_index <- c (1, 11:13, 15:16)
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
