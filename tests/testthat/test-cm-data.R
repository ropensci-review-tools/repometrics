test_that ("cm data git", {

    path <- generate_test_pkg ()

    log <- cm_data_gitlog (path)

    expect_s3_class (log, "data.frame")
    expect_equal (ncol (log), 10L)
    nms <- c (
        "hash", "aut_name", "aut_email", "timestamp", "message",
        "nfiles_changed", "lines_added", "lines_removed", "whitespace_added",
        "whitespace_removed"
    )
    expect_equal (names (log), nms)

    char_nms <- nms [c (1:3, 5)]
    int_nms <- nms [6:10]
    for (n in names (log)) {
        type <- ifelse (n %in% char_nms, "character", "integer")
        if (n == "timestamp") {
            expect_s3_class (log [[n]], "POSIXct")
        } else {
            expect_type (log [[n]], type)
        }
    }

    fs::dir_delete (path)
})

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

test_that ("cm data dependencies", {

    path <- generate_test_pkg ()
    deps <- cm_data_dependencies (path)
    fs::dir_delete (path)

    expect_s3_class (deps, "data.frame")
    expect_equal (nrow (deps), 1L)
    expect_equal (ncol (deps), 3L)
    nms <- c ("name", "type", "version")
    expect_equal (names (deps), nms)
    for (n in names (deps)) {
        expect_type (deps [[n]], "character")
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
