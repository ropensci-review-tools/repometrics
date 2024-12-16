test_that ("user data martrices", {

    user_data <- lapply (1:2, function (i) mock_user_rel_data ())
    names (user_data) <- c ("a", "b")

    mats <- user_relation_matrices (user_data)

    expect_s3_class (mats, "data.frame")
    expect_equal (ncol (mats), 8L)
    nms <- c (
        "login1", "login2", "commit_cmt", "commits", "followers", "following",
        "issue_cmts", "issues"
    )
    expect_equal (names (mats), nms)
    expect_true (nrow (mats) > 0L)
})
