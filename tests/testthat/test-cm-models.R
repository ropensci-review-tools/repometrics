end_date <- as.Date ("2024-08-01")

test_that ("cm model developer responsiveness", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    res <- cm_model_dev_reponsiveness (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 2L)
    expect_named (res, c ("mean", "median"))
})

test_that ("cm model project engagement and awareness", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    eng <- cm_model_proj_engagement (path, end_date = end_date)
    awa <- cm_model_proj_awareness (path, end_date = end_date)

    fs::dir_delete (path)

    expect_type (eng, "double")
    expect_length (eng, 1L)
    expect_named (eng, NULL)
    expect_true (eng > 0)

    expect_type (awa, "integer")
    expect_length (awa, 1L)
    expect_named (awa, NULL)
    expect_true (awa > 0L)
})

test_that ("cm model community activity + oss compliance", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    res <- cm_model_community_activity (path, end_date = end_date)
    oss <- cm_model_oss_compliance (path, end_date = end_date)

    expect_type (res, "double")
    expect_length (res, 1L)
    expect_named (res, NULL)
    expect_true (res > 0)

    expect_type (oss, "double")
    expect_length (oss, 1L)
    expect_named (oss, NULL)
    expect_true (oss > 0)

    fs::dir_delete (path)
})
