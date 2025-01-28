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

    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 1L)
    expect_named (res, NULL)
    expect_true (res > 0)

    expect_type (oss, "double")
    expect_length (oss, 1L)
    expect_named (oss, NULL)
    expect_true (oss > 0)
})

test_that ("cm model viability", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    com <- cm_model_viability_community (path, end_date = end_date)
    sta <- cm_model_viability_starter (path, end_date = end_date)
    gov <- cm_model_viability_gov (path, end_date = end_date)
    str <- cm_model_viability_strategy (path, end_date = end_date)
    dev <- cm_model_collab_devel_index (path, end_date = end_date)
    css <- cm_model_comm_serv_support (path, end_date = end_date)
    sth <- cm_model_starter_health (path, end_date = end_date)

    fs::dir_delete (path)

    for (i in list (com, sta, gov, str, dev, css, sth)) {
        expect_type (i, "double")
        expect_length (i, 1L)
        expect_named (i, NULL)
        expect_true (i > 0)
    }
})
