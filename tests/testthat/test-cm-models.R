end_date <- as.Date ("2024-08-01")

test_that ("cm model developer responsiveness", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    res <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "dev_responsiveness"
    )

    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 1L)
    expect_named (res, NULL)
})

test_that ("cm model project engagement and awareness", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    eng <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "proj_engagement"
    )
    awa <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "proj_awareness"
    )

    fs::dir_delete (path)

    expect_type (eng, "double")
    expect_length (eng, 1L)
    expect_named (eng, NULL)
    expect_true (eng < 0) # Most metrics are 0, and log-scaled so -1

    expect_type (awa, "double")
    expect_length (awa, 1L)
    expect_named (awa, NULL)
    expect_true (awa >= 0L)
})

test_that ("cm model community activity + oss compliance", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    res <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "community_activity"
    )
    oss <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "oss_compliance"
    )

    fs::dir_delete (path)

    expect_type (res, "double")
    expect_length (res, 1L)
    expect_named (res, NULL)
    expect_true (res < 0) # Several 0's on log-scale converted to -1

    expect_type (oss, "double")
    expect_length (oss, 1L)
    expect_named (oss, NULL)
    expect_true (oss > 0)
})

test_that ("cm model viability", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    com <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "viability_community"
    )
    sta <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "viability_starter"
    )
    gov <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "viability_gov"
    )
    str <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "viability_strategy"
    )
    dev <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "collab_devel_index"
    )
    css <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "comm_serv_support"
    )
    sth <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "starter_health"
    )
    wel <- calculate_one_model (
        path,
        end_date = end_date,
        model_name = "comm_welcoming"
    )

    fs::dir_delete (path)

    for (i in list (com, sta, gov, str, dev, css, sth, wel)) {
        expect_type (i, "double")
        expect_length (i, 1L)
        expect_named (i, NULL)
    }
})

test_that ("collate all models", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    mod_dat <- collate_all_models (path, end_date = end_date)

    metrics_data <- collate_all_metrics (path, end_date = end_date)
    mod_dat_metrics <- collate_all_models (metrics_data = metrics_data)

    metrics_all <- withr::with_options (
        list ("repometrics_period" = 365.25 / 2),
        metrics_over_end_dates (path, end_date = end_date, num_years = 1)
    )
    models_all <- withr::with_options (
        list ("repometrics_period" = 365.25 / 2),
        models_over_end_dates (path, end_date = end_date, num_years = 1)
    )

    fs::dir_delete (path)

    expect_type (mod_dat, "double")
    expect_length (mod_dat, 13L)
    nms <- names (load_model_json_data ()$models)
    expect_named (mod_dat, nms)
    expect_false (any (is.na (mod_dat)))

    expect_identical (mod_dat, mod_dat_metrics)

    expect_type (metrics_all, "list")
    end_dates <- withr::with_options (
        list ("repometrics_period" = 365.25 / 2),
        get_end_date_seq (end_date = end_date, num_years = 1)
    )
    expect_length (metrics_all, length (end_dates))
    expect_named (metrics_all, as.character (end_dates))
    expect_identical (names (metrics_all [[1]]), names (metrics_data))

    expect_s3_class (models_all, "data.frame")
    expect_equal (nrow (models_all), length (end_dates))
    expect_named (models_all, c ("date", nms))
})
