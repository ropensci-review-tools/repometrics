end_date <- as.Date ("2024-08-01")

test_that ("cm model developer responsiveness", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    dat <- mock_rm_data ()
    path <- generate_test_pkg () # has URL of "goodpractice"

    res <- cm_model_dev_reponsiveness (path, end_date = end_date)

    expect_type (res, "double")
    expect_length (res, 2L)
    expect_named (res, c ("mean", "median"))

    fs::dir_delete (path)
})
