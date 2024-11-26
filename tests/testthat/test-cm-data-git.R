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

skip_on_cran ()
# The `releases_from_gh_api()` call fails here on mac, I guess because of
# httptest2, and some interaction with the `cran_package_db()` call also
# triggered with `cm_data_libyears()`?
skip_on_os ("mac")

test_that ("cm data libyears", {

    mock_cm_data ()

    path <- generate_test_pkg ()
    libyears <- cm_data_libyears (path)
    fs::dir_delete (path)

    expect_type (libyears, "double")
    expect_length (libyears, 2L)
    expect_named (libyears)
    expect_equal (names (libyears), c ("mean", "median"))
    expect_true (all (libyears > 0))
})
