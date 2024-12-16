test_that ("rm data git", {

    path <- generate_test_pkg ()

    log <- rm_data_gitlog (path)

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
# triggered with `rm_data_libyears()`?
skip_on_os ("mac")

test_that ("rm data libyears", {

    dat <- mock_rm_data ()

    path <- generate_test_pkg ()
    libyears <- rm_data_libyears (path)
    fs::dir_delete (path)

    expect_s3_class (libyears, "data.frame")
    expect_equal (nrow (libyears), 1L)
    nms <- c ("name", "type", "version", "cran_version", "published", "libyears")
    expect_equal (names (libyears), nms)
})
