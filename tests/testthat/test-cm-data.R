test_that ("cm data git", {

    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

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
