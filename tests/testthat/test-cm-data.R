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

test_that ("cm data gh contribs", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    desc_path <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    url <- "https://github.com/ropensci-review-tools/goodpractice"
    desc <- c (
        readLines (desc_path),
        paste0 ("URL: ", url)
    )
    writeLines (desc, desc_path)

    ctbs <- with_mock_dir ("gh_api_ctbs", {
        contribs_from_gh_api (path, n_per_page = 2L)
    })

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

    fs::dir_delete (path)
})
