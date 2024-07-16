pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
flist <- unzip (pkg, exdir = fs::path_temp ())
path <- fs::path_dir (flist [1])

res0 <- githist (path)

test_that ("dashboard input errors", {

    res <- res0
    res$desc_data <- NULL
    expect_error (
        ghist_dashboard (res, action = "build"),
        "Assertion on 'results' failed: Must have length 3"
    )

    res <- res0
    names (res) [1] <- "desc"
    expect_error (
        ghist_dashboard (res, action = "build"),
        "Assertion on 'names\\(results\\)' failed\\: Names must be a identical to set"
    )

    res <- res0
    expect_error (
        ghist_dashboard (res, action = "build"),
        "'arg' should be one of"
    )

    res$stats <- res$stats [, -1]
    expect_error (
        ghist_dashboard (res, action = "render"),
        "'results' has wrong number of columns"
    )
    res <- res0
    res$stats <- res$stats [-seq_len (nrow (res$stats)), ]
    expect_error (
        ghist_dashboard (res, action = "render"),
        "'results' contains empty tables."
    )
})

test_that ("dashboard build", {

    res <- res0
    ghist_dashboard (res, action = "render")

    # Expect quarto docs to have been modified with package name:
    pkg_name <- res0$desc_data$package [1]
    path_tmp <- fs::path (fs::path_temp (), "quarto")
    expect_true (fs::dir_exists (path_tmp))

    f_index <- fs::path (path_tmp, "index.qmd")
    index_qmd <- brio::read_lines (f_index)
    i <- grep ("^title\\:", index_qmd)
    expect_true (grepl ("\\{testpkg\\}", index_qmd [i]))

    f_yaml <- fs::path (path_tmp, "_quarto.yml")
    y <- brio::read_lines (f_yaml)
    i <- grep ("^(\\s+?)title", y)
    expect_true (grepl ("testpkg", y [i]))

    # Expect site docs to have been built:
    path_tmp_site <- fs::path (fs::path_temp (), "quarto", "_site")
    expect_true (fs::dir_exists (path_tmp_site))
    f_tmp_site <- fs::path (fs::path_temp (), "quarto", "_site", "index.html")
    expect_true (fs::file_exists (f_tmp_site))
})
