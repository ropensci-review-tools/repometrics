pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
flist <- unzip (pkg, exdir = fs::path_temp ())
path <- fs::path_dir (flist [1])
pkgstats <- repo_pkgstats_history (path, num_cores = 1L)
cm_data <- mock_cm_data ()
data0 <- list (pkgstats = pkgstats, cm = cm_data)

test_that ("dashboard input errors", {

    data <- data0
    expect_error (
        repometrics_dashboard (data, action = "noarg"),
        "\\'arg\\' should be one of"
    )
    names (data) [1] <- "changed"
    expect_error (
        repometrics_dashboard (data, action = "render"),
        "Assertion on \\'names\\(data\\)\\' failed\\: Names must be "
    )

    data$pkgstats$stats <- data$pkgstats$stats [, -1]
    expect_error (
        repometrics_dashboard (data, action = "render"),
        "Assertion on \\'names\\(data\\)\\' failed\\: Names must be "
    )

    data <- data0
    data$pkgstats$stats <- data$pkgstats$stats [-seq_len (nrow (data$pkgstats$stats)), ]
    expect_error (
        repometrics_dashboard (data, action = "render"),
        "\\'data\\' contains empty tables."
    )
})

test_that ("dashboard build", {

    data <- data0
    repometrics_dashboard (data, action = "render")

    # Expect quarto docs to have been modified with package name:
    pkg_name <- data0$pkgstats$desc_data$package [1]
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

if (fs::dir_exists (path)) {
    fs::dir_delete (path)
}
