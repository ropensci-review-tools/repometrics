rm_data <- mock_rm_data ()

pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
flist <- unzip (pkg, exdir = fs::path_temp ())
path <- fs::path_dir (flist [1])
pkgstats <- repo_pkgstats_history (path, num_cores = 1L)
data0 <- list (pkgstats = pkgstats, rm = rm_data)

data_users <- lapply (1:4, function (i) mock_user_rel_data ())
names (data_users) <- letters [seq_len (length (data_users))]

test_that ("dashboard input errors", {

    data <- data0
    expect_error (
        repometrics_dashboard (data, data_users, action = "noarg"),
        "\\'arg\\' should be one of"
    )
    names (data) [1] <- "changed"
    expect_error (
        repometrics_dashboard (data, data_users, action = "render"),
        "Assertion on \\'names\\(data\\)\\' failed\\: Names must be "
    )

    data$pkgstats$stats <- data$pkgstats$stats [, -1]
    expect_error (
        repometrics_dashboard (data, data_users, action = "render"),
        "Assertion on \\'names\\(data\\)\\' failed\\: Names must be "
    )

    data <- data0
    data$pkgstats$stats <- data$pkgstats$stats [-seq_len (nrow (data$pkgstats$stats)), ]
    expect_error (
        repometrics_dashboard (data, data_users, action = "render"),
        "\\'data\\' contains empty tables."
    )

    expect_error (
        repometrics_dashboard (data, data_users, ctb_threshold = "a"),
        "Assertion on \\'ctb\\_threshold\\' failed\\: Must be of type \\'numeric\\'"
    )
    expect_error (
        repometrics_dashboard (data, data_users, ctb_threshold = 2),
        "Assertion on \\'ctb\\_threshold\\' failed\\: Element 1 is not <= 1"
    )
    expect_error (
        repometrics_dashboard (data, data_users, max_ctbs = "a"),
        "Assertion on \\'max\\_ctbs\\' failed\\: Must be of type \\'integerish\\'"
    )
    expect_error (
        repometrics_dashboard (data, data_users, max_ctbs = 0),
        "Assertion on \\'max\\_ctbs\\' failed\\: Element 1 is not >= 1"
    )
})

test_that ("dashboard build", {

    data_repo <- data0
    repometrics_dashboard (data_repo, data_users, action = "render")

    # Expect quarto docs to have been modified with package name:
    pkg_name <- data_repo$pkgstats$desc_data$package [1]
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
