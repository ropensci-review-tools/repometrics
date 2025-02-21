rm_data <- mock_rm_data ()

pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
flist <- unzip (pkg, exdir = fs::path_temp ())
path <- fs::path_dir (flist [1])
pkgstats <- repo_pkgstats_history (path, num_cores = 1L)
data0 <- list (pkgstats = pkgstats, rm = rm_data)

num_users <- 4L
data_users <- lapply (seq_len (num_users), function (i) mock_user_rel_data ())
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
    data$pkgstats$stats <-
        data$pkgstats$stats [-seq_len (nrow (data$pkgstats$stats)), ]
    expect_error (
        repometrics_dashboard (data, data_users, action = "render"),
        "\\'data\\' contains empty tables."
    )

    expect_error (
        repometrics_dashboard (data, data_users, ctb_threshold = "a"),
        paste0 (
            "Assertion on \\'ctb\\_threshold\\' failed\\: ",
            "Must be of type \\'numeric\\'"
        )
    )
    expect_error (
        repometrics_dashboard (data, data_users, ctb_threshold = 2),
        "Assertion on \\'ctb\\_threshold\\' failed\\: Element 1 is not <= 1"
    )
    expect_error (
        repometrics_dashboard (data, data_users, max_ctbs = "a"),
        paste0 (
            "Assertion on \\'max\\_ctbs\\' failed\\: ",
            "Must be of type \\'integerish\\'"
        )
    )
    expect_error (
        repometrics_dashboard (data, data_users, max_ctbs = 0),
        "Assertion on \\'max\\_ctbs\\' failed\\: Element 1 is not >= 1"
    )
    expect_error (
        repometrics_dashboard (
            data,
            data_users,
            ctb_threshold = 0.5,
            max_ctbs = 2
        ),
        "Only one of \\'ctb\\_threshold\\' or \\'max\\_ctbs\\' may be specified"
    )
})

test_that ("dashboard build", {

    data_repo <- data0
    repometrics_dashboard (data_repo, data_users, action = "render")

    # Expect quarto docs to have been modified with package name:
    path_tmp <- fs::path (fs::path_temp (), "quarto")
    expect_true (fs::dir_exists (path_tmp))

    pkg_name <- data_repo$pkgstats$desc_data$package [1]
    f_index <- fs::path (path_tmp, "index.qmd")
    index_qmd <- brio::read_lines (f_index)
    i <- grep ("^title\\:", index_qmd)
    ptn <- paste0 ("\\{", pkg_name, "\\}")
    expect_true (grepl (ptn, index_qmd [i]))

    f_yaml <- fs::path (path_tmp, "_quarto.yml")
    y <- brio::read_lines (f_yaml)
    i <- grep ("^(\\s+?)title", y)
    expect_true (grepl (pkg_name, y [i]))

    # Expect site docs to have been built:
    path_tmp_site <- fs::path (fs::path_temp (), "quarto", "_site")
    expect_true (fs::dir_exists (path_tmp_site))
    f_tmp_site <- fs::path (fs::path_temp (), "quarto", "_site", "index.html")
    expect_true (fs::file_exists (f_tmp_site))

    f_users <- fs::path (path_tmp, "results-users.Rds")
    expect_true (fs::file_exists (f_users))
    expect_length (readRDS (f_users), num_users)

    f_net <- fs::path (path_tmp, "results-user-network.json")
    expect_true (fs::file_exists (f_net))
    net <- jsonlite::read_json (f_net, simplify = TRUE)
    expect_equal (length (which (net$nodes$group == "person")), num_users)

    fs::dir_delete (path_tmp)
})

test_that ("dashboard with user reduction", {

    data_repo <- data0
    max_ctbs <- 2L
    repometrics_dashboard (
        data_repo,
        data_users,
        max_ctbs = max_ctbs,
        action = "render"
    )

    path_tmp <- fs::path (fs::path_temp (), "quarto")
    expect_true (fs::dir_exists (path_tmp))

    f_users <- fs::path (path_tmp, "results-users.Rds")
    expect_true (fs::file_exists (f_users))
    expect_false (length (readRDS (f_users)) == num_users)
    expect_length (readRDS (f_users), max_ctbs)

    f_net <- fs::path (path_tmp, "results-user-network.json")
    expect_true (fs::file_exists (f_net))
    net <- jsonlite::read_json (f_net, simplify = TRUE)
    expect_true (length (which (net$nodes$group == "person")) < num_users)
    expect_equal (length (which (net$nodes$group == "person")), max_ctbs)

    fs::dir_delete (path_tmp)
})

test_that ("reduce_data_users", {

    data_users_red <- reduce_data_users (data_users, max_ctb = 2L)
    expect_length (data_users, num_users)
    expect_length (data_users_red, 2L)

    data_users_red <- reduce_data_users (data_users, ctb_threshold = 0.5)
    expect_true (length (data_users_red) < length (data_users))
})

if (fs::dir_exists (path)) {
    fs::dir_delete (path)
}
