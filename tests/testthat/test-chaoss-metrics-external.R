test_that ("chaoss external util fns", {
    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    pkg_name <- pkg_name_from_path (path)
    expect_equal (pkg_name, "testpkg")

    fs::dir_delete (path)
})

test_that ("chaoss external cran_downloads", {

    pkg_name <- "goodpractice"
    end_date <- as.Date ("2024-01-01")
    dl <- with_mock_dir ("cran_dl", {
        cran_downloads (pkg_name = pkg_name, end_date = end_date)
    })
    expect_type (dl, "integer")
    expect_length (dl, 1L)
    expect_equal (dl, 2308)
})

test_that ("chaoss has CI external", {
    org <- "ropensci-review-tools"
    repo <- "repometrics"
    ci_data <- with_mock_dir (
        "gh_workflow",
        github_repo_workflow_query (org, repo, n = 2L)
    )

    expect_s3_class (ci_data, "data.frame")
    expect_equal (nrow (ci_data), 2L)
    expect_equal (ncol (ci_data), 7L)
    expect_equal (
        names (ci_data),
        c ("name", "id", "sha", "title", "status", "conclusion", "created")
    )
})
