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
