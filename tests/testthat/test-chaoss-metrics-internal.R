test_that ("chaoss internal num_commits", {
    pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    n <- chaoss_internal_num_commits (path, end_date = as.Date ("2024-08-01"))
    expect_equal (n, 4L)

    n <- chaoss_internal_num_contributors (path, end_date = as.Date ("2024-08-01"))
    expect_equal (n, 1L)

    fs::dir_delete (path)
})

test_that ("chaoss has CI internal", {
    pkg <- system.file ("extdata", "testpkg.zip", package = "githist")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    has_ci <- repo_has_ci_files (path)
    expect_length (has_ci, 0L) # No CI files

    url <- pkg_gh_url_from_path (path)
    expect_null (url)

    desc_path <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    url <- "https://github.com/my/pkg"
    desc <- c (
        readLines (desc_path),
        paste0 ("URL: ", url)
    )
    writeLines (desc, desc_path)

    expect_identical (url, pkg_gh_url_from_path (path))

    org_repo <- org_repo_from_path (path)
    expect_identical (org_repo, c ("my", "pkg"))

    fs::dir_delete (path)
})
