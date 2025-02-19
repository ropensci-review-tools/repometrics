test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

skip_if (!test_all)

test_that ("author matches", { # R/cm-metric-cran-downloads.R

    dat <- mock_rm_data ()

    ctbs_gh <- dat$contribs_from_gh_api
    ctbs_log <- dat$contribs_from_log

    expect_equal (ncol (ctbs_log), 2L)
    expect_equal (names (ctbs_log), c ("handle", "email"))
    expect_equal (nrow (ctbs_log), 1L)
    expect_true ("mpadge" %in% ctbs_log$handle)
    expect_false ("hfrick" %in% ctbs_log$handle)

    # Fake extend log data
    ctbs_log <- rbind (
        ctbs_log,
        data.frame (handle = "hannafrick", email = "hanna@here")
    )

    # Fake extend gh data:
    n <- nrow (ctbs_gh)
    ctbs_gh <- ctbs_gh [c (1:n, n), ]
    ctbs_gh [n + 1, ] <- NA
    ctbs_gh$login [n + 1] <- "mpadge"

    ctbs_log <- get_all_contribs (ctbs_log, ctbs_gh)
    expect_equal (ncol (ctbs_log), 3L)
    expect_equal (names (ctbs_log), c ("name", "email", "gh_handle"))
    expect_equal (nrow (ctbs_log), 2L)
    expect_true ("mpadge" %in% ctbs_log$gh_handle)
    expect_true ("hfrick" %in% ctbs_log$gh_handle)
})

test_that ("url from path", {

    path <- generate_test_pkg ()
    url <- pkg_gh_url_from_path (path)
    url_in_desc <- "https://github.com/ropensci-review-tools/goodpractice"
    expect_equal (url, url_in_desc)

    # Rm URL from desc:
    desc_path <- fs::dir_ls (path, regexp = "DESCRIPTION", type = "file")
    desc <- brio::readLines (desc_path)
    url_line <- grep ("^URL", desc, value = TRUE)
    desc <- desc [-grep ("^URL", desc)]
    writeLines (desc, desc_path)
    expect_length (pkg_gh_url_from_path (path), 0L)

    url_line <- gsub ("^URL", "BugReports", url_line)
    url_line <- paste0 (url_line, "/issues")
    desc <- c (desc, url_line)
    writeLines (desc, desc_path)
    url <- pkg_gh_url_from_path (path)
    expect_equal (url, url_in_desc)
    # Then remove again:
    desc <- desc [-length (desc)]
    writeLines (desc, desc_path)
    expect_length (pkg_gh_url_from_path (path), 0L)

    # Add git remote:
    gert::git_remote_add ("https://not.a.url", repo = path)
    expect_length (pkg_gh_url_from_path (path), 0L)
    gert::git_remote_set_url (url_in_desc, remote = "origin", repo = path)
    expect_equal (pkg_gh_url_from_path (path), url_in_desc)

    fs::dir_delete (path)
})
