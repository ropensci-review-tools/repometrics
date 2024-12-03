# Helper function to use httptest2 mocks to load all data, after which function
# calls are memoised and so can be called without mocking from that point on.

mock_cm_data <- function () {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    end_date <- as.Date ("2024-01-01")

    path <- generate_test_pkg ()
    ctbs <- httptest2::with_mock_dir ("gh_api_ctbs", {
        cm_data_contribs_from_gh_api (path)
    })
    repo <- httptest2::with_mock_dir ("gh_api_repo", {
        cm_data_repo_from_gh_api (path)
    })
    forks <- httptest2::with_mock_dir ("gh_api_forks", {
        cm_data_repo_forks (path)
    })
    stargazers <- httptest2::with_mock_dir ("gh_api_stars", {
        cm_data_repo_stargazers (path)
    })
    issues <- httptest2::with_mock_dir ("gh_api_issues", {
        cm_data_issues_from_gh_api (path)
    })
    cmts <- httptest2::with_mock_dir ("gh_api_issue_cmts", {
        cm_data_issue_comments_from_gh_api (path)
    })
    prs <- httptest2::with_mock_dir ("gh_api_prs", {
        cm_data_prs_from_gh_api (path)
    })
    releases <- httptest2::with_mock_dir ("gh_api_releases", {
        cm_data_releases_from_gh_api (path)
    })
    workflow <- httptest2::with_mock_dir ("gh_api_workflow", {
        cm_data_gh_repo_workflow (path)
    })
    libyears <- httptest2::with_mock_dir ("gh_libyears", {
        cm_data_libyears (path)
    })

    # cran_downloads fn needs modified DESC:
    desc_path <- fs::path (path, "DESCRIPTION")
    desc <- readLines (desc_path)
    desc [1] <- "Package: goodpractice"
    writeLines (desc, desc_path)

    dl <- httptest2::with_mock_dir ("cran_dl", {
        cm_metric_cran_downloads (path, end_date = end_date)
    })

    # The return full mocked data set:
    data_fns <- get_cm_data_fns ()
    res <- lapply (data_fns, function (i) {
        do.call (i, list (path = path))
    })
    names (res) <- gsub ("^cm\\_data\\_", "", data_fns)
    res$contributors <- get_all_contribs (
        res$contribs_from_log,
        res$contribs_from_gh_api
    )

    fs::dir_delete (path)

    return (res)
}
