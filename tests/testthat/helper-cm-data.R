# Helper function to use httptest2 mocks to load all data, after which function
# calls are memoised and so can be called without mocking from that point on.

mock_cm_data <- function () {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    n_per_page <- 2L
    end_date <- as.Date ("2024-01-01")

    path <- generate_test_pkg ()
    ctbs <- with_mock_dir ("gh_api_ctbs", {
        cm_data_contribs_from_gh_api (path, n_per_page = n_per_page)
    })
    repo <- with_mock_dir ("gh_api_repo", {
        cm_data_repo_from_gh_api (path)
    })
    issues <- with_mock_dir ("gh_api_issues", {
        cm_data_issues_from_gh_api (path, n_per_page = n_per_page)
    })
    cmts <- with_mock_dir ("gh_api_issue_cmts", {
        cm_data_issue_comments_from_gh_api (path, n_per_page = n_per_page)
    })
    prs <- with_mock_dir ("gh_api_prs", {
        cm_data_prs_from_gh_api (path, n_per_page = n_per_page)
    })
    releases <- with_mock_dir ("gh_api_releases", {
        cm_data_releases_from_gh_api (path, n_per_page = n_per_page)
    })
    workflow <- with_mock_dir ("gh_api_workflow", {
        cm_data_gh_repo_workflow (path, n_per_page = n_per_page)
    })
    libyears <- with_mock_dir ("gh_libyears", {
        cm_data_libyears (path)
    })
    dl <- with_mock_dir ("cran_dl", {
        cm_metric_cran_downloads (path, end_date = end_date)
    })

    fs::dir_delete (path)
}
