# Helper function to use httptest2 mocks to load all data, after which function
# calls are memoised and so can be called without mocking from that point on.

mock_rm_data <- function (repo = TRUE) {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    end_date <- as.Date ("2024-01-01")

    path <- generate_test_pkg ()
    ctbs <- httptest2::with_mock_dir ("gh_api_ctbs", {
        rm_data_contribs_from_gh_api (path)
    })
    workflow <- httptest2::with_mock_dir ("gh_api_workflow", {
        rm_data_gh_repo_workflow (path)
    })
    cmts <- httptest2::with_mock_dir ("gh_api_issue_cmts", {
        rm_data_issue_comments_from_gh_api (path)
    })
    issues <- httptest2::with_mock_dir ("gh_api_issues", {
        rm_data_issues_from_gh_api (path)
    })
    libyears <- httptest2::with_mock_dir ("gh_libyears", {
        rm_data_libyears (path)
    })
    prs <- httptest2::with_mock_dir ("gh_api_prs", {
        rm_data_prs_from_gh_api (path)
    })
    releases <- httptest2::with_mock_dir ("gh_api_releases", {
        rm_data_releases_from_gh_api (path)
    })
    forks <- httptest2::with_mock_dir ("gh_api_forks", {
        rm_data_repo_forks (path)
    })
    repo_data <- httptest2::with_mock_dir ("gh_api_repo", {
        rm_data_repo_from_gh_api (path)
    })
    stargazers <- httptest2::with_mock_dir ("gh_api_stars", {
        rm_data_repo_stargazers (path)
    })

    # rm-data-user:
    login <- "mpadge"
    ended_at <- as.POSIXct ("2024-01-01T00:00:00")
    general <- httptest2::with_mock_dir ("gh_user_general", {
        gh_user_general (login)
    })
    followers <- httptest2::with_mock_dir ("gh_user_followers", {
        gh_user_follow (login, followers = TRUE, n_per_page = 1)
    })
    following <- httptest2::with_mock_dir ("gh_user_following", {
        gh_user_follow (login, followers = FALSE, n_per_page = 1)
    })
    user_commit_cmt <- httptest2::with_mock_dir ("gh_user_commit_cmt", {
        gh_user_commit_cmt (login, n_per_page = 1)
    })
    user_commits <- httptest2::with_mock_dir ("gh_user_commits", {
        gh_user_commits (login, n_per_page = 1, ended_at = ended_at)
    })
    user_issues <- httptest2::with_mock_dir ("gh_user_issues", {
        gh_user_issues (login, n_per_page = 1, ended_at = ended_at)
    })
    user_issue_cmts <- httptest2::with_mock_dir ("gh_user_issue_cmts", {
        gh_user_issue_cmts (login, n_per_page = 1)
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
    if (repo) {
        data_fns <- get_rm_data_fns ()
        res <- lapply (data_fns, function (i) {
            do.call (i, list (path = path))
        })
        names (res) <- gsub ("^rm\\_data\\_", "", data_fns)
        res$contributors <- get_all_contribs (
            res$contribs_from_log,
            res$contribs_from_gh_api
        )
    } else {
        data_fns <- get_rm_gh_user_fns () # length = 6
        pars <- list (
            gh_user_general = list (login = login),
            gh_user_followers =
                list (login = login, followers = TRUE, n_per_page = 1),
            gh_user_following =
                list (login = login, followers = FALSE, n_per_page = 1),
            gh_user_commit_cmt = list (login = login, n_per_page = 1),
            gh_user_commits =
                list (login = login, n_per_page = 1, ended_at = ended_at),
            gh_user_issues =
                list (login = login, n_per_page = 1, ended_at = ended_at),
            gh_user_issue_cmts = list (login = login, n_per_page = 1)
        ) # length = 7

        res <- lapply (seq_along (pars), function (i) {
            fn_i <- gsub ("follow.*$", "follow", names (pars) [i])
            do.call (fn_i, pars [[i]])
        })
        names (res) <- gsub ("^gh\\_user\\_", "", names (pars))
    }

    fs::dir_delete (path)

    return (res)
}
