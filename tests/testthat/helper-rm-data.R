# Helper function to use httptest2 mocks to load all data, after which function
# calls are memoised and so can be called without mocking from that point on.

mock_rm_data <- function (repo = TRUE) {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

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
    coverage <- httptest2::with_mock_dir ("coverage", {
        cm_data_test_coverage (path)
    })

    # rm-data-user:
    logins <- c ("gaborcsardi", "hfrick", "mpadge")
    end_date <- as.Date ("2024-01-01")
    for (login in logins) {
        prfx <- paste0 ("gh_user", match (login, logins), "_")
        pars <- list (
            login = login,
            n_per_page = 1L,
            end_date = end_date,
            nyears = 1
        )
        general <- httptest2::with_mock_dir (paste0 (prfx, "general"), {
            do.call (gh_user_general, pars)
        })
        followers <- httptest2::with_mock_dir (paste0 (prfx, "followers"), {
            do.call (gh_user_follow, c (pars, followers = TRUE))
        })
        following <- httptest2::with_mock_dir (paste0 (prfx, "following"), {
            do.call (gh_user_follow, c (pars, followers = FALSE))
        })
        user_commit_cmt <- httptest2::with_mock_dir (paste0 (prfx, "commit_cmt"), {
            do.call (gh_user_commit_cmt, pars)
        })
        user_commits <- httptest2::with_mock_dir (paste0 (prfx, "commits"), {
            do.call (gh_user_commits, pars)
        })
        user_issues <- httptest2::with_mock_dir (paste0 (prfx, "issues"), {
            do.call (gh_user_issues, pars)
        })
        user_issue_cmts <- httptest2::with_mock_dir (paste0 (prfx, "issue_cmts"), {
            do.call (gh_user_issue_cmts, pars)
        })
    }

    # cran_downloads fn needs modified DESC:
    desc_path <- fs::path (path, "DESCRIPTION")
    desc <- readLines (desc_path)
    desc [1] <- "Package: goodpractice"
    writeLines (desc, desc_path)

    # test-cm-metrics.R only tests cran downloads for this one date:
    end_date_cran <- as.Date ("2024-08-01")
    dl <- httptest2::with_mock_dir ("cran_dl", {
        cm_metric_cran_downloads (path, end_date = end_date_cran)
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
        data_fns <- get_rm_gh_user_fns ()
        logins <- c ("gaborcsardi", "hfrick")
        res <- lapply (logins, function (login) {
            pars <- list (
                login = login,
                n_per_page = 1L,
                end_date = end_date,
                nyears = 1
            )

            res_i <- lapply (data_fns, function (i) {
                do.call (i, pars)
            })
            names (res_i) <- gsub ("^gh\\_user\\_", "", data_fns)

            names (res_i) <- gsub ("follow", "followers", names (res_i))
            res_i$following <- do.call (gh_user_follow, c (pars, followers = FALSE))

            i <- grep ("general", names (res_i))
            res_i <- c (res_i [i], res_i [-i] [order (names (res_i) [-i])])
            return (res_i)
        })
        names (res) <- logins
    }

    fs::dir_delete (path)

    return (res)
}
