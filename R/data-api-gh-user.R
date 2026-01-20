#' Extract and combine data on all contributors to a repository.
#'
#' This forms part of the data collated by the main \link{repometrics_data}
#' function, along with data on repository structure and historical developed
#' extracted by the \link{repometrics_data_repo} function.
#'
#' @inheritParams repometrics_data
#' @param login GitHub login of user
#' @param n_per_page Number of items per page to pass to GitHub GraphQL API
#' requests. This should never need to be changed.
#' @return A list of the following `data.frame` objects:
#' \enumerate{
#' \item `commit_cmt` with details of commits made on commits
#' \item `commits` with summaries of all repositories to which user made commits
#' \item `followers` A list of followers of specified user
#' \item `following` A list of other people who nominated user is following
#' \item `general` with some general information about specified user
#' \item `issue_cmts` with information on all issue comments made by user
#' \item `issues` with information on all issues opened by user
#' }
#'
#' @family data
#' @export
repometrics_data_user <- function (login,
                                   end_date = Sys.Date (),
                                   nyears = 1,
                                   n_per_page = 100) {

    repometrics_data_user_memoised (login, end_date, nyears, n_per_page)
}

repometrics_data_user_internal <- function (login,
                                            end_date = Sys.Date (),
                                            nyears = 1,
                                            n_per_page = 100) {

    checkmate::assert_character (login, len = 1L)
    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    if (is_test_env) {
        n_per_page <- 1L
    }

    data_fns <- get_rm_gh_user_fns ()
    pars <- list (
        login = login,
        n_per_page = n_per_page,
        end_date = end_date,
        nyears = nyears
    )

    if (all_gh_user_fns_memoised (data_fns, pars)) {
        res <- lapply (data_fns, function (i) {
            do.call (i, pars)
        })
    } else {
        res <- pbapply::pblapply (data_fns, function (i) {
            do.call (i, pars)
        })
    }
    names (res) <- gsub ("^gh\\_user\\_", "", data_fns)

    names (res) <- gsub ("follow", "followers", names (res))
    res$following <- do.call (gh_user_follow, c (pars, followers = FALSE))

    i <- grep ("general", names (res))
    res <- c (res [i], res [-i] [order (names (res) [-i])])

    return (res)
}

repometrics_data_user_memoised <- memoise::memoise (repometrics_data_user_internal)

get_rm_gh_user_fns <- function () {

    pkg_fns <- ls (envir = asNamespace ("repometrics"))
    data_fns <- grep ("^gh\\_user\\_", pkg_fns, value = TRUE)
    data_fns <- data_fns [which (!grepl ("\\_(internal|qry)$", data_fns))]

    return (data_fns)
}

all_gh_user_fns_memoised <- function (data_fns, pars) {
    is_memoised <- vapply (data_fns, function (i) {
        tryCatch (
            memoise::has_cache (get (i)) (
                login = pars$login,
                n_per_page = pars$n_per_page,
                end_date = pars$end_date,
                nyears = pars$nyears
            ),
            error = function (e) FALSE
        )
    }, logical (1L))

    length (which (is_memoised)) > (length (data_fns) / 2)
}

# All functions accept same parameters, even through not all parameters are
# used in all functions.

gh_user_general_qry <- function (login = "") {

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            name
            location
            bio
            company
            email
            organizations (first: 100) {
                nodes {
                    location
                    name
                    resourcePath
                    url
                    websiteUrl
                    membersWithRole (first: 1) {
                        totalCount
                    }
                }
            }
            avatarUrl
            repositories (isFork: false, first: 1) {
                totalCount
            }
            repositoriesContributedTo (first: 1) {
                totalCount
            }
            issues (first: 1) {
                totalCount
            }
            pullRequests (first: 1) {
                totalCount
            }
            starredRepositories (first: 1) {
                totalCount
            }
        }
    }")

    return (q)
}

# Only uses `login` param:
gh_user_general_internal <- function (login = "",
                                      end_date = Sys.Date (),
                                      nyears = 1,
                                      n_per_page = 100L) {

    q <- gh_user_general_qry (login = login)
    dat <- gh::gh_gql (query = q)

    user <- dat$data$user

    user_dat <- data.frame (
        login = null2na_char (user$login),
        name = null2na_char (user$name),
        email = null2na_char (user$email),
        location = null2na_char (user$location),
        company = null2na_char (user$company),
        bio = null2na_char (user$bio),
        avatarUrl = null2na_char (user$avatarUrl),
        num_repositories = null2na_int (user$repositories$totalCount),
        repos_contributed_to =
            null2na_int (user$repositoriesContributedTo$totalCount),
        num_issues_opened = null2na_int (user$issues$totalCount),
        num_prs_opened = null2na_int (user$pullRequests$totalCount),
        num_starred_repos = null2na_int (user$starredRepositories$totalCount)
    )

    orgs <- user$organizations$nodes

    org_name <- org_gh_org <- org_url <-
        org_web_url <- org_location <- character (0L)
    org_num_members <- integer (0L)

    nms <- c (
        "name", "resourcePath", "url",
        "websiteUrl", "location", "membersWithRole"
    )
    has_names <- vapply (
        orgs,
        function (i) all (nms %in% names (i)),
        logical (1L)
    )

    if (length (orgs) > 0L && all (has_names)) {

        org_name <- vapply (orgs, function (i) i$name, character (1L))
        org_gh_org <- vapply (orgs, function (i) i$resourcePath, character (1L))
        org_url <- vapply (orgs, function (i) i$url, character (1L))

        org_web_url <- vapply (
            orgs,
            function (i) null2na_char (i$websiteUrl),
            character (1L)
        ) |> null2na_char ()
        org_location <- vapply (
            orgs,
            function (i) null2na_char (i$location),
            character (1L)
        ) |> null2na_char ()
        org_num_members <- vapply (
            orgs,
            function (i) i$membersWithRole$totalCount,
            integer (1L)
        )
    }

    orgs <- data.frame (
        name = org_name,
        gh_org = org_gh_org,
        url = org_url,
        web_url = org_web_url,
        location = org_location,
        num_members = org_num_members
    )

    list (user = user_dat, orgs = orgs)
}
gh_user_general <- memoise::memoise (gh_user_general_internal)
