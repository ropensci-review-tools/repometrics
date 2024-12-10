#' Extract and combine all user data
#'
#' @param path Path to local source repository.
#' @return A list of the following `data.frame` objects:
#' \enumerate{
#' \item `contribs_from_gh_api` with details of all code contributors from GitHub
#' \item `contribs_from_log` with details of all code contributors from the local git log
#' \item `dependencies` A simple `data.frame` of all package dependencies
#' \item `gh_repo_workflow` with details of all workflows run on GitHub,
#' including status of most recent runs
#' \item `gitlog` with one row for each git commit, and associated statistics
#' \item `issue_comments_from_gh_api` with details of all comments from all
#' repository issues on GitHub
#' \item `issues_from_gh_api` with details of all issues on GitHub
#' \item `libyears` The CHAOSS metric described at
#' \url{https://chaoss.community/kb/metric-libyears/}, measuring the relative
#' age of a project's dependencies, with lower values indicating more
#' up-to-date projects. This is the only item which is not a `data.frame`,
#' rather a named numerical vector of mean and median "libyears"
#' \item `prs_from_gh_api` with details of all pull requests on GitHub
#' \item `releases_from_gh_api` with details of all repository releases on GitHub
#' \item `repo_from_gh_api` A `data.frame` of a single line, with several key
#' attributes of the repository on GitHub.
#' }
#' @export
rm_data_user <- function (login) {

    checkmate::assert_character (login, len = 1L)

    data_fns <- get_rm_gh_user_fns ()

    if (all_gh_user_fns_memoised (data_fns, path)) {
        res <- lapply (data_fns, function (i) {
            do.call (i, list (login = login))
        })
    } else {
        res <- pbapply::pblapply (data_fns, function (i) {
            do.call (i, list (login = login))
        })
    }
    names (res) <- gsub ("^rm\\_data\\_", "", data_fns)

    return (res)
}

get_rm_gh_user_fns <- function () {

    pkg_fns <- ls (envir = asNamespace ("repometrics"))
    data_fns <- grep ("^gh\\_user\\_", pkg_fns, value = TRUE)
    data_fns <- data_fns [which (!grepl ("\\_(internal|qry)$", data_fns))]

    return (data_fns)
}

all_gh_user_fns_memoised <- function (data_fns, login) {
    is_memoised <- vapply (data_fns, function (i) {
        tryCatch (
            memoise::has_cache (get (i)) (login),
            error = function (e) FALSE
        )
    }, logical (1L))

    length (which (is_memoised)) > (length (data_fns) / 2)
}
