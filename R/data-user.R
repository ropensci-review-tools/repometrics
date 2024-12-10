#' Extract and combine all user data
#'
#' @param login GitHub login of user
#' @param ended_at Parameter used in some aspects of resultant data to limit
#' the end date of data collection. Defaults to `Sys.time()`.
#' @param nyears Parameter <= 1 determining fraction of a year over which data
#' up until `end_date` are collected.
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
#' @export
repometrics_user_data <- function (login,
                                   ended_at = Sys.time (),
                                   nyears = 1,
                                   n_per_page = 100) {

    checkmate::assert_character (login, len = 1L)

    data_fns <- get_rm_gh_user_fns ()
    pars <- list (
        login = login,
        n_per_page = n_per_page,
        ended_at = ended_at,
        nyears = nyears
    )

    if (all_gh_user_fns_memoised (data_fns, pars)) {
        res <- lapply (data_fns, function (i) {
            do.call (i, pars)
        })
    } else {
        res <- pbapply::pblapply (data_fns, function (i) {
            do.call (i, list (login = login))
        })
    }
    names (res) <- gsub ("^gh\\_user\\_", "", data_fns)

    names (res) <- gsub ("follow", "followers", names (res))
    res$following <- do.call (gh_user_follow, c (pars, followers = FALSE))

    i <- grep ("general", names (res))
    res <- c (res [i], res [-i] [order (names (res) [-i])])

    return (res)
}

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
                ended_at = pars$ended_at,
                nyears = pars$nyears
            ),
            error = function (e) FALSE
        )
    }, logical (1L))

    length (which (is_memoised)) > (length (data_fns) / 2)
}
