#' Extract and combine all user data
#'
#' @param login GitHub login of user
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
rm_data_user <- function (login) {

    checkmate::assert_character (login, len = 1L)

    data_fns <- get_rm_gh_user_fns ()

    if (all_gh_user_fns_memoised (data_fns, login)) {
        res <- lapply (data_fns, function (i) {
            do.call (i, list (login = login))
        })
    } else {
        res <- pbapply::pblapply (data_fns, function (i) {
            do.call (i, list (login = login))
        })
    }
    names (res) <- gsub ("^gh\\_user\\_", "", data_fns)
    gsub ("^gh\\_user\\_", "", data_fns)

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
