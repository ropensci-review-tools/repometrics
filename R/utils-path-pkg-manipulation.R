pkg_name_from_path <- function (path) {
    desc <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    checkmate::assert_file_exists (desc)

    unname (read.dcf (desc) [, "Package"])
}

pkg_gh_url_from_path <- function (path) {

    desc <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    checkmate::assert_file_exists (desc)

    desc <- read.dcf (desc)
    url <- NULL
    strip_url <- function (url) {
        url <- strsplit (unname (url), "\\n|,") [[1]]
        url <- gsub ("^[[:space:]]*", "", url)
        url <- gsub ("[[:space:]].*$", "", url)
        grep ("github\\.com", url, value = TRUE)
    }
    if ("URL" %in% colnames (desc)) {
        url <- strip_url (desc [, "URL"])
    }
    if (length (url) == 0L && "BugReports" %in% colnames (desc)) {
        url <- strip_url (desc [, "BugReports"])
        url <- gsub ("\\/issues(\\/?)$", "", url)
    }
    # No url listed in DESC, try git remote:
    if (length (url) == 0L) {
        remotes <- gert::git_remote_list (path)
        url <- grep ("github\\.com", remotes$url, value = TRUE)
    }
    return (url)
}

org_repo_from_path <- function (path) {

    url <- pkg_gh_url_from_path (path)
    if (length (url) == 0L) {
        return (FALSE)
    }

    url_parts <- strsplit (url, "\\/") [[1]]
    i <- which (url_parts == "github.com")
    if (length (i) == 0L || i > (length (url_parts) + 2L)) {
        return (FALSE)
    }
    org <- url_parts [i + 1L]
    repo <- url_parts [i + 2L]

    c (org, repo)
}
