pkg_name_from_path <- function (path) {
    desc <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    checkmate::assert_file_exists (desc)

    unname (read.dcf (desc) [, "Package"])
}

pkg_gh_url_from_path <- function (path) {

    desc <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    checkmate::assert_file_exists (desc)

    desc <- read.dcf (desc)
    ret <- NULL
    if ("URL" %in% colnames (desc)) {
        url <- strsplit (unname (desc [, "URL"]), "\\n|,") [[1]]
        url <- gsub ("^[[:space:]]*", "", url)
        url <- gsub ("[[:space:]].*$", "", url)
        ret <- grep ("github\\.com", url, value = TRUE)
    }
    return (ret)
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
