#' Return names of CI services from corresponding configuration files contained
#' in local repo.
#' @noRd
repo_has_ci_files <- function (path) {

    flist <- fs::dir_ls (path, type = "file", all = TRUE, recurse = TRUE)
    ptns <- c (
        "\\.appveyor\\.yml",
        "\\.github\\/workflows",
        "\\.gitlab\\-ci\\.yml",
        "\\.circleci\\/config\\.yml",
        "codefresh\\.yml",
        "drone\\.yml",
        "\\.travis\\.yml"
    )
    has_it <- vapply (ptns, function (i) any (grepl (i, flist)), logical (1L))
    nms <- gsub ("\\\\.|\\\\.y.*$|\\\\/.*$|\\\\-.*$", "", ptns)

    return (nms [which (has_it)])
}
