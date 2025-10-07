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

has_gh_ci_tests <- function (path) {

    ci_data <- rm_data_repo_from_gh_api (path)
    h <- gert::git_log (repo = path, max = 1e6)
    any (ci_data$sha %in% h$commit)
}

# 'end_date' not used here, but specified for consistent interface to all
# metric fns.
rm_data_has_ci_internal <- function (path, end_date = NULL) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    has_ci <- ifelse (is_test_env, FALSE, has_gh_ci_tests (path))

    if (!has_ci) {
        ci_files <- repo_has_ci_files (path)
        has_ci <- length (ci_files) > 0L
        if (!has_ci && is_verbose ()) {
            cli::cli_alert_info (paste0 (
                "Unable to determine whether runs are ",
                "recent for CI service [{ci_files}]."
            ))
        }
    }

    return (has_ci)
}

rm_metric_has_ci <- function (path, end_date = NULL) {
    rm_data_has_ci_internal (path, end_date)
}

rm_metric_has_ci_url <- function () {
    "metric-test-coverage"
}
