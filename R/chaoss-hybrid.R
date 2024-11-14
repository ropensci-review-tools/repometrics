# Hybird metrics from both internal structure and external data

chaoss_metric_has_ci <- function (path) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    has_ci <- ifelse (is_test_env, FALSE, has_gh_ci_tests (path))

    if (!has_ci) {
        ci_files <- repo_has_ci_files (path)
        has_ci <- length (ci_files) > 0L
        if (has_ci) {
            cli::cli_alert_info (
                "Unable to determine whether runs are recent for CI service [{ci_files}]."
            )
        }
    }

    return (has_ci)
}
