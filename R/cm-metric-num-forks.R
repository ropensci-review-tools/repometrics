cm_metric_num_forks <- function (path) {
    repo_dat <- cm_data_repo_from_gh_api (path)
    return (repo_dat$forks_count)
}
