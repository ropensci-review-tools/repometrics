#' Get repository data from GitHub API
#'
#' @param path Local path to repository
#' @noRd
rm_data_repo_from_gh_api_internal <- function (path) { # nolint

    or <- org_repo_from_path (path)

    u_base <- "https://api.github.com/repos/"
    u_org_repo <- paste0 (u_base, or [1], "/", or [2])

    req <- httr2::request (u_org_repo)

    req <- add_gh_token_to_req (req)
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    body <- httr2::resp_body_json (resp)

    topics <- paste0 (unlist (body$topics), collapse = ", ")

    data.frame (
        id = body$id,
        name = body$name,
        full_name = body$full_name,
        owner = body$owner$login,
        url = body$html_url,
        description = null2na_char (body$description),
        is_fork = body$fork,
        created_at = body$created_at,
        updated_at = body$updated_at,
        homepage = null2na_char (body$homepage),
        size = body$size,
        stargazers_count = body$stargazers_count,
        subscribers_count = body$subscribers_count,
        language = null2na_char (body$language),
        forks_count = body$forks_count,
        open_issues_count = body$open_issues_count,
        topics = topics,
        default_branch = null2na_char (body$default_branch)
    )
}
rm_data_repo_from_gh_api <- memoise::memoise (rm_data_repo_from_gh_api_internal)
