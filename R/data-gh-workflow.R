#' Retrieve latest GitHub workflow results from Rest API
#'
#' @noRd
rm_data_gh_repo_workflow_internal <- function (path, n_per_page = 30L) { # nolint

    n_per_page <- n_per_page_in_tests (n_per_page)
    or <- org_repo_from_path (path)

    # The workflow run qquery needs an additional parameter to return full
    # results. Easiest here is the default branch:
    repo_dat <- rm_data_repo_from_gh_api (path)

    checkmate::assert_integer (n_per_page, lower = 1L)
    u_wf <- gh_rest_api_endpoint (orgrepo = or, endpoint = "actions/runs")

    req <- httr2::request (u_wf) |>
        add_gh_token_to_req () |>
        httr2::req_url_query (
            per_page = n_per_page,
            branch = repo_dat$default_branch
        )

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    body <- httr2::resp_body_json (resp)
    workflows <- body$workflow_runs

    ids <- vapply (workflows, function (i) i$id, numeric (1L))
    names <- vapply (workflows, function (i) i$name, character (1L))
    logs_url <- vapply (workflows, function (i) i$logs_url, character (1L))
    shas <- vapply (workflows, function (i) i$head_sha, character (1L))
    titles <- vapply (
        workflows,
        function (i) null2na_char (i$display_title),
        character (1L)
    )
    status <- vapply (
        workflows,
        function (i) null2na_char (i$status),
        character (1L)
    )
    conclusion <- vapply (
        workflows,
        function (i) null2na_char (i$conclusion),
        character (1L)
    )
    created <- vapply (workflows, function (i) i$created_at, character (1L))
    created <- to_posix (created)

    data.frame (
        name = names,
        id = ids,
        sha = shas,
        logs_url = logs_url,
        title = titles,
        status = status,
        conclusion = conclusion,
        created = created
    )
}
rm_data_gh_repo_workflow <- memoise::memoise (rm_data_gh_repo_workflow_internal)


coverage_from_one_log <- function (log_url) {

    cov <- NA_real_

    req <- httr2::request (log_url) |>
        add_gh_token_to_req ()

    path <- fs::path (fs::path_temp (), "temp.zip")
    resp <- tryCatch (
        httr2::req_perform (req, path = path),
        error = function (e) NULL
    )
    if (is.null (resp)) {
        return (cov)
    }

    httr2::resp_check_status (resp)

    if (fs::file_exists (path)) {
        dirs_old <- fs::dir_ls (fs::path_temp (), type = "dir")
        flist <- utils::unzip (path, exdir = fs::path_temp ())
        dirs_new <- fs::dir_ls (fs::path_temp (), type = "dir")
        dirs_new <- dirs_new [which (!dirs_new %in% dirs_old)]

        f_cov <- grep ("coverage\\.txt", flist, value = TRUE)
        cov <- unlist (lapply (f_cov, function (f) {
            txt <- readr::read_lines (f, progress = FALSE)
            grep ("coverage\\s*:", txt, value = TRUE, ignore.case = TRUE)
        }))
        cov <- gsub ("^.*coverage\\s*:|%", "", cov, ignore.case = TRUE)
        cov <- as.numeric (cov)
        cov <- unique (cov [which (!is.na (cov))])

        fs::file_delete (flist)
        fs::file_delete (path)
        fs::dir_delete (dirs_new)
    }

    cov <- ifelse (length (cov) == 0L, NA_real_, max (cov))

    return (cov)
}
