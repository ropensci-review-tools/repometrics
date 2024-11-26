#' Retrieve latest GitHub workflow results from Rest API
#'
#' This uses default of 30 most recent results.
#' @noRd
cm_data_gh_repo_workflow_internal <- function (org = NULL, repo = NULL, n = 30L) {

    checkmate::assert_integer (n, lower = 1L)
    u_wf <- gh_rest_api_endpoint (orgrepo = c (org, repo), endpoint = "actions/runs")

    req <- httr2::request (u_wf) |>
        add_gh_token_to_req () |>
        httr2::req_url_query (per_page = n)

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    body <- httr2::resp_body_json (resp)
    workflows <- body$workflow_runs

    ids <- vapply (workflows, function (i) i$id, numeric (1L))
    names <- vapply (workflows, function (i) i$name, character (1L))
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
    conclusion <- vapply (workflows, function (i) null2na_char (i$conclusion), character (1L))
    created <- vapply (workflows, function (i) i$created_at, character (1L))
    created <- to_posix (created)

    data.frame (
        name = names,
        id = ids,
        sha = shas,
        title = titles,
        status = status,
        conclusion = conclusion,
        created = created
    )
}
cm_data_gh_repo_workflow <- memoise::memoise (cm_data_gh_repo_workflow_internal)
