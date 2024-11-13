github_repo_files_query <- function (org = NULL, repo = NULL) {

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
            object(expression: \"HEAD:\") {
                ... on Tree {
                    entries {
                        name
                        type
                        mode
                        object {
                            ... on Commit {
                                oid
                                committedDate
                            }
                            ... on Blob {
                                byteSize
                                isBinary
                            }
                        }
                    }
                }
            }
        }
    }")
}

#' Retrieve latest GitHub workflow results from Rest API
#'
#' This uses default of 30 most recent results, and returns the most recent
#' result for each unique workflow.
#' @noRd
github_repo_workflow_query <- function (org = NULL, repo = NULL) {

    u_base <- "https://api.github.com/repos/"
    u_repo <- paste0 (u_base, org, "/", repo, "/")
    u_wf <- paste0 (u_repo, "actions/runs")

    tok <- get_gh_token ()
    headers <- list (Authorization = paste0 ("Bearer ", tok))

    req <- httr2::request (u_wf) |>
        httr2::req_headers ("Authorization" = headers)
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    body <- httr2::resp_body_json (resp)
    workflows <- body$workflow_runs

    ids <- vapply (workflows, function (i) i$id, numeric (1L))
    names <- vapply (workflows, function (i) i$name, character (1L))
    sha <- vapply (workflows, function (i) i$head_sha, character (1L))
    titles <- vapply (workflows, function (i) i$display_title, character (1L))
    status <- vapply (workflows, function (i) i$status, character (1L))
    conclusion <- vapply (workflows, function (i) i$conclusion, character (1L))
    created <- vapply (workflows, function (i) i$created_at, character (1L))

    data.frame (
        name = names,
        id = ids,
        sha = sha,
        title = titles,
        status = status,
        conclusion = conclusion,
        created = as.Date (created)
    ) |>
        dplyr::group_by (name) |>
        dplyr::filter (dplyr::row_number () == 1L)
}
