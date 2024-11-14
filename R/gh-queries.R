#' Retrieve latest GitHub workflow results from Rest API
#'
#' This uses default of 30 most recent results.
#' @noRd
github_repo_workflow_query <- function (org = NULL, repo = NULL, n = 30L) {

    checkmate::assert_integer (n, lower = 1L)
    u_base <- "https://api.github.com/repos/"
    u_repo <- paste0 (u_base, org, "/", repo, "/")
    u_wf <- paste0 (u_repo, "actions/runs?per_page=", n)

    req <- httr2::request (u_wf)

    if (!nzchar (Sys.getenv ("GITHUB_WORKFLOW"))) {
        tok <- get_gh_token ()
        headers <- list (Authorization = paste0 ("Bearer ", tok))
        req <- httr2::req_headers (req, "Authorization" = headers)
    }

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    body <- httr2::resp_body_json (resp)
    workflows <- body$workflow_runs

    ids <- vapply (workflows, function (i) i$id, numeric (1L))
    names <- vapply (workflows, function (i) i$name, character (1L))
    shas <- vapply (workflows, function (i) i$head_sha, character (1L))
    titles <- vapply (workflows, function (i) i$display_title, character (1L))
    status <- vapply (workflows, function (i) i$status, character (1L))
    conclusion <- vapply (workflows, function (i) i$conclusion, character (1L))
    created <- vapply (workflows, function (i) i$created_at, character (1L))
    created <- as.POSIXct (created, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

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

#' Use the GitHub Rest API activity list to extract event types.
#' Activity requests are described at
#' \url{https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#list-repository-activities}
#' and the list of all event types is at
#' \url{https://docs.github.com/en/rest/using-the-rest-api/github-event-types?apiVersion=2022-11-28}.
#' @noRd
github_issues_prs_query <- function (org = NULL, repo = NULL) {

    period <- get_repometrics_period ()

    u_base <- "https://api.github.com/repos/"
    u_repo <- paste0 (u_base, org, "/", repo, "/")
    u_wf <- paste0 (u_repo, "activity?per_page=100")

    body <- NULL
    this_url <- u_wf
    while (!is.null (this_url)) {

        req <- httr2::request (this_url) |>
            add_token_to_req ()

        resp <- httr2::req_perform (req)
        httr2::resp_check_status (resp)

        this_body <- httr2::resp_body_json (resp)
        body <- c (body, this_body)
        this_url <- get_next_link (resp)
    }

    ids <- vapply (body, function (i) i$id, numeric (1L))
    activity_type <- vapply (body, function (i) i$activity_type, character (1L))
    login <- vapply (body, function (i) i$actor$login, character (1L))
    timestamp <- vapply (body, function (i) i$timestamp, character (1L))
    timestamp <- as.POSIXct (timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

    data.frame (
        id = ids,
        activity_type = activity_type,
        login = login,
        timestamp = timestamp
    )
}

add_token_to_req <- function (req) {

    if (!nzchar (Sys.getenv ("GITHUB_WORKFLOW"))) {
        tok <- get_gh_token ()
        headers <- list (Authorization = paste0 ("Bearer ", tok))
        req <- httr2::req_headers (req, "Authorization" = headers)
    }

    return (req)
}

#' Pagination for Rest API. see
#' https://docs.github.com/en/rest/using-the-rest-api/using-pagination-in-the-rest-api
#' @noRd
get_next_link <- function (resp) {

    link <- httr2::resp_headers (resp)$link

    next_url <- NULL

    if (!is.null (link)) {
        next_ptn <- "rel\\=\\\"next"
        if (grepl (next_ptn, link)) {
            # "next" is always first; where there are multiples, "prev" comes
            # after "next"
            ptn <- "<([^>]+)>"
            next_url <- regmatches (link, regexpr (ptn, link))
            next_url <- gsub ("<|>", "", next_url)
            if (!grepl ("after", next_url)) {
                cli::cli_abort ("Pagination link in GitHub Rest API malformed: [{next_url}]")
            }
        }
    }

    return (next_url)
}
