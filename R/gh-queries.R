#' Use the GitHub Rest API activity list to extract event types.
#' Activity requests are described at
#' \url{https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#list-repository-activities}
#' and the list of all event types is at
#' \url{https://docs.github.com/en/rest/using-the-rest-api/github-event-types?apiVersion=2022-11-28}.
#' @noRd
github_issues_prs_query <- function (org = NULL, repo = NULL) {

    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"

    u_events <- gh_rest_api_endpoint (orgrepo = c (org, repo), endpoint = "events")

    req <- req0 <- httr2::request (u_events) |>
        add_token_to_req () |>
        httr2::req_url_query (per_page = ifelse (is_test_env, 2, 100))

    body <- NULL
    next_page <- 1
    while (!is.null (next_page)) {

        resp <- httr2::req_perform (req)
        httr2::resp_check_status (resp)

        this_body <- httr2::resp_body_json (resp)
        body <- c (body, this_body)

        next_page <- gh_next_page (resp)
        if (is_test_env) {
            next_page <- NULL
        }
        req <- httr2::req_url_query (req0, page = next_page)
    }

    # Extraction function for single fields which may not be present
    extract_one <- function (body, field = "action", naval = NA_character_) {
        ret_type <- do.call (typeof (naval), list (1L))
        vapply (body, function (i) {
            ifelse (field %in% names (i$payload), i$payload [[field]], naval)
        }, ret_type)
    }

    # Extraction function for doubly-nexted fields which may not be present
    extract_two <- function (body,
                             field1 = "pull_request",
                             field2 = "comments",
                             naval = NA_character_) {

        ret_type <- do.call (typeof (naval), list (1L))
        vapply (body, function (i) {
            ret <- naval
            if (field1 %in% names (i$payload)) {
                if (field2 %in% names (i$payload [[field1]])) {
                    ret <- i$payload [[field1]] [[field2]]
                }
            }
            ifelse (is.null (ret), naval, ret)
        }, ret_type)
    }

    # Items which are always present:
    ids <- vapply (body, function (i) i$id, character (1L))
    type <- vapply (body, function (i) i$type, character (1L))
    login <- vapply (body, function (i) i$actor$login, character (1L))

    # Single-nested items:
    action <- extract_one (body, "action", NA_character_)
    number <- extract_one (body, "number", NA_integer_)

    # Doubly-nested items:
    num_comments <- extract_two (body, "pull_request", "comments", NA_integer_)
    num_review_comments <-
        extract_two (body, "pull_request", "review_comments", NA_integer_)
    commits <- extract_two (body, "pull_request", "commits", NA_integer_)
    additions <- extract_two (body, "pull_request", "additions", NA_integer_)
    deletions <- extract_two (body, "pull_request", "deletions", NA_integer_)
    changed_files <-
        extract_two (body, "pull_request", "changed_files", NA_integer_)
    created_at <-
        extract_two (body, "pull_request", "created_at", NA_character_)
    created_at <- to_posix (created_at)
    merged_at <-
        extract_two (body, "pull_request", "created_at", NA_character_)
    merged_at <- to_posix (merged_at)

    data.frame (
        id = ids,
        type = type,
        login = login,
        action = action,
        number = number,
        commits = commits,
        num_comments = num_comments,
        num_review_comments = num_review_comments,
        additions = additions,
        deletions = deletions,
        changed_files = changed_files,
        created_at = created_at,
        merged_at = merged_at
    )
}
