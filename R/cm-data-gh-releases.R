releases_from_gh_api <- function (path) {

    u_endpoint <- gh_rest_api_endpoint (path = path, endpoint = "releases")

    req <- httr2::request (u_endpoint)
    req <- add_gh_token_to_req (req)
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    body <- httr2::resp_body_json (resp)

    data.frame (
        id = vapply (body, function (i) i$id, integer (1L)),
        author_login = vapply (body, function (i) i$author$login, character (1L)),
        author_id = vapply (body, function (i) i$author$id, integer (1L)),
        tag_name = vapply (body, function (i) i$tag_name, character (1L)),
        target_commitish = vapply (body, function (i) i$target_commitish, character (1L)),
        name = vapply (body, function (i) i$name, character (1L)),
        draft = vapply (body, function (i) i$draft, logical (1L)),
        prerelease = vapply (body, function (i) i$prerelease, logical (1L)),
        created_at = vapply (body, function (i) i$created_at, character (1L)),
        published_at = vapply (body, function (i) i$published_at, character (1L))
    )
}
