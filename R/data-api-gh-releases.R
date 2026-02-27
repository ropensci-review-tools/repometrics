rm_data_releases_from_gh_api_internal <- function (path, # nolint
                                                   n_per_page = 100L,
                                                   latest_only = FALSE) {

    checkmate::assert_integerish (n_per_page)
    checkmate::assert_logical (latest_only)

    if (latest_only) {
        n_per_page <- 1L
    }

    u_endpoint <- gh_rest_api_endpoint (path = path, endpoint = "releases")
    body <- gh_rest_paginate (
        u_endpoint,
        n_per_page = n_per_page,
        max_pages = if (latest_only) 1L else Inf
    )

    null2char <- function (x) {
        ifelse (is.null (x), "", x)
    }

    data.frame (
        id = vapply (body, function (i) i$id, integer (1L)),
        author_login =
            vapply (body, function (i) null2char (i$author$login), character (1L)),
        author_id = vapply (body, function (i) i$author$id, integer (1L)),
        tag_name = vapply (body, function (i) null2char (i$tag_name), character (1L)),
        target_commitish =
            vapply (body, function (i) null2char (i$target_commitish), character (1L)),
        name = vapply (body, function (i) null2char (i$name), character (1L)),
        draft = vapply (body, function (i) i$draft, logical (1L)),
        prerelease = vapply (body, function (i) i$prerelease, logical (1L)),
        created_at = vapply (body, function (i) null2char (i$created_at), character (1L)),
        published_at =
            vapply (body, function (i) null2char (i$published_at), character (1L))
    )
}
rm_data_releases_from_gh_api <-
    memoise::memoise (rm_data_releases_from_gh_api_internal)
