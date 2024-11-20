# nocov start
get_gh_token <- function () {
    e <- Sys.getenv ()
    nms <- names (e)
    tok <- unique (e [grep ("GITHUB", nms)])
    if (length (tok) != 1L) {
        tok <- unique (e [grep ("GITHUB\\_(PAT|TOK)", nms)])
    }
    if (length (tok) != 1L) {
        cli::cli_abort (
            "Unable to determine unique GitHub token from environment variables"
        )
    }
    return (tok)
}
# nocov end

add_gh_token_to_req <- function (req) {
    if (!nzchar (Sys.getenv ("GITHUB_WORKFLOW"))) {
        tok <- get_gh_token ()
        headers <- list (Authorization = paste0 ("Bearer ", tok))
        req <- httr2::req_headers (req, "Authorization" = headers)
    }

    return (req)
}
