#' Start quarto dashboard with results of
#' `repometrics_collate_org_data` function for collation of data across orgs.
#'
#' @param data_org Data on GitHub organization as returned from
#' `repometrics_collate_org_data` function.
#' @param action One of "preview", to start and open a live preview of the
#' dashboard website, or "render" to render a static version without previewing
#' or opening.
#' @return (Invisibly) Path to main "index.html" document of quarto site. Note
#' that the site must be served with `action = "preview"`, and will not work by
#' simply opening this "index.html" file.
#' @export
orgmetrics_dashboard <- function (data_org, data_users, action = "preview") {

    requireNamespace ("brio")
    requireNamespace ("jsonlite")
    requireNamespace ("quarto")
    requireNamespace ("withr")

    action <- match.arg (action, c ("preview", "render"))
    quarto_action <- paste0 ("quarto::quarto_", action)

    path_src <- system.file ("extdata", "quarto-org", package = "repometrics")
    path_dest <- fs::path (fs::path_temp (), "quarto-org")
    dir <- fs::dir_copy (path_src, path_dest, overwrite = TRUE)

    withr::with_dir (dir, {
        do.call (eval (parse (text = quarto_action)), list ())
    })
}
