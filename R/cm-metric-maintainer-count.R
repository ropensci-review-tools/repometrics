#' CHAOSS metric "maintainer count", here both as listed in "DESCRIPTION", and
#' taken as a static value, and also potentially reduced version of that,
#' counting only those who were active either in commits, issues, or pull
#' requests over the standard time period.
#' @noRd
cm_metric_maintainer_count <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding note:
    user_login <- merged_by <- participants <- created_at <-
        assignee <- closed_by <- NULL

    requireNamespace ("desc", quiety = TRUE)

    checkmate::assert_date (end_date)

    start_date <- end_date - get_repometrics_period ()

    auts <- get_desc_authors (path)

    # Then list gh hanldes of all those who contributed to issues or prs:
    prs <- get_prs_in_period (path, end_date) # in cm-metrics-change-req.R
    pr_revs <- lapply (prs$reviews, function (i) i$login) |>
        unlist ()
    pr_ctbs <- dplyr::select (prs, user_login, merged_by, participants) |>
        as.list () |>
        unlist ()
    pr_ctbs <- strsplit (pr_ctbs, ",") |>
        unlist () |>
        unname ()

    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    issue_auts <- dplyr::select (issues, user_login, assignee, closed_by) |>
        as.list () |>
        unlist () |>
        unname ()
    issue_auts <- issue_auts [which (!is.na (issue_auts))]

    issue_cmts <- rm_data_issue_comments_from_gh_api (path) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)
    issue_cmt_ctbs <- issue_cmts$user_login

    gh_auts <- unique (c (pr_revs, pr_ctbs, issue_auts, issue_cmt_ctbs))

    num_gh_auts <- match_repo_ctbs_to_desc (path, auts, gh_auts)

    c (total = nrow (auts), recent = num_gh_auts)
}

#' Match repo contributors from both git log and GitHub API to author names and
#' emails given in "DESCRIPTION" file.
#'
#' @return The number of authors listed in "DESCRIPTION" file who have
#' contributed in any recorded way in the default time period.
#' @noRd
match_repo_ctbs_to_desc <- function (path, desc_auts, gh_auts) {

    # Suppress no visible binding note:
    login <- name <- email <- handle <- NULL

    ctbs_from_log <- rm_data_contribs_from_log (path) |>
        dplyr::rename (name = handle) |>
        dplyr::mutate (login = NA_character_)
    ctbs_from_gh <- rm_data_contribs_from_gh_api (path) |>
        dplyr::filter (login %in% gh_auts) |>
        dplyr::select (login, name, email) |>
        dplyr::bind_rows (ctbs_from_log) |>
        unique ()

    index_from_log <- cbind (
        match_string_vecs (desc_auts$name, ctbs_from_log$name),
        match_string_vecs (desc_auts$email, ctbs_from_log$email)
    )
    index_from_gh <- cbind (
        match_string_vecs (desc_auts$name, ctbs_from_gh$name),
        match_string_vecs (desc_auts$email, ctbs_from_gh$email)
    )
    n_from_log <- n_from_gh <- 0L
    if (length (index_from_log) > 0L) {
        n_from_log <- apply (index_from_log, 1, function (i) any (!is.na (i))) |>
            which () |>
            length ()
    }
    if (length (index_from_gh) > 0L) {
        n_from_gh <- apply (index_from_gh, 1, function (i) any (!is.na (i))) |>
            which () |>
            length ()
    }

    return (max (c (n_from_log, n_from_gh)))
}



get_desc_authors <- function (path, roles = c ("cre", "aut")) {

    desc_path <- fs::dir_ls (path, type = "file", regexp = "DESCRIPTION$")
    checkmate::assert_file_exists (desc_path)

    roles <- paste0 (roles, collapse = "|")
    auts <- tryCatch (
        desc::desc_get_authors (desc_path),
        error = function (e) NULL
    )
    if (is.null (auts)) {
        desc_dcf <- read.dcf (desc_path)
        aut <- grep ("^aut", colnames (desc_dcf), ignore.case = TRUE)
        if (length (aut) == 1L) {
            auts <- desc_dcf [aut]
        }
    }
    aut_names <- auts
    if (any (grepl ("\\[", auts))) {
        desc_roles <- gsub ("^.*\\[", "[", auts)
        auts <- gsub ("(\\s*?)\\[.*$", "", auts [grep (roles, desc_roles)])
        aut_names <- gsub ("(\\s*?)<.*$", "", auts)
    }
    aut_emails <- ""
    if (any (grepl ("<", auts))) {
        aut_emails <- gsub ("^.*<|>(\\s?)$", "", auts)
    }
    if (length (aut_names) == 0L || length (aut_emails) == 0L) {
        aut_names <- aut_emails <- character (0L)
    }

    data.frame (name = aut_names, email = aut_emails)
}
