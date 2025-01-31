#' CHAOSS metric for "Types of Contributions", which assesses the diversity of
#' contribution types.
#'
#' This is greatly simplified here, to measure a single proportion of
#' propoertions of unique GitHub users contributing to issues who are not
#' otherwise main contributors.
#'
#' \url{https://chaoss.community/kb/metric-types-of-contributions/}.
#'
#' @noRd
cm_metric_ctb_diversity <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding notes:
    updated_at <- NULL

    start_date <- end_date - get_repometrics_period ()

    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::mutate (updated_at = as.Date (updated_at)) |>
        dplyr::filter (updated_at >= start_date & updated_at <= end_date)
    # issue_aut <- unique (c (issues$user_login, issues$closed_by))
    # Main issue "authors" are taken only from those who close issues, as
    # issue-opening needs to be considered here as potentially unique external
    # contribution.
    issue_aut <- issues$closed_by [which (!is.na (issues$closed_by))]

    issue_cmts <- rm_data_issue_comments_from_gh_api (path) |>
        dplyr::mutate (updated_at = as.Date (updated_at)) |>
        dplyr::filter (updated_at >= start_date & updated_at <= end_date)
    cmt_aut <- unique (issue_cmts$user_login)
    cmt_aut <- cmt_aut [which (!is.na (cmt_aut))]

    prs <- rm_data_prs_from_gh_api (path) |>
        dplyr::mutate (updated_at = as.Date (updated_at)) |>
        dplyr::filter (updated_at >= start_date & updated_at <= end_date)
    pr_aut <- unique (c (prs$user_login, prs$merged_by))

    main_auts <- unique (c (issue_aut, pr_aut))

    return (length (setdiff (cmt_aut, main_auts)) / length (main_auts))
}
