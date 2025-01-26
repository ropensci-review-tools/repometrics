cm_metric_issue_response_time <- function (path, end_date = Sys.Date ()) {

    # suppress no visible warning notes:
    user_login <- issue_number <- created_at <- response_date <- NULL

    ctbs_main <- rm_data_contribs_from_gh_api (path)
    ctbs_main <- unique (ctbs_main$login)

    issues <- rm_data_issues_from_gh_api (path)
    issues <- issues [grep ("issues", issues$url), ]
    comments <- rm_data_issue_comments_from_gh_api (path)

    cmt_responses <- dplyr::filter (comments, user_login %in% ctbs_main) |>
        dplyr::group_by (issue_number) |>
        dplyr::summarise (
            response_date = as.Date (dplyr::first (created_at))
        ) |>
        dplyr::rename (number = issue_number)
    issue_responses <-
        dplyr::left_join (issues, cmt_responses, by = "number") |>
        dplyr::mutate (created_at = as.Date (created_at)) |>
        dplyr::mutate (
            response_time = difftime (response_date, created_at, units = "days")
        )

    start_date <- end_date - get_repometrics_period ()
    issue_responses <- dplyr::filter (
        issue_responses,
        created_at >= start_date & response_date <= end_date
    )

    return (issue_responses$response_time)
}

cm_metric_defect_resolution_dur <- function (path, end_date = Sys.Date ()) { # nolint

    issues <- rm_data_issues_from_gh_api (path)
    index <- grep ("bug|defect|fix", issues$label, ignore.case = TRUE)
    index <- index [which (!grepl ("wontfix", issues$label [index]))]
    bugs <- issues [index, ]
    bugs <- bugs [grep ("issues", bugs$url), ]

    ret <- c (mean = NA_real_, median = NA_real_)
    if (nrow (bugs) == 0L) {
        return (ret)
    }

    bugs$created_at <- as.Date (bugs$created_at)
    bugs$closed_at <- as.Date (bugs$closed_at)
    bugs$resolution_dur <-
        difftime (bugs$closed_at, bugs$created_at, units = "days")
    bugs$resolution_dur <- as.numeric (bugs$resolution_dur)
    start_date <- end_date - get_repometrics_period ()
    index <- which (bugs$created_at >= start_date & bugs$closed_at <= end_date)
    bugs <- bugs [index, ]

    return (c (
        mean = ifelse (nrow (bugs) == 0L, NA_real_, mean (bugs$resolution_dur)),
        median = stats::median (bugs$resolution_dur)
    ))
}

#' CHAOSS metric "Time to Close"
#'
#' \url{https://chaoss.community/kb/metric-time-to-close/}
#' "How much time passes between creating and closing an operation such as an
#' issue, change request, or support ticket?"
#' @noRd
cm_metric_time_to_close <- function (path, end_date = Sys.Date ()) {

    # suppress no visible binding notes:
    state <- NULL

    issues <- get_issues_in_period (path, end_date, closed_only = TRUE) |>
        dplyr::filter (state == "closed")

    times_to_close <- NA_integer_
    if (nrow (issues) > 0) {
        created_at <- as.Date (issues$created_at)
        closed_at <- as.Date (issues$closed_at)
        times_to_close <- difftime (closed_at, created_at, units = "days") |>
            as.integer ()
    }

    return (mn_med_sum (times_to_close))
}

#' CHAOSS metric "Change Request Closure Ratio"
#'
#' \url{https://chaoss.community/kb/metric-change-request-closure-ratio/}
#' The "ratio atio between the total number of open change requests during a
#' time period versus the total number of change requests closed in that same
#' period. A high change request closure ratio indicates that changes are
#' addressed promptly ..."
#'
#' Although the first sentence suggests a ratio of open/closed, the second
#' indicates that they actually mean closed/open. That is then what is done
#' here, so that low ratios are good. I submitted
#' https://github.com/chaoss/wg-metrics-development/pull/265
#' to suggest a fix. That should at least help clarify.
#'
#' @noRd
cm_metric_pr_closure_ratio <- function (path, end_date = Sys.Date ()) {

    # suppress no visible binding notes:
    closed <- NULL

    prs <- rm_data_prs_from_gh_api (path)
    prs$created_at <- as.Date (prs$created_at)
    prs$closed_at <- as.Date (prs$closed_at)

    closed_at <- as.Date (prs$closed_at)
    start_date <- end_date - get_repometrics_period ()

    prs_closed <- dplyr::filter (prs, closed)
    index_closed <- which (prs_closed$closed_at >= start_date &
        prs_closed$closed_at <= end_date)

    index_open <- which (!prs$closed | prs$closed_at >= start_date)

    length (index_closed) / length (index_open)
}

cm_metric_issue_age <- function (path, end_date = Sys.Date ()) {

    # suppress no visible binding notes:
    created_at <- closed_at <- NULL

    start_date <- end_date - get_repometrics_period ()

    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::mutate (
            created_at = as.Date (created_at), closed_at = as.Date (closed_at)
        ) |>
        dplyr::filter (created_at < end_date) |>
        dplyr::filter (is.na (closed_at) |
            (closed_at >= start_date & closed_at <= end_date))
    # using mutate mucks up Date class:
    issues$closed_at [which (is.na (issues$closed_at))] <- end_date

    dt <- difftime (issues$closed_at, issues$created_at, units = "days") |>
        as.integer ()

    c (
        mean = as.integer (mean (dt)),
        median = as.integer (stats::median (dt)),
        n = length (dt)
    )
}

#' CHAOSS metric for "Issues Active", which is simply the number of active
#' issues during specified period.
#'
#' \url{https://chaoss.community/kb/metric-issues-active/}
#' @noRd
cm_metric_issues_active <- function (path, end_date = Sys.Date ()) {

    # suppress no visible binding notes:
    created_at <- closed_at <- updated_at <- NULL

    start_date <- end_date - get_repometrics_period ()

    issues <- rm_data_issues_from_gh_api (path) |>
        dplyr::mutate (updated_at = as.Date (updated_at)) |>
        dplyr::filter (updated_at >= start_date & updated_at <= end_date)

    return (nrow (issues))
}

cm_metric_issue_comments <- function (path, end_date = Sys.Date ()) {

    # suppress no visible binding notes:
    created_at <- closed_at <- NULL

    start_date <- end_date - get_repometrics_period ()

    comments <- rm_data_issue_comments_from_gh_api (path) |>
        dplyr::mutate (created_at = as.Date (created_at)) |>
        dplyr::filter (created_at >= start_date & created_at <= end_date)

    return (nrow (comments))
}
