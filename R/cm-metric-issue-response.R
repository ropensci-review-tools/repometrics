cm_metric_issue_response_time <- function (path, end_date = Sys.Date ()) {

    # suppress no visible warning notes:
    user_login <- issue_number <- created_at <- response_date <- NULL

    ctbs_main_recent <- main_contributors (path, end_date = end_date, period = 365)
    ctbs_main_all <- main_contributors (path, end_date = end_date, period = NULL)
    ctbs_main <- unique (c (ctbs_main_recent, ctbs_main_all))

    issues <- cm_data_issues_from_gh_api (path)
    issues <- issues [grep ("issues", issues$url), ]
    comments <- cm_data_issue_comments_from_gh_api (path)

    cmt_responses <- dplyr::filter (comments, user_login %in% ctbs_main) |>
        dplyr::group_by (issue_number) |>
        dplyr::summarise (
            response_date = as.Date (dplyr::first (created_at))
        ) |>
        dplyr::rename (number = issue_number)
    issue_responses <- dplyr::left_join (issues, cmt_responses, by = "number") |>
        dplyr::mutate (created_at = as.Date (created_at)) |>
        dplyr::mutate (
            response_time = difftime (response_date, created_at, units = "days")
        )

    start_date <- end_date - get_repometrics_period ()
    index <- which (issue_responses$created_at >= start_date &
        issue_responses$response_date <= end_date)

    ret <- c (mean = NA_real_, median = NA_real_)
    if (length (index) > 0L) {
        ret <- c (
            mean = mean (as.numeric (issue_responses$response_time [index])),
            median = stats::median (as.numeric (issue_responses$response_time [index]))
        )
    }

    return (ret)
}

cm_metric_defect_resolution_dur <- function (path, end_date = Sys.Date ()) {

    issues <- cm_data_issues_from_gh_api (path)
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
    bugs$resolution_dur <- difftime (bugs$closed_at, bugs$created_at, units = "days")
    bugs$resolution_dur <- as.numeric (bugs$resolution_dur)
    start_date <- end_date - get_repometrics_period ()
    index <- which (bugs$created_at >= start_date & bugs$closed_at <= end_date)
    bugs <- bugs [index, ]

    return (c (
        mean = ifelse (nrow (bugs) == 0L, NA_real_, mean (bugs$resolution_dur)),
        median = stats::median (bugs$resolution_dur)
    ))
}
