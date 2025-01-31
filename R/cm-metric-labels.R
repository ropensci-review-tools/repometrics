#' Iusse label inclusivity
#'
#' \url{https://chaoss.community/kb/metric-issue-label-inclusivity/}
#' Based on identifying "the published list of issue labels"
#' \itemize{
#' \item General labels identify general needs of the project (e.g.Feature,
#' Bug, and Documentation)
#' \item Newcomer friendly labels identify issues that are appropriate for
#' first-time contributors (e.g., newcomer, good first issue)
#' \item Skill labels identify skills needed (e.g, Java, Python, HTML, machine
#' learning)
#' }
#'
#' The labels here are taken for issues opened up to the given `end_date`,
#' regardless of when the labels were actually attached. The metrics only
#' consider the "Newcomer friendly" labels, as the "skill labels" are too
#' subjective to automate in a general way here.
#'
#' Note that this also does not consider a "start_date", because the labels may
#' have been added at any time after an actual creation date of each issue.
#'
#' @noRd
cm_metric_label_inclusivity <- function (path, end_date = Sys.Date ()) {

    # suppress no visible binding notes:
    created_at <- NULL

    issues_data <- rm_data_issues_from_gh_api (path)
    issues_data$created_at <- as.Date (issues_data$created_at)

    issues_data <- dplyr::filter (issues_data, created_at <= end_date)

    labels <- issues_data$labels
    index <- which (nzchar (labels))
    prop_labelled <- length (index) / length (labels)
    labels <- labels [index]

    prop_labelled_friendly <- prop_friendly_overall <- 0

    if (length (labels) > 0) {
        friendly_ptn <- "newcomer|first\\s*issue|welcome|help"
        labels_newcomer_friendly <-
            grep (friendly_ptn, labels, value = TRUE, ignore.case = TRUE)
        prop_labelled_friendly <-
            length (labels_newcomer_friendly) / length (labels)
        prop_friendly_overall <-
            length (labels_newcomer_friendly) / nrow (issues_data)
    }

    c (
        prop_labelled = prop_labelled,
        prop_labelled_friendly = prop_labelled_friendly,
        prop_friendly_overall = prop_friendly_overall
    )
}
