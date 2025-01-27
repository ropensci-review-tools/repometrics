#' CHAOSS model "Developer Responsiveness"
#'
#' \url{https://chaoss.community/kb/metrics-model-development-responsiveness/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/4}.
#'
#' This takes the four metrics of:
#' 1. Review cycle duration within a change request (in days)
#' 2. Change request duration (in days)
#' 3. Issue response time (in days)
#' 4. Defect resolution duration (in days)
#' measured in both mean and median forms, and converts all measured values to
#' aggregate mean and median values.
#'
#' @return A numeric vector of two values for mean and median of overall
#' response durations.
#'
#' @noRd
cm_model_dev_reponsiveness <- function (path, end_date = Sys.Date ()) {

    pr_durs <- cm_metric_pr_review_duration (path, end_date = end_date)
    issue_resp_time <- mn_med_sum (
        cm_metric_issue_response_time (path, end_date = end_date)
    )
    names (issue_resp_time) <- paste0 ("issue_resp_", names (issue_resp_time))
    defect_resol_dur <-
        cm_metric_defect_resolution_dur (path, end_date = end_date)
    names (defect_resol_dur) <-
        paste0 ("defect_resol_", names (defect_resol_dur))

    nms <- list (c ("mn", "mean"), c ("md", "median"))
    vals <- lapply (nms, function (nm) {
        index_i <- grep (nm [2], names (issue_resp_time), value = TRUE)
        index_d <- grep (nm [2], names (defect_resol_dur), value = TRUE)
        vals_i <- c (
            pr_durs [grep (nm [1], names (pr_durs), value = TRUE)],
            issue_resp_time [index_i],
            defect_resol_dur [index_d]
        )
        return (vals_i [which (!is.na (vals_i))])
    })
    vals <- c (mean = mean (vals [[1]]), median = stats::median (vals [[2]]))
    names (vals) <- c ("mean", "median")
    vals [which (is.na (vals))] <- NA_real_

    return (vals)
}
