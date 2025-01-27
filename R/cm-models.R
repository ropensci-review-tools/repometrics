#' CHAOSS model "Developer Responsiveness"
#'
#' \url{https://chaoss.community/kb/metrics-model-development-responsiveness/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/4}.
#'
#' Lower value are better than higher values.
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

#' CHAOSS model "project engagement"
#'
#' \url{https://chaoss.community/kb/metrics-model-project-engagement/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/5}
#'
#' Higher values are better than lower values.
#'
#' The final item of "review cycle duration" is not included here, as all the
#' others can be used to form a simple numeric sum. These are:
#' 1. Nr. change requests accepted
#' 2. Nr. commiters, in 3 forms, each counting number of unique commiters who
#'    (starred or forked, created issues or comments or reviews, and created or
#'    merged PRs).
#' 3. Nr. code contributors
#' 4. Nr. issues closed
#' 5. Nr. issues updated
#' 6. Nr. comments in issues
#'
#' @noRd
cm_model_proj_engagement <- function (path, end_date = Sys.Date ()) {

    pr_dat <- cm_metric_change_req (path, end_date = end_date)
    num_prs_merged <- ifelse (
        length (pr_dat) > 1,
        pr_dat [["n_closed"]],
        0L
    )

    counts <- cm_metric_committer_count (path, end_date = end_date)
    # has number of unique commiters for (watchers or forks, issues, prs)

    num_code_ctbs <- cm_metric_ctb_count (path, end_date = end_date) [["code"]]

    num_issues_closed <- cm_metric_issues_closed (path, end_date = end_date)
    num_issues_updated <- cm_metric_issue_updates (path, end_date = end_date)
    num_issue_comments <- cm_metric_issue_cmt_count (path, end_date = end_date)

    # Not included here:
    # rev_cycle_dur <- cm_metric_pr_review_duration (path, end_date = end_date)

    res <- c (
        num_prs_merged, counts, num_code_ctbs,
        num_issues_closed, num_issues_updated, num_issue_comments
    )

    return (sum (res, na.rm = TRUE))
}

#' CHAOSS model for "project awareness"
#'
#' \url{https://chaoss.community/kb/metrics-model-project-awareness/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/6}
#'
#' Higher values are better than lower values.
#'
#' This model is formed here from directly summing numbers of forks and stars
#' (both within period only). The burstiness metric in the CHAOSS docs is
#' intended to be applied to burstiness in awareness-type activities like
#' starring, issue comments, and stuff like that, and not burstiness in actual
#' commit activity, which is what is measured here in `cm_metric_burstiness`.
#'
#' @noRd
cm_model_proj_awareness <- function (path, end_date = Sys.Date ()) {

    num_forks <- cm_metric_num_forks (path, end_date = end_date)
    num_forks <- num_forks [["num_in_period"]]

    num_stars <- cm_metric_popularity (path, end_date = end_date) [["stars"]]

    return (num_forks + num_stars)
}

#' CHAOSS model for "community activity"
#'
#' \url{https://chaoss.community/kb/metrics-model-community-activity/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/7}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_community_activity <- function (path, end_date = Sys.Date ()) {

    # ----- Single integer results which can be directly added:
    ctbs <- cm_metric_ctb_count (path, end_date = end_date)
    # Those are [code, pr_authors, issue_authors, issue_cmt_authors], each as
    # number of unique authors.
    prs <- cm_metric_pr_reviews (path, end_date = end_date)
    prs_approved <- prs [["approved_count"]]

    num_releases <- cm_metric_recent_releases (path, end_date = end_date)
    issues_updated <- cm_metric_issue_updates (path, end_date = end_date)
    num_maintainers <- cm_metric_maintainer_count (path, end_date = end_date)

    # ----- Model includes frequencies of both commits and comments, but these
    # ----- are here treated instead as direct counts.
    commit_count <- cm_metric_commit_freq (path, end_date = end_date) [["mean"]]
    comment_counts <- cm_metric_issue_cmt_count (path, end_date = end_date)

    res <- c (
        ctbs, prs_approved, num_releases, issues_updated,
        num_maintainers, commit_count, comment_counts
    )

    return (sum (res, na.rm = TRUE))

}

#' CHAOSS model for "OSS compliance and security"
#'
#' \url{https://chaoss.community/kb/metrics-model-oss-project-viability-compliance-security/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/8}
#'
#' Lower values are better than higher values.
#'
#' @noRd
cm_model_oss_compliance <- function (path, end_date = Sys.Date ()) {

    # All of these metrics are single numeric values which may be added, but
    # first need to be negated or inverted, so that 0 or lower values are
    # better:
    bp_badge <- as.integer (!cm_metric_best_practices (path))
    lic_coverage <- 1 - cm_metric_license_coverage (path)
    lic_declared <- as.integer (length (cm_metric_licenses_declared (path)) == 0L)

    # These are then directly measured so that lower values are better:
    defect_res_dur <-
        cm_metric_defect_resolution_dur (path, end_date = end_date) [["mean"]]
    libyears <- cm_metric_libyears (path) [["mean"]]

    # Artbitrarily divide number of deps by 20 to put on roughly equal scale to
    # the other metrics, with 20 dependencies giving a score of 1.:
    deps <- rm_data_dependencies (path)
    num_deps <- nrow (deps) / 20

    res <- c (
        bp_badge, lic_coverage, lic_declared, defect_res_dur,
        libyears, num_deps
    )

    return (sum (res, na.rm = TRUE))
}

#' CHAOSS model for "oss project viability: community"
#'
#' Higher values are better than lower values
#'
#' \url{https://chaoss.community/kb/metrics-model-oss-project-viability-community/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/9}
#' @noRd
cm_model_viability_community <- function (path, end_date = Sys.Date ()) {

    counts <- cm_metric_committer_count (path, end_date = end_date)
    # has number of unique commiters for (watchers or forks, issues, prs)
    counts <- counts [["watchers"]] # stars and forks, as unique users

    pr_dat <- cm_metric_change_req (path, end_date = end_date)
    # The model includes "Change Requests" (as direct count), and "Change
    # Request Closure Ratio". The latter here is replaced by number of "closed"
    # change requests, which is the number merged. Thus each opeend and merged
    # PR counts for 2, while each unmerged counts onlyu for 1.
    pr_dat <- pr_dat [c ("n_opened", "n_closed")]

    num_auts <- cm_metric_maintainer_count (path, end_date = end_date)
    num_auts <- num_auts [["recent"]]

    libyears <- cm_metric_libyears (path) [["mean"]]
    # lower libyears are better, and they can also be negative, so simply
    # negate here:
    libyears <- -libyears

    res <- c (counts, pr_dat, num_auts, libyears)

    return (sum (res, na.rm = TRUE))
}

#' CHAOSS model "oss project viability: starter"
#'
#' \url{https://chaoss.community/kb/metrics-model-project-viability-starter/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/10}
#'
#' Higher values are better than lower values
#'
#' @noRd
cm_model_viability_starter <- function (path, end_date = Sys.Date ()) {

    abs <- cm_metric_contrib_absence (path, end_date = end_date)
    # Only take absence factor for commits, as authors are be definition very
    # highly correlcted.
    abs <- abs [["ncommits"]]
    ele <- cm_metric_elephant_factor (path) [["ncommits"]]
    lic_declared <- as.integer (length (cm_metric_licenses_declared (path)) > 0L)
    pr_dat <- cm_metric_change_req (path, end_date = end_date)
    pr_dat <- pr_dat [c ("n_opened", "n_closed")]
    libyears <- -cm_metric_libyears (path) [["mean"]]

    res <- c (abs, ele, lic_declared, pr_dat, libyears)

    return (sum (res, na.rm = TRUE))
}

#' CHAOSS model for "oss project viability: governance"
#'
#' \url{https://chaoss.community/kb/metrics-model-oss-project-viability-governance/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/15}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_viability_gov <- function (path, end_date = Sys.Date ()) {

    # ---- Higher values are better:
    labs <- cm_metric_label_inclusivity (path, end_date = end_date)
    labs_prop_friendly <- labs [["prop_friendly_overall"]] # [0, 1]

    pr_dat <- cm_metric_change_req (path, end_date = end_date)
    pr_closure_ratio <- pr_dat [["prop_merged"]] # [0, 1]

    pop <- cm_metric_popularity (path, end_date = end_date)
    pop <- pop [c ("forks", "stars")] # [0, N >> 1]
    # Convert to values that shouldn't generally exceed 1:
    pop <- pop / c (100, 1000)

    # ----- Lower values are better:
    issues <- cm_metric_time_to_close (path, end_date = end_date)
    iss_time_to_close <- issues [["mean"]] # [0, N >> 1]

    libyears <- cm_metric_libyears (path) [["mean"]]

    issue_age <- cm_metric_issue_age (path, end_date = end_date)
    issue_age <- issue_age [["mean"]] # [0, N >> 1]

    rel_freq <- cm_metric_release_freq (path, end_date = end_date)
    rel_freq <- rel_freq [["mean"]] # [0, N >> 1]

    # These latter 3 are then inverted and added, so that higher values closer
    # to one are better:
    res <- c (
        labs_prop_friendly, pr_closure_ratio, pop,
        1 / c (iss_time_to_close, issue_age, rel_freq)
    )
    return (sum (res, na.rm = TRUE))
}

#' CHAOSS model for "oss project viability: strategy"
#'
#' \url{https://chaoss.community/kb/metrics-model-oss-project-viability-strategy/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/16}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_viability_strategy <- function (path, end_date = Sys.Date ()) {

    langs <- cm_metric_languages (path)
    lang_dist_mn <- mean (langs$ncode_pc) # lower is better
    # Re-scale this so that 4 languages translates to a value of 1:
    lang_dist_mn <- 0.25 / lang_dist_mn

    bus <- cm_metric_contrib_absence (path, end_date = end_date)
    bus <- bus [["ncommits"]] # higher is better

    ele <- cm_metric_elephant_factor (path, end_date = end_date)
    ele <- ele [["ncommits"]] # higher is better

    rel_freq <- cm_metric_release_freq (path, end_date = end_date)
    rel_freq <- 1 / rel_freq [["mean"]] # higher is better

    res <- c (lang_dist_mn, bus, ele, rel_freq)

    return (sum (res, na.rm = TRUE))
}

#' CHAOSS model for "collaboration development index"
#'
#' \url{https://chaoss.community/kb/metrics-model-collaboration-development-index/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/11}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_collab_devel_index <- function (path, end_date = Sys.Date ()) {

    # metrics that are in [0, 1]:
    ci_test_data <- gh_workflow_test_coverage (path)
    has_ci_tests <- nrow (ci_test_data)

    pr_dat <- cm_metric_change_req (path, end_date = end_date)
    pr_prop_code <- pr_dat [["prop_code_from_prs"]]
    issues_to_prs <- cm_metric_issues_to_prs (path, end_date = end_date)

    # metrics that are in [0, N ~ O(1)]:
    num_ctbs <- cm_metric_num_contributors (path, end_date = end_date)
    pr_dat <- cm_metric_pr_reviews (path, end_date = end_date)
    num_pr_reviews <- pr_dat [["approved_count"]]
    num_forks <- cm_metric_num_forks (path, end_date = end_date)
    num_forks <- num_forks [["num_in_period"]]

    # matrics that are in [0, N >> 1]:
    num_commits <- cm_metric_num_commits (path, end_date = end_date) # [0, N >> 1]
    code_change_lines <- cm_metric_code_change_lines (path, end_date = end_date)

    res_O1 <- c (has_ci_tests, pr_prop_code, issues_to_prs)
    res_ON <- c (num_ctbs, num_pr_reviews, num_forks)
    res_ON2 <- log10 (c (num_commits, code_change_lines))

    res <- c (res_O1, res_ON, res_ON2)

    return (sum (res, na.rm = TRUE))
}
