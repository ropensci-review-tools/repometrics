#' CHAOSS model "Developer Responsiveness"
#'
#' \url{https://chaoss.community/kb/metrics-model-development-responsiveness/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/4}.
#'
#' Higher values are better than lower values.
#'
#' This takes the four metrics of:
#' 1. Review cycle duration within a change request (in days)
#' 2. Change request duration (in days)
#' 3. Issue response time (in days)
#' 4. Defect resolution duration (in days)
#' measured in both mean and median forms, and converts all measured values to
#' aggregate mean and median values.
#'
#' @return A single numeric value formed from the mean of all of the four
#' values. As this is in days, it is converted to log 10, with both NA values
#' and values < 1 converted to values of 1 prior to log. The final value is
#' then two minus this value, so higher values are better, and all are assessed
#' against a standard scale of 100 days (log10(100) = 2).
#'
#' @noRd
cm_model_dev_responsiveness <- function (path,
                                         end_date = Sys.Date (),
                                         metrics_data = NULL) {

    if (is.null (metrics_data)) {
        pr_dur_mn <- cm_metric_pr_review_duration (path, end_date = end_date)
        issue_resp_time <-
            cm_metric_issue_response_time (path, end_date = end_date)
        defect_resol_dur <-
            cm_metric_defect_resolution_dur (path, end_date = end_date)
    } else {
        pr_dur_mn <- metrics_data$pr_review_duration
        issue_resp_time <- metrics_data$issue_response_time
        defect_resol_dur <- metrics_data$defect_resolution_dur
    }

    vals <- c (pr_dur_mn, issue_resp_time, defect_resol_dur)
    # convert final value to scale so that higher is better by
    val <- 2 - log10 (mean (vals, na.rm = TRUE))
    val <- ifelse (is.na (val), 0, val)
    return (val)
}

#' CHAOSS model "project engagement"
#'
#' \url{https://chaoss.community/kb/metrics-model-project-engagement/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/5}
#'
#' Values are counts in [0, N], with final value of log10(sum(...)), so that
#' higher values are better than lower values.
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
cm_model_proj_engagement <- function (path,
                                      end_date = Sys.Date (),
                                      metrics_data = NULL) {

    if (is.null (metrics_data)) {

        num_prs_merged <- cm_metric_change_req_n_closed (path, end_date = end_date)
        counts <- cm_metric_committer_count (path, end_date = end_date)
        # has number of unique commiters for (watchers or forks, issues, prs)

        num_code_ctbs <-
            cm_metric_ctb_count (path, end_date = end_date) [["code"]]

        num_issues_closed <-
            cm_metric_issues_closed (path, end_date = end_date)
        num_issues_updated <-
            cm_metric_issue_updates (path, end_date = end_date)
        num_issue_comments <-
            cm_metric_issue_cmt_count (path, end_date = end_date)

        # Not included here:
        # rev_cycle_dur <-
        #   cm_metric_pr_review_duration (path, end_date = end_date)
    } else {

        num_prs_merged <- metrics_data$change_req_n_closed
        counts <- metrics_data$committer_count
        num_code_ctbs <- metrics_data$ctb_count
        num_issues_closed <- metrics_data$issues_closed
        num_issues_updated <- metrics_data$issue_updates
        num_issue_comments <- metrics_data$issue_cmt_count
    }

    res <- c (
        num_prs_merged, counts, num_code_ctbs,
        num_issues_closed, num_issues_updated, num_issue_comments
    )
    res [which (res == 0)] <- 1
    res <- sum (log10 (res), na.rm = TRUE)

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
cm_model_proj_awareness <- function (path,
                                     end_date = Sys.Date (),
                                     metrics_data = NULL) {

    if (is.null (metrics_data)) {

        num_forks <- cm_metric_num_forks (path, end_date = end_date)
        num_stars <- cm_metric_num_stars (path, end_date = end_date)

    } else {

        num_forks <- metrics_data$num_forks
        num_stars <- metrics_data$num_stars

    }

    res <- c (num_forks, num_stars)
    res [which (res == 0)] <- 1
    res <- sum (log10 (res), na.rm = TRUE)

    return (res)
}

#' CHAOSS model for "community activity"
#'
#' \url{https://chaoss.community/kb/metrics-model-community-activity/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/7}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_community_activity <- function (path,
                                         end_date = Sys.Date (),
                                         metrics_data = NULL) {

    if (is.null (metrics_data)) {


        # ----- Single integer results which can be directly added:
        ctbs <- cm_metric_ctb_count (path, end_date = end_date)
        # Those are [code, pr_authors, issue_authors, issue_cmt_authors], each
        # as number of unique authors.
        prs_approved <- cm_metric_pr_reviews_approved (path, end_date = end_date)

        num_releases <- cm_metric_recent_releases (path, end_date = end_date)
        issues_updated <- cm_metric_issue_updates (path, end_date = end_date)
        num_maintainers <-
            cm_metric_maintainer_count (path, end_date = end_date)

        # ----- Model includes frequencies of both commits and comments, but
        # ----- these are here treated instead as direct counts.
        commit_count <- cm_metric_commit_count (path, end_date = end_date)
        comment_counts <- cm_metric_issue_cmt_count (path, end_date = end_date)

    } else {

        ctbs <- metrics_data$ctb_count
        prs_approved <- metrics_data$pr_reviews_approved
        num_releases <- metrics_data$recent_releases
        issues_updated <- metrics_data$issue_updates
        num_maintainers <-
            metrics_data$maintainer_count
        commit_count <- metrics_data$commit_count
        comment_counts <- metrics_data$issue_cmt_count

    }

    res <- c (
        ctbs, prs_approved, num_releases, issues_updated,
        num_maintainers, commit_count, comment_counts
    )
    res [which (res == 0)] <- 1
    res <- sum (log10 (res), na.rm = TRUE)

    return (res)

}

#' CHAOSS model for "OSS compliance and security"
#'
#' \url{https://chaoss.community/kb/metrics-model-oss-project-viability-compliance-security/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/8}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_oss_compliance <- function (path,
                                     end_date = Sys.Date (),
                                     metrics_data = NULL) {

    if (is.null (metrics_data)) {

        # All of these metrics are single numeric values which may be added, and
        # for which higher values are better:
        bp_badge <- cm_metric_best_practices (path)
        lic_coverage <- cm_metric_license_coverage (path)
        lic_declared <- cm_metric_licenses_declared (path)

        # These are then directly measured so that lower values are better:
        defect_res_dur <-
            cm_metric_defect_resolution_dur (path, end_date = end_date)
        libyears <- cm_metric_libyears (path)
        depcount <- cm_metric_dependency_count (path)

    } else {

        bp_badge <- metrics_data$best_practices
        lic_coverage <- metrics_data$license_coverage
        lic_declared <- metrics_data$licenses_declared
        defect_res_dur <- metrics_data$defect_resolution_dur
        libyears <- metrics_data$libyears
        depcount <- metrics_data$dependency_count

    }

    bp_badge <- as.integer (bp_badge)
    lic_declared <- as.integer (lic_declared)
    defect_res_dur <- ifelse (defect_res_dur > 0, log10 (defect_res_dur), 0)

    num_deps <- ifelse (depcount == 0L, 0, log10 (depcount))

    res_0N <- sum (c (bp_badge, lic_coverage, lic_declared), na.rm = TRUE)
    res_log <- sum (c (defect_res_dur, libyears, num_deps), na.rm = TRUE)

    res <- res_0N - res_log

    return (res)
}

#' CHAOSS model for "oss project viability: community"
#'
#' Higher values are better than lower values
#'
#' \url{https://chaoss.community/kb/metrics-model-oss-project-viability-community/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/9}
#' @noRd
cm_model_viability_community <- function (path,
                                          end_date = Sys.Date (),
                                          metrics_data = NULL) {

    if (is.null (metrics_data)) {

        watcher_count <- cm_metric_watcher_count (path, end_date = end_date)
        pr_n_opened <- cm_metric_change_req_n_opened (path, end_date = end_date)
        pr_n_closed <- cm_metric_change_req_n_closed (path, end_date = end_date)
        num_auts <- cm_metric_maintainer_count (path, end_date = end_date)
        libyears <- cm_metric_libyears (path)

    } else {

        watcher_count <- metrics_data$watcher_count
        pr_dat <- metrics_data$change_req
        pr_n_opened <- metrics_data$change_req_n_opened
        pr_n_closed <- metrics_data$change_req_n_closed
        num_auts <- metrics_data$maintainer_count
        libyears <- metrics_data$libyears

    }

    # The model includes "Change Requests" (as direct count), and "Change
    # Request Closure Ratio". The latter here is replaced by number of "closed"
    # change requests, which is the number merged. Thus each opened and merged
    # PR counts for 2, while each unmerged counts onlyu for 1.
    res <- c (watcher_count, pr_n_opened, pr_n_closed, num_auts)
    res [which (res == 0)] <- 1
    # lower values of libyears are  better, so appended below in negated form:
    res <- sum (c (log10 (res), -libyears), na.rm = TRUE)

    return (res)
}

#' CHAOSS model "oss project viability: starter"
#'
#' \url{https://chaoss.community/kb/metrics-model-project-viability-starter/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/10}
#'
#' Higher values are better than lower values
#'
#' @noRd
cm_model_viability_starter <- function (path,
                                        end_date = Sys.Date (),
                                        metrics_data = NULL) {

    if (is.null (metrics_data)) {

        # Only take absence factors for commits, as authors are be definition
        # very highly correlated.
        abs <- cm_metric_contrib_absence_commits (path, end_date = end_date)
        ele <- cm_metric_elephant_factor (path)
        lic_declared <- cm_metric_licenses_declared (path)
        pr_n_opened <- cm_metric_change_req_n_opened (path, end_date = end_date)
        pr_n_closed <- cm_metric_change_req_n_closed (path, end_date = end_date)
        libyears <- cm_metric_libyears (path)

    } else {

        abs <- metrics_data$contrib_absence_commits
        ele <- metrics_data$elephant_factor
        lic_declared <- metrics_data$licenses_declared
        pr_n_opened <- metrics_data$change_req_n_opened
        pr_n_closed <- metrics_data$change_req_n_closed
        libyears <- metrics_data$libyears

    }

    lic_declared <- as.integer (lic_declared)

    res <- c (abs, ele, lic_declared, pr_n_opened, pr_n_closed)
    res [which (res == 0)] <- 1
    res <- sum (c (log10 (res), -libyears), na.rm = TRUE)

    return (res)
}

#' CHAOSS model for "oss project viability: governance"
#'
#' \url{https://chaoss.community/kb/metrics-model-oss-project-viability-governance/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/15}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_viability_gov <- function (path,
                                    end_date = Sys.Date (),
                                    metrics_data = NULL) {


    if (is.null (metrics_data)) {

        # ---- Higher values are better:
        labs <- cm_metric_label_inclusivity (path, end_date = end_date)
        pr_closure_ratio <- cm_metric_change_req_prop_merged (path, end_date = end_date)
        num_forks <- cm_metric_num_forks (path, end_date = end_date)
        num_stars <- cm_metric_num_stars (path, end_date = end_date)

        # ----- Lower values are better:
        issue_time_to_close <- cm_metric_time_to_close (path, end_date = end_date)
        libyears <- cm_metric_libyears (path)
        issue_age <- cm_metric_issue_age (path, end_date = end_date)
        rel_freq <- cm_metric_release_freq (path, end_date = end_date)

    } else {

        labs <- metrics_data$label_inclusivity
        pr_closure_ratio <- metrics_data$change_req_prop_merged
        num_forks <- metrics_data$num_forks
        num_stars <- metrics_data$num_stars
        issue_time_to_close <- metrics_data$time_to_close
        libyears <- metrics_data$libyears
        issue_age <- metrics_data$issue_age
        rel_freq <- metrics_data$release_freq

    }

    rel_freq <- rel_freq [["mean"]] # [0, N >> 1]

    labs_prop_friendly <- labs [["prop_friendly_overall"]] # [0, 1]

    # ------ Combine all:
    res_01 <- c (labs_prop_friendly, pr_closure_ratio) # higher is better
    num_forks <- ifelse (num_forks == 0, 0, log10 (num_forks))
    num_stars <- ifelse (num_stars == 0, 0, log10 (num_stars))
    res_days <- c (issue_time_to_close, issue_age, rel_freq)
    res_days [which (res_days == 0)] <- 1
    res_days <- log10 (res_days) # lower is better

    res <- c (res_01, num_forks, num_stars, -res_days, -libyears)
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
cm_model_viability_strategy <- function (path,
                                         end_date = Sys.Date (),
                                         metrics_data = NULL) {

    if (is.null (metrics_data)) {

        langs <- cm_metric_languages (path)
        bus <- cm_metric_contrib_absence_commits (path, end_date = end_date)
        ele <- cm_metric_elephant_factor (path, end_date = end_date)
        rel_freq <- cm_metric_release_freq (path, end_date = end_date)

    } else {

        langs <- metrics_data$languages
        bus <- metrics_data$contrib_absence_commits
        ele <- metrics_data$elephant_factor
        rel_freq <- metrics_data$release_freq

    }

    lang_dist_mn <- mean (langs$ncode_pc) # lower is better
    # Re-scale this so that 4 languages translates to a value of 1:
    lang_dist_mn <- ifelse (lang_dist_mn == 0, 0, 0.25 / lang_dist_mn)

    bus <- log10 (bus) # higher is better
    ele <- log10 (ele) # higher is better

    req_freq <- rel_freq [["mean"]]
    rel_freq <- log10 (ifelse (rel_freq == 0, 1, rel_freq))

    res_0N <- c (bus, ele, -rel_freq)
    res <- lang_dist_mn + sum (res_0N, na.rm = TRUE)

    return (res)
}

#' CHAOSS model for "collaboration development index"
#'
#' \url{https://chaoss.community/kb/metrics-model-collaboration-development-index/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/11}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_collab_devel_index <- function (path,
                                         end_date = Sys.Date (),
                                         metrics_data = NULL) {

    if (is.null (metrics_data)) {

        # metrics that are in [0, 1]:
        has_ci_tests <- cm_metric_has_ci (path, end_date = end_date)

        prop_code_from_prs <- cm_metric_change_req_prop_code (path, end_date = end_date)
        issues_to_prs <- cm_metric_issues_to_prs (path, end_date = end_date)

        # metrics that are in [0, N ~ O(1)]:
        num_ctbs <- cm_metric_num_contributors (path, end_date = end_date)
        pr_dat <- cm_metric_pr_reviews (path, end_date = end_date)
        num_forks <- cm_metric_num_forks (path, end_date = end_date)

        # matrics that are in [0, N >> 1]:
        num_commits <- cm_metric_num_commits (path, end_date = end_date) # [0, N >> 1]
        code_change_lines <- cm_metric_code_change_lines (path, end_date = end_date)

    } else {

        has_ci_tests <- metrics_data$has_ci
        prop_code_from_prs <- metrics_data$change_req_prop_code
        issues_to_prs <- metrics_data$issues_to_prs
        num_ctbs <- metrics_data$num_contributors
        pr_dat <- metrics_data$pr_reviews
        num_forks <- metrics_data$num_forks
        num_commits <- metrics_data$num_commits
        code_change_lines <- metrics_data$code_change_lines

    }

    num_pr_reviews <- pr_dat [["approved_count"]]

    res_O1 <- c (has_ci_tests, prop_code_from_prs, issues_to_prs)
    res_ON <- c (num_ctbs, num_pr_reviews, num_forks)
    res_ON2 <- c (num_commits, code_change_lines)
    res_ON2 [which (res_ON2 == 0)] <- 1
    res_ON2 <- log10 (res_ON2)

    res <- c (res_O1, res_ON, res_ON2)

    return (sum (res, na.rm = TRUE))
}

#' CHAOSS model for community service and support
#'
#' \url{https://chaoss.community/kb/metrics-model-community-service-and-support/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/12}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_comm_serv_support <- function (path,
                                        end_date = Sys.Date (),
                                        metrics_data = NULL) {

    if (is.null (metrics_data)) {

        # Metrics measured in days, for which lower is better:
        issue_resp_time <- cm_metric_issue_response_time (path, end_date = end_date)
        issue_age <- cm_metric_issue_age (path, end_date = end_date)
        issue_res_duration <-
            cm_metric_defect_resolution_dur (path, end_date = end_date)

        pr_age <- cm_metric_pr_age (path, end_date = end_date)
        pr_dur_mn <- cm_metric_pr_review_duration (path, end_date = end_date)

        # Metrics measured in N > 1, for which higher is better:
        issue_num_cmts <- cm_metric_issue_comments (path, end_date = end_date)
        issues_active <- cm_metric_issues_active (path, end_date = end_date)
        pr_num_revs <- cm_metric_pr_reviews (path, end_date = end_date)

    } else {

        issue_resp_time <- metrics_data$issue_response_time
        issue_age <- metrics_data$issue_age
        issue_res_duration <- metrics_data$defect_resolution_dur
        pr_age <- metrics_data$pr_age
        pr_dur_mn <- metrics_data$pr_review_duration
        issue_num_cmts <- metrics_data$issue_comments
        issues_active <- metrics_data$issues_active
        pr_num_revs <- metrics_data$pr_reviews

    }

    pr_age <- pr_age [["mean"]]

    pr_num_revs_approved <- pr_num_revs [["approved_count"]]
    pr_num_revs_rejected <- pr_num_revs [["rejected_count"]]

    res_N_days <- c (issue_resp_time, issue_age, issue_res_duration, pr_age, pr_dur_mn)
    res_ON <-
        c (issue_num_cmts, issues_active, pr_num_revs_approved, pr_num_revs_rejected)
    res_log10 <- vapply (list (res_N_days, res_ON), function (i) {
        i [which (i == 0)] <- 1
        return (sum (log10 (i), na.rm = TRUE))
    }, numeric (1L))
    # res_log10[2] is then better for higher values, while res_log10[1] is
    # better for lower values:
    res <- 10^(res_log10 [2] - res_log10 [1])

    return (res)
}

#' CHAOSS model for "starter project health"
#'
#' \url{https://chaoss.community/kb/metrics-model-starter-project-health/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/13}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_starter_health <- function (path,
                                     end_date = Sys.Date (),
                                     metrics_data = NULL) {

    if (is.null (metrics_data)) {

        # Metrics measured in days, for which lower is better:
        time_first_resp <- cm_metric_issue_response_time (path, end_date = end_date)

        # Metrics in [0, 1], for which higher is better:
        pr_closure_ratio <- cm_metric_change_req_prop_merged (path, end_date = end_date)

        # Metrics in [0, N], for which higher is better:
        abs <- cm_metric_contrib_absence_commits (path, end_date = end_date)
        rel_freq <- cm_metric_release_freq (path, end_date = end_date)

    } else {

        time_first_resp <- metrics_data$issue_response_time
        pr_closure_ratio <- metrics_data$change_req_prop_merged
        abs <- metrics_data$contrib_absence_commits
        rel_freq <- metrics_data$release_freq

    }

    time_first_resp <- ifelse (time_first_resp == 0, 1, time_first_resp)

    rel_freq <- rel_freq [["mean"]] # [0, N >> 1]
    rel_freq <- ifelse (rel_freq == 0, 1, rel_freq)

    res_high <- c (pr_closure_ratio, log10 (abs), log10 (rel_freq))

    res <- sum (res_high, na.rm = TRUE)
    if (!is.na (time_first_resp)) {
        res <- res - log10 (time_first_resp)
    }

    return (res)
}

#' CHAOSS model for "community welcomingness"
#'
#' \url{https://chaoss.community/kb/metrics-model-community-welcomingness/}
#' \url{https://github.com/ropensci-review-tools/repometrics/issues/14}
#'
#' Higher values are better than lower values.
#'
#' @noRd
cm_model_comm_welcoming <- function (path,
                                     end_date = Sys.Date (),
                                     metrics_data = NULL) {

    if (is.null (metrics_data)) {

        # ----- Values in days for which lower are better:
        issue_age <- cm_metric_issue_age (path, end_date = end_date)

        time_first_resp <- cm_metric_issue_response_time (path, end_date = end_date)

        # ----- Values in [0, 1] for which higher are better:
        lic_coverage <- cm_metric_license_coverage (path)
        lic_declared <- cm_metric_licenses_declared (path)
        ci_test_data <- cm_metric_test_coverage (path)
        bp_badge <- cm_metric_best_practices (path)
        pr_closure_ratio <- cm_metric_change_req_prop_merged (path, end_date = end_date)

        bus <- cm_metric_contrib_absence_commits (path, end_date = end_date)
        ele <- cm_metric_elephant_factor (path)
        num_code_ctbs <- cm_metric_ctb_count (path, end_date = end_date)

    } else {

        issue_age <- metrics_data$issue_age
        time_first_resp <- metrics_data$issue_response_time
        lic_coverage <- metrics_data$license_coverage
        lic_declared <- metrics_data$licenses_declared
        ci_test_data <- metrics_data$test_coverage
        bp_badge <- metrics_data$best_practices
        pr_closure_ratio <- metrics_data$change_req_prop_merged
        bus <- metrics_data$contrib_absence_commits
        ele <- metrics_data$elephant_factor
        num_code_ctbs <- metrics_data$ctb_count

    }

    issue_age <- ifelse (issue_age == 0, 1, issue_age)
    time_first_resp <- ifelse (time_first_resp == 0, 1, time_first_resp)

    val_days <- log10 (c (issue_age, time_first_resp))

    lic_declared <- as.integer (lic_declared)
    bp_badge <- as.integer (bp_badge)

    test_cov <- ifelse (nrow (ci_test_data) > 0, ci_test_data$coverage, 0.0)
    test_cov <- test_cov / 100

    val_01 <-
        c (lic_coverage, lic_declared, bp_badge, test_cov, pr_closure_ratio)

    # ----- Values in [0, N] for which higher are better:
    num_code_ctbs <- num_code_ctbs [["code"]]
    num_code_ctbs <- ifelse (num_code_ctbs == 0, 1, num_code_ctbs)

    val_0N <- log10 (c (bus, ele, num_code_ctbs))

    res <- sum (val_01, na.rm = TRUE) +
        sum (val_0N, na.rm = TRUE) -
        sum (val_days, na.rm = TRUE)

    return (res)
}
