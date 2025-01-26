#' Change request reviews
#' \url{https://chaoss.community/kb/metric-change-request-reviews/}.
#'
#' The "Overview" at that link says:
#' The Change Request Reviews metric evaluates the level and quality of formal
#' review processes for change requests (e.g., pull requests) within
#' open-source projects. This metric tracks specific data points such as the
#' number of reviews, types of review feedback, and review outcomes (e.g.,
#' accepted, declined) to determine the rigor and quality of reviews. Measuring
#' this helps project maintainers gauge the thoroughness of code evaluation,
#' process efficiency, and software quality. Change request reviews include top
#' level comments about the entire change request, file level comments asking
#' for specific changes, and whether the change request was “accepted”, “had
#' changes requested”, or the reasoning behind a change request being closed
#' without getting merged. Additionally, this metric can reveal insights into
#' DEI-related aspects, such as the diversity of contributors participating in
#' review processes.
#'
#' This function generates a few outputs which can be used to provide insight
#' into that.
#'
#' @noRd
cm_metric_pr_reviews <- function (path, end_date = Sys.Date ()) {

    prs <- get_prs_in_period (path, end_date) # in data-gh-prs.R

    prs$created_at <- as.Date (prs$created_at)
    prs$closed_at <- as.Date (prs$closed_at)
    prs$review_decision [which (is.na (prs$review_decision))] <- "NA"

    index_approved <- which (prs$review_decision == "APPROVED")
    index_rejected <-
        which (prs$review_decision != "APPROVED" & !prs$merged & prs$closed)
    index_other <- which (prs$review_decision != "APPROVED" & prs$merged)
    index_open <- which (!prs$merged)

    approved_ratio <- rejected_ratio <- 0
    if (nrow (prs) > 0) {
        approved_ratio <- length (index_approved) / nrow (prs)
        rejected_ratio <- length (index_rejected) / nrow (prs)
    }

    mean_to_na <- function (x) {
        ifelse (length (x) == 0L, NA_real_, mean (x))
    }

    pr_duration <- difftime (prs$closed_at, prs$created_at, units = "days")
    pr_duration <- as.integer (pr_duration)
    approval_duration <- mean_to_na (pr_duration [index_approved])

    n_comments <- prs$total_comments
    n_comments_per_approved <- mean_to_na (n_comments [index_approved])
    n_comments_per_rejected <- mean_to_na (n_comments [index_rejected])
    n_comments_per_other <- mean_to_na (n_comments [index_other])

    num_commenters <- vapply (seq_len (nrow (prs)), function (i) {
        cmt_authors <- unique (prs$comments [[i]]$author)
        cmt_authors <- cmt_authors [which (!cmt_authors == prs$user_login [i])]
        length (cmt_authors)
    }, integer (1L))
    num_comment_iterations <- vapply (seq_len (nrow (prs)), function (i) {
        auts <- prs$comments [[i]]$author
        creator <- prs$user_login [i]
        index <- rep (0L, length (auts))
        index [which (auts == creator)] <- 1L
        ifelse (length (index) == 0L, 0L, max (cumsum (index)))
    }, integer (1L))
    n_commenters_per_approved <- mean_to_na (num_commenters [index_approved])
    n_commenters_per_rejected <- mean_to_na (num_commenters [index_rejected])
    n_commenters_per_other <- mean_to_na (num_commenters [index_other])
    n_iterations_per_approved <-
        mean_to_na (num_comment_iterations [index_approved])
    n_iterations_per_rejected <-
        mean_to_na (num_comment_iterations [index_rejected])
    n_iterations_per_other <- mean_to_na (num_comment_iterations [index_other])

    ret <- data.frame (
        approved_count = length (index_approved),
        rejected_count = length (index_rejected),
        approved_ratio = approved_ratio,
        rejected_ratio = rejected_ratio,
        approval_duration = approval_duration,
        n_comments_per_approved = n_comments_per_approved,
        n_comments_per_rejected = n_comments_per_rejected,
        n_comments_per_other = n_comments_per_other,
        n_commenters_per_approved = n_commenters_per_approved,
        n_commenters_per_rejected = n_commenters_per_rejected,
        n_commenters_per_other = n_commenters_per_other,
        n_iterations_per_approved = n_iterations_per_approved,
        n_iterations_per_rejected = n_iterations_per_rejected,
        n_iterations_per_other = n_iterations_per_other
    )

    return (ret)
}

cm_metric_pr_review_duration <- function (path, end_date = Sys.Date ()) {

    prs <- get_prs_in_period (path, end_date) # in cm-metrics-change-req.R

    prs$created_at <- as.Date (prs$created_at)
    prs$closed_at <- as.Date (prs$closed_at)

    index_approved <- which (prs$review_decision == "APPROVED")

    cycle_dur <- vapply (prs$reviews, function (i) {
        dates <- as.Date (i$submitted_at)
        ret <- NA_real_
        if (length (dates) > 1L) {
            dt <- diff (dates, units = "days")
            ret <- mean (as.integer (dt))
        }
        return (ret)
    }, numeric (1L))
    cycle_dur <- as.numeric (cycle_dur)
    cycle_dur_mn <- mean (cycle_dur [index_approved], na.rm = TRUE)
    cycle_dur_md <- stats::median (cycle_dur [index_approved], na.rm = TRUE)

    review_dur <- difftime (prs$closed_at, prs$created_at, units = "days")
    review_dur <- as.numeric (review_dur)
    review_dur_mn <- mean (review_dur [index_approved], na.rm = TRUE)
    review_dur_md <- stats::median (review_dur [index_approved], na.rm = TRUE)

    c (
        cycle_dur_mn = cycle_dur_mn,
        cycle_dur_md = cycle_dur_md,
        review_dur_mn = review_dur_mn,
        review_dur_md = review_dur_md
    )
}

cm_metric_pr_cmt_count <- function (path, end_date = Sys.Date ()) {

    pr_dat <- get_prs_in_period (path, end_date) # in cm-metrics-change-req.R
    comment_counts <- c (
        pr_dat$n_comments_per_approved,
        pr_dat$n_comments_per_rejected,
        pr_dat$n_comments_per_other
    )
    if (all (is.na (comment_counts))) {
        comment_counts <- 0L
    }

    return (mn_med_sum (comment_counts))
}

#' Measure response duration to PRs, but only from primary contributors.
#' https://chaoss.community/kb/metric-time-to-first-response/
#'
#' @return Difftime vector of response durations in days
#' @noRd
cm_metric_pr_response_durations <- function (path, end_date = Sys.Date ()) {

    contribs <- rm_data_contribs_from_gh_api (path)
    contribs <- unique (contribs$login)

    prs <- get_prs_in_period (path, end_date) # in cm-metrics-change-req.R
    pr_opened <- as.Date (prs$created_at)

    # First comment by a repo contributor:
    cmt_dates <- vapply (prs$comments, function (i) {
        i <- i [which (i$author %in% contribs), ]
        if (nrow (i) == 0L) {
            return ("")
        }
        as.character (min (as.Date (i$created_at)))
    }, character (1L))
    cmt_dates <- as.Date (cmt_dates)

    # First review by anybody:
    rev_dates <- vapply (prs$reviews, function (i) {
        if (nrow (i) == 0L) {
            return ("")
        }
        as.character (min (as.Date (i$submitted_at)))
    }, character (1L))
    rev_dates <- as.Date (rev_dates)

    closed_dates <- as.Date (prs$closed_at)

    first_date <- vapply (seq_len (nrow (prs)), function (i) {
        dates_i <- c (cmt_dates [i], rev_dates [i], closed_dates [i])
        as.character (min (dates_i, na.rm = TRUE))
    }, character (1L))
    first_date <- as.Date (first_date)

    durations <- difftime (first_date, pr_opened, units = "days")

    return (durations)
}

cm_metric_pr_age <- function (path, end_date = Sys.Date ()) {

    # Suppress no visible binding notes:
    created_at <- closed_at <- NULL

    pr_dat <- get_prs_in_period (path, end_date) |>
        dplyr::mutate (
            created_at = as.Date (created_at),
            closed_at = as.Date (closed_at)
        )
    pr_dat$closed_at [which (is.na (pr_dat$closed_at))] <- end_date

    ages <- difftime (pr_dat$closed_at, pr_dat$opened_at, units = "days")

    return (mn_med_sum (ages))
}

#' CHAOSS metric "Change Request Reviews", which assesses "to what extent are
#' change requests put through a formal review process using platform
#' features?" This is assessed here as the simple propotion of all merged PRs
#' which were officially "approved".
#'
#' \url{https://chaoss.community/kb/metric-change-request-reviews/}
#'
#' @noRd
cm_metric_pr_reviews_approved <- function (path, end_date = Sys.Date ()) {

    pr_dat <- get_prs_in_period (path, end_date)

    ret <- NA_real_
    if (nrow (pr_dat) > 0L) {
        ret <- length (which (pr_dat$review_decision == "APPROVED")) /
            nrow (pr_dat)
    }
    return (ret)
}
