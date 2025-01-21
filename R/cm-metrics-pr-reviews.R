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

    prs <- get_prs_in_period (path, end_date) # in cm-metrics-change-req.R

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

    pr_dat <- cm_metric_pr_reviews (path, end_date = end_date)
    comment_counts <- c (
        pr_dat$n_comments_per_approved,
        pr_dat$n_comments_per_rejected,
        pr_dat$n_comments_per_other
    )
    if (all (is.na (comment_counts))) {
        comment_counts <- 0L
    }

    c (
        mean = mean (comment_counts, na.rm = TRUE),
        median = stats::median (comment_counts, na.rm = TRUE)
    )
}
