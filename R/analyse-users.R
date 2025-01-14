#' Construct user-by-user square matrices of strengths of relation between
#' users.
#'
#' @param user_data Result of `lapply(logins, repometrics_data_user)`.
#' Contains the following fields:
#' \enumerate{
#' \item general (not considered here)
#' \item commit_cmt Comments on commits
#' \item commits Commits to different repositories
#' \item followers GitHub followers
#' \item following Logins of people/orgs followed by user on GitHub
#' \item issue_cmts Comments on issues
#' \item issues Issues opened by user.
#' }
#' @return A `data.frame` of pairwise user logins, and proportions of overlap
#' betwen repositories in the six variables described above.
#' @noRd
user_relation_matrices <- function (user_data) {

    # Suppress no visible binding notes:
    followers <- following <- org_repo <- repo <- login <- num_comments <- NULL

    user_names <- names (user_data)
    user_data <- add_user_login_cols (user_data) |>
        combine_user_data ()

    # Pre-processing to name grouping column "repo" and count column "n":
    user_data$commit_cmt$repo <-
        paste0 (user_data$commit_cmt$org, user_data$commit_cmt$repo)

    user_data$followers <-
        dplyr::rename (user_data$followers, repo = followers) |>
        dplyr::mutate (n = 1L)
    user_data$following <-
        dplyr::rename (user_data$following, repo = following) |>
        dplyr::mutate (n = 1L)

    user_data$issue_cmts <-
        dplyr::rename (user_data$issue_cmts, repo = org_repo) |>
        dplyr::group_by (repo, login) |>
        dplyr::summarise (n = sum (num_comments), .groups = "keep")
    user_data$issues <- dplyr::rename (user_data$issues, repo = org_repo) |>
        dplyr::group_by (repo, login) |>
        dplyr::summarise (n = dplyr::n (), .groups = "keep")

    overlap <- lapply (names (user_data), function (n) {
        user_data [[n]] <- user_relate_fields (user_data, user_names, what = n)
    })

    res <- dplyr::left_join (
        overlap [[1]],
        overlap [[2]],
        by = c ("login1", "login2")
    ) |>
        dplyr::left_join (overlap [[3]], by = c ("login1", "login2")) |>
        dplyr::left_join (overlap [[4]], by = c ("login1", "login2")) |>
        dplyr::left_join (overlap [[5]], by = c ("login1", "login2")) |>
        dplyr::left_join (overlap [[6]], by = c ("login1", "login2"))

    return (res)
}

#' Add 'login' columns to all user data, so each element can be combined.
#' @noRd
add_user_login_cols <- function (user_data) {

    nms <- names (user_data)
    res <- lapply (seq_along (user_data), function (u) {
        nms_u <- names (user_data [[u]])
        res_u <- lapply (seq_along (user_data [[u]]), function (i) {
            ud <- user_data [[u]] [[i]]
            if (is.data.frame (ud) && nrow (ud) > 0L) {
                ud$login <- names (user_data) [u]
            } else if (is.character (ud)) {
                ud <- data.frame (ud, login = names (user_data) [u])
                names (ud) [1] <- names (user_data [[u]]) [i]
            }
            return (ud)
        })
        names (res_u) <- nms_u

        return (res_u)
    })
    names (res) <- nms

    return (res)
}

#' Combine all individual elements of 'user_data' for all users.
#'
#' The `add_user_login_cols` enables all data to be `rbind`-ed here.
#' @noRd
combine_user_data <- function (user_data) {

    data <- lapply (names (user_data [[1]]), function (n) {
        these <- lapply (user_data, function (i) i [[n]])
        res <- do.call (rbind, these)
        rownames (res) <- NULL
        return (res)
    })

    names (data) <- names (user_data [[1]])
    data$general <- NULL

    return (data)
}

user_relate_fields <- function (user_data, user_names, what = "commits") {

    # Suppress no visible binding notes:
    num_commits <- login <- repo <- n <- NULL

    user_combs <- t (utils::combn (user_names, m = 2L))
    if (what == "commits") {
        user_data [[what]] <-
            dplyr::rename (user_data [[what]], n = num_commits)
    } else if (what == "commit_cmt") {
        user_data$commit_cmt$n <- 1L
    }

    res <- apply (user_combs, 1, function (i) {
        cmt1 <- dplyr::filter (user_data [[what]], login == i [1]) |>
            dplyr::group_by (repo) |>
            dplyr::summarise (n1 = sum (n))
        cmt2 <- dplyr::filter (user_data [[what]], login == i [2]) |>
            dplyr::group_by (repo) |>
            dplyr::summarise (n2 = sum (n))
        overlap <- dplyr::inner_join (cmt1, cmt2, by = "repo")

        res <- 0
        if (nrow (overlap) > 0L) {
            res <- (sum (overlap$n1) + sum (overlap$n2)) /
                (sum (cmt1$n1) + sum (cmt2$n2))
        }
        return (res)
    })

    res <- data.frame (
        login1 = user_combs [, 1],
        login2 = user_combs [, 2],
        res
    )
    names (res) [3] <- what

    return (res)
}
