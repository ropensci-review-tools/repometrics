#' Construct user-by-user square matrices of strengths of relation between
#' users.
#'
#' @param data_users Result of `lapply(logins, repometrics_data_user)`.
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
user_relation_matrices <- function (data_users) {

    # Suppress no visible binding notes:
    followers <- following <- org_repo <- repo <- login <- num_comments <- NULL

    user_names <- names (data_users)
    data_users <- add_user_login_cols (data_users) |>
        combine_user_data ()

    # Pre-processing to name grouping column "repo" and count column "n":
    data_users$commit_cmt$repo <-
        paste0 (data_users$commit_cmt$org, data_users$commit_cmt$repo)

    data_users$followers <-
        dplyr::rename (data_users$followers, repo = followers) |>
        dplyr::mutate (n = 1L)
    data_users$following <-
        dplyr::rename (data_users$following, repo = following) |>
        dplyr::mutate (n = 1L)

    data_users$issue_cmts <-
        dplyr::rename (data_users$issue_cmts, repo = org_repo) |>
        dplyr::group_by (repo, login) |>
        dplyr::summarise (n = sum (num_comments), .groups = "keep")
    data_users$issues <- dplyr::rename (data_users$issues, repo = org_repo) |>
        dplyr::group_by (repo, login) |>
        dplyr::summarise (n = dplyr::n (), .groups = "keep")

    overlap <- lapply (names (data_users), function (n) {
        data_users [[n]] <-
            user_relate_fields (data_users, user_names, what = n)
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
add_user_login_cols <- function (data_users) {

    nms <- names (data_users)
    res <- lapply (seq_along (data_users), function (u) {
        nms_u <- names (data_users [[u]])
        res_u <- lapply (seq_along (data_users [[u]]), function (i) {
            ud <- data_users [[u]] [[i]]
            if (is.data.frame (ud) && nrow (ud) > 0L) {
                ud$login <- names (data_users) [u]
            } else if (is.character (ud)) {
                login <- names (data_users) [i]
                ud <- data.frame (ud, login = rep (login, length (ud)))
                names (ud) [1] <- names (data_users [[u]]) [i]
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
combine_user_data <- function (data_users) {

    data <- lapply (names (data_users [[1]]), function (n) {
        these <- lapply (data_users, function (i) i [[n]])
        res <- do.call (rbind, these)
        rownames (res) <- NULL
        return (res)
    })

    names (data) <- names (data_users [[1]])
    data$general <- NULL

    return (data)
}

user_relate_fields <- function (data_users, user_names, what = "commits") {

    # Suppress no visible binding notes:
    num_commits <- login <- repo <- n <- NULL

    user_combs <- t (utils::combn (user_names, m = 2L))
    if (what == "commits") {
        data_users [[what]] <-
            dplyr::rename (data_users [[what]], n = num_commits)
    } else if (what == "commit_cmt") {
        data_users$commit_cmt$n <- 1L
    }

    res <- apply (user_combs, 1, function (i) {
        cmt1 <- dplyr::filter (data_users [[what]], login == i [1]) |>
            dplyr::group_by (repo) |>
            dplyr::summarise (n1 = sum (n))
        cmt2 <- dplyr::filter (data_users [[what]], login == i [2]) |>
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
