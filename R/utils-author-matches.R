# Utility functions to match authors based on emails, GitHub logins, and git commit names

#' List all contributors from git log, with matches to GitHub login handles.
#'
#' @param ctbs_log The `cm$contribs_from_log` component of Chaoss metrics data.
#' @param ctbs_gh The `cm$contribs_from_gh_api` component of Chaoss metrics data.
#' @return A modified version of `ctbs_log`, with GitHub handlees assocaited
#' with each git contributors.
#'
#' @noRd
get_all_contribs <- function (ctbs_log, ctbs_gh) {

    checkmate::assert_data_frame (ctbs_log, ncols = 2L, col.names = "named")
    checkmate::assert_data_frame (ctbs_gh, ncols = 17L, col.names = "named")

    # Suppress no visible binding notes:
    handle <- email <- NULL

    ctbs_log <- dplyr::rename (ctbs_log, name = handle) |>
        dplyr::filter (!grepl ("noreply", email))

    # Match emails:
    index <- match (ctbs_log$email, ctbs_gh$email)
    ctbs_log$gh_handle <- ctbs_gh$login [index]

    # Then match names:
    index_na <- which (is.na (ctbs_log$gh_handle))
    index <- match (tolower (ctbs_log$name [index_na]), tolower (ctbs_gh$name))
    ctbs_log$gh_handle [index_na] <- ctbs_gh$login [index]

    # Then github handles in log entries:
    index_na <- which (is.na (ctbs_log$gh_handle))
    index <- match (tolower (ctbs_log$name [index_na]), tolower (ctbs_gh$login))
    ctbs_log$gh_handle [index_na] <- ctbs_gh$login [index]

    # The apply `match_string_pairs()` to fill in any missing values based on
    # the explicitly combinations in the following calls:
    ctbs_log <- match_string_pairs (ctbs_log$name, ctbs_gh$name, ctbs_log)
    ctbs_log <- match_string_pairs (ctbs_log$name, ctbs_gh$email, ctbs_log)
    ctbs_log <- match_string_pairs (ctbs_log$name, ctbs_gh$login, ctbs_log)
    ctbs_log <- match_string_pairs (ctbs_log$email, ctbs_gh$name, ctbs_log)
    ctbs_log <- match_string_pairs (ctbs_log$email, ctbs_gh$email, ctbs_log)
    ctbs_log <- match_string_pairs (ctbs_log$email, ctbs_gh$login, ctbs_log)

    return (ctbs_log)
}

#' Match pairs of strings by greatest extent of overlapping sequences,
#' potentially with gaps in between.
#'
#' This is intended to match either names, github handles, or emails. It
#' matches two sets of input strings, and identifies the closest match in the
#' second source to each value in the first. Matches have the greatest absolute
#' length of overlapping sequences, regardless of non-matching parts which may
#' intervene in either string. This is important to be able to match, for
#' example, "My Name" to "M. Name". Matches are only accepted where the
#' proportion of overlapping string exceeds the fixed value set immediately
#' below.
#'
#' @return A modified version of `ctbs_log` with an additional `gh_handle`
#' values found by succeessful matching appended to the input log.
#' @noRd
match_string_pairs <- function (name_src1, name_src2, ctbs_log) {

    match_limit <- 0.9

    index_na <- which (is.na (ctbs_log$gh_handle))

    if (length (index_na) > 0) {
        if (all (grepl ("@", name_src1))) {
            names1 <- gsub ("\\@.*$", "", name_src1 [index_na]) |>
                strsplit ("\\.|\\||\\-")
        } else {
            names1 <- strsplit (name_src1 [index_na], "\\s+")
        }
        if (all (grepl ("@", name_src2))) {
            names2 <- gsub ("\\@.*$", "", name_src2) |>
                strsplit ("\\.|\\||\\-")
        } else {
            names2 <- strsplit (name_src2, "\\s+")
        }

        matches <- lapply (seq_along (names1), function (i) {
            res <- unique (match_names (names1 [[i]], names2) [, 2:3])
            res$i <- i
            return (res)
        })
        matches <- do.call (rbind, matches)
        matches <- matches [which (matches$match >= match_limit), ]
        if (nrow (matches) > 0L) {
            ctbs_log$gh_handle [index_na] [matches$i] <- ctbs_gh$login [matches$index]
        }
    }

    return (ctbs_log)
}

#' Simple function to return all permutations of input vector, `x`.
#' @noRd
get_permutations <- function (x) {

    res <- x
    if (length (x) > 1) {
        res <- matrix (nrow = 0, ncol = length (x))
        for (i in seq_along (x)) {
            res <- rbind (res, cbind (x [i], Recall (x [-i])))
        }
    }
    return (res)
}

permute_names <- function (names) {
    names <- names_initial <- get_permutations (names)
    if (length (names) > 1L) {
        first_name <- names [, 1]
        names_initial [which (names_initial == first_name)] <- substr (first_name, 1, 1)
        names <- tolower (rbind (names, names_initial))
        names <- apply (names, 1, function (i) paste0 (i, collapse = ""))
    }
    return (names)
}

match_names <- function (name1, names2) {

    name_parts1 <- permute_names (name1)

    matches <- lapply (name_parts1, function (n1) {

        n1_sp <- strsplit (n1, "") [[1]]

        matches_n <- lapply (seq_along (names2), function (n2) {

            name_parts2 <- permute_names (names2 [[n2]])

            match_lengths <- vapply (name_parts2, function (i) {

                n2_sp <- strsplit (i, "") [[1]]

                n1_len <- length (which (!is.na (pmatch (n1_sp, n2_sp))))
                n2_len <- length (which (!is.na (pmatch (n2_sp, n1_sp))))
                if (n1_len > n2_len) {
                    res <- c (n1_len, n1_len / length (n1_sp))
                } else {
                    res <- c (n2_len, n2_len / length (n2_sp))
                }
                return (res)
            }, numeric (2L))
            # Extract items based on maximum matched length ([1, ]):
            index <- which (match_lengths [1, ] == max (match_lengths [1, ]))
            # But then return actual proportional length ([2, ]):
            data.frame (
                name = colnames (match_lengths) [index],
                match = as.numeric (match_lengths [2, index]),
                index = n2
            )
        })

        matches_val <- vapply (matches_n, function (i) max (i$match), numeric (1L))
        matches_val_max <- max (matches_val)
        matches_val_i <- which (matches_val == matches_val_max)
        res <- do.call (rbind, matches_n [matches_val_i])

        return (res)
    })

    matches <- do.call (rbind, matches)
    matches <- matches [which (matches$match == max (matches$match)), ]
    rownames (matches) <- NULL

    return (matches)
}
