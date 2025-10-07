main_contributors <- function (path,
                               end_date = Sys.Date (),
                               threshold = 0.9,
                               all_ctbs = FALSE) {

    # suppress no visible warning notes:
    login <- n <- NULL

    if (!all_ctbs) {
        log <- git_log_in_period (path, end_date = end_date)
    } else {
        log <- rm_data_gitlog (path)
    }
    contribs <- rm_data_contribs_from_gh_api (path)

    index <- match (log$aut_email, contribs$email)
    log$login <- contribs$login [index]
    index <- which (is.na (log$login))
    index2 <- match (tolower (log$aut_name), tolower (contribs$name)) [index]
    log$login [index] <- contribs$login [index2] [index]

    log_contribs <- dplyr::filter (log, !is.na (login)) |>
        dplyr::group_by (login) |>
        dplyr::summarise (n = dplyr::n ()) |>
        dplyr::arrange (dplyr::desc (n)) |>
        dplyr::mutate (sum = cumsum (n) / sum (n))

    index <- which (log_contribs$sum <= threshold)
    # Then include the next entry as well, if it exists, because for example
    # with only two entries the first may be skipped if it's below the
    # threshold.
    index <- c (index, length (index) + 1L)

    logins <- log_contribs$login [index]
    logins <- logins [which (!is.na (logins))]

    return (logins)
}
