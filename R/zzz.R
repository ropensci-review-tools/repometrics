# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    op <- options ()

    op.githist <- list (
        githist_period = 90
    )

    toset <- !(names (op.githist) %in% names (op))
    if (any (toset)) {
        options (op.githist [toset])
    }
    invisible ()
}
# nocov end

get_githist_period <- function () {
    period <- getOption ("githist_period")
    checkmate::assert_int (period, lower = 1L)
    return (period)
}
