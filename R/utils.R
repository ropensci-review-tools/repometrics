#' Return a tripartite vector of (mean, sd, median, sum) of a given input vector
#'
#' @param x A numberic vector
#' @return Named vector of three numeric values of (mean, median, sum)
#' @noRd
mn_med_sum <- function (x) {
    ret <- c (mean = NA_real_, sd = NA_real_, median = NA_real_, sum = NA_real_)
    if (length (x) > 0) {
        ret <- c (
            mean = mean (x, na.rm = TRUE),
            sd = stats::sd (x, na.rm = TRUE),
            median = stats::median (x, na.rm = TRUE),
            sum = sum (x, na.rm = TRUE)
        )
    }
    return (ret)
}

#' Convert single values of length 0 or NULL to `NA_integer`.
#' @noRd
null2na_int <- function (x) {
    ifelse (length (x) == 0, NA_integer_, x)
}

#' Convert single values of length 0 or NULL to `NA_character_`.
#' @noRd
null2na_char <- function (x) {
    ifelse (length (x) == 0, NA_character_, x)
}

set_num_cores <- function (num_cores) {
    if (num_cores <= 0L) {
        num_cores <- parallel::detectCores () + num_cores
    }
    num_cores <- min (num_cores, parallel::detectCores ())
    if (num_cores < 1L) {
        cli::cli_abort ("Number of cores must be at least 1")
    }
    if (nzchar (Sys.getenv ("GITHUB_WORKFLOW", ""))) {
        # Force single core on all gha workflows:
        num_cores <- 1L
    }
    return (num_cores)
}

to_posix <- function (x) {
    as.POSIXct (x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
}

n_per_page_in_tests <- function (n_per_page) {
    is_test_env <- Sys.getenv ("REPOMETRICS_TESTS") == "true"
    ifelse (is_test_env, 2L, n_per_page)
}

# Fn used in collating all metrics over ranges of end dates:
get_end_date_seq <- function (end_date = Sys.Date (),
                              period = get_repometrics_period (),
                              num_years = 3) {

    num_periods <- floor (num_years * 365.25 / period)
    period_seq <- seq_len (num_periods) * period - period
    end_dates <- end_date - period_seq

    # Adjust days to same day as 'end_date', but no date-time pkgs here:
    if (period > 30) {
        end_dates <- as.character (end_dates)
        days <- regmatches (end_dates, regexpr ("[0-9]+$", end_dates))
        # re-adjust months where period shifts back to last few days:
        index <- which (as.integer (days) > 25)
        end_dates [index] <- gsub ("[0-9]+$", "01", end_dates [index])
        end_dates [index] <- vapply (end_dates [index], function (i) {
            m <- regmatches (i, regexpr ("\\-[0-9]+\\-", i))
            m_num <- as.integer (gsub ("\\-", "", m))
            m_plus_1 <- paste0 ("-", sprintf ("%02d", m_num + 1), "-")
            gsub (m, m_plus_1, i)
        }, character (1L))

        end_dates <- as.Date (end_dates)
    }

    return (end_dates)
}

is_verbose <- function () {
    getOption ("rlang_message_verbosity", "") == "verbose"
}
