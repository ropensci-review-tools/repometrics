rm_data_test_coverage_internal <- function (path, end_date = NULL) {

    requireNamespace ("readr", quietly = TRUE)

    or <- org_repo_from_path (path)

    readme <- fs::dir_ls (path, regexp = "README\\.md$")
    if (length (readme) == 0L) {
        return (NA_real_)
    }

    readme <- readr::read_lines (readme, progress = FALSE)
    codecov <- grep ("codecov\\.io.*\\.svg", readme, value = TRUE)
    if (length (codecov) == 0L) {
        return (NA_real_)
    }
    # That may not be separated by a linebreak, so:
    codecov <- strsplit (codecov, "\\[\\!\\[|\\s") [[1]]
    codecov <- grep ("codecov\\.io.*\\.svg", codecov, value = TRUE)
    if (length (codecov) == 0L) {
        return (NA_real_)
    }
    ptn <- "http(s?)\\:.*\\.svg"
    badge_svg <- regmatches (codecov, gregexpr (ptn, codecov)) [[1]]
    if (is.null (badge_svg)) {
        return (NA_real_)
    }
    # Have to use httr2 to enable mocking via httptest2:
    resp <- httr2::request (badge_svg) |>
        httr2::req_retry (max_tries = 5L) |>
        httr2::req_perform ()

    if (httr2::resp_is_error (resp)) {
        return (NA_real_)
    }

    badge <- httr2::resp_body_string (resp)

    # No XML/HTML parsing pkgs here, so grep for text:
    text_in <- gregexpr ("<text", badge) [[1]]
    text_out <- gregexpr ("<\\/text", badge) [[1]]
    if (length (text_in) != length (text_out)) {
        return (NA_real_)
    }
    text <- apply (cbind (text_in, text_out), 1, function (j) {
        res <- substring (badge, j [1], j [2]) [[1]]
        gsub ("^.*>|<*$", "", res)
    })
    coverage <- unique (grep ("%", text, value = TRUE))
    if (length (coverage) > 0L) {
        coverage <- max (unique (as.numeric (gsub ("%", "", coverage))))
    } else {
        coverage <- NA_real_
    }
    return (coverage)
}

rm_metric_test_coverage_internal <- function (path, end_date = Sys.Date ()) {
    rm_data_test_coverage_internal (path, end_date) / 100
}
rm_metric_test_coverage <- memoise::memoise (rm_metric_test_coverage_internal)

rm_metric_test_coverage_url <- function () {
    "metric-test-coverage"
}
