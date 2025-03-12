cm_data_test_coverage <- function (path, end_date = NULL) {

    requireNamespace ("readr")

    or <- org_repo_from_path (path)

    readme <- fs::dir_ls (path, regexp = "README\\.md$")
    if (length (readme) == 0L) {
        return (NA_real_)
    }

    readme <- readr::read_lines (readme, progress = FALSE)
    codecov <- grep ("codecov\\.io.*\\.svg", readme, value = TRUE)
    ptn <- "https\\:.*\\.svg"
    badge_svg <- regmatches (codecov, gregexpr (ptn, codecov)) [[1]]
    # Have to use httr2 to enable mocking via httptest2:
    badge <- httr2::request (badge_svg) |>
        httr2::req_perform ()
    httr2::resp_check_status (badge)
    badge <- httr2::resp_body_string (badge)

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

cm_metric_test_coverage_internal <- function (path, end_date = Sys.Date ()) {
    cm_data_test_coverage (path, end_date) / 100
}
cm_metric_test_coverage <- memoise::memoise (cm_metric_test_coverage_internal)

cm_metric_test_coverage_url <- function () {
    "metric-test-coverage"
}
