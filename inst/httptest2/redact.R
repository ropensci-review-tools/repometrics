function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "http://cranlogs.r-pkg.org/downloads/daily/",
        "cranlogs/",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://api.github.com/repos/",
        "ghrepos/",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://api.github.com/users/",
        "ghusers/",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "ropensci-review-tools/goodpractice",
        "repo/",
        fixed = TRUE
    )

    test_repo <- "ropensci-review-tools/repometrics"
    resp <- httptest2::gsub_response (
        resp,
        paste0 (test_repo, "/actions"),
        "repo/",
        fixed = TRUE
    )

    # Timestamp pattern, where replacing with "" removes sub-dir:
    ptn <- "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}"
    resp <- httptest2::gsub_response (
        resp,
        paste0 (ptn, "\\:", ptn),
        "",
        fixed = FALSE
    )

    return (resp)
}
