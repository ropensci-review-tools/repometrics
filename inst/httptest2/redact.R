function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "http://cranlogs.r-pkg.org/downloads/total/",
        "cranlogs/",
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
