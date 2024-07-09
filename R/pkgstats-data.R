run_one_pkgstats <- function (path) {

    s <- pkgstats::pkgstats (path)

    index <- which (
        s$objects$kind == "function" &
            !grepl ("^anonFunc", s$objects$fn_name) &
            !is.na (s$objects$npars)
    )
    fns <- s$objects [index, ] |>
        dplyr::select ("fn_name", "language", "loc", "npars", "has_dots", "exported", "num_doclines")
    doclines <- mn_med_sum (fns$num_doclines [which (!is.na (fns$num_doclines))])
    npars <- mn_med_sum (fns$npars)
    loc <- mn_med_sum (fns$loc)

    n_fns <- length (unique (fns$fn_name))

    package <- NULL # Suppress 'no visible binding' note.
    ext_calls <- s$external_calls |>
        dplyr::select ("call", "package") |>
        dplyr::group_by (package) |>
        dplyr::count (package) |>
        dplyr::filter (package != s$desc$package)

    base_calls <- ext_calls$n [ext_calls$package == "base"]
    n_ext_pkgs <- nrow (ext_calls) - 1L
    ext_calls <- mn_med_sum (ext_calls$n [ext_calls$package != "base"])


    list (
        package = s$desc$package,
        version = s$desc$version,
        date = s$desc$date,
        n_aut = s$desc$aut,
        n_ctb = s$desc$ctb,
        n_fns = n_fns,
        n_ext_pkgs = n_ext_pkgs,
        base_calls = base_calls,
        loc = s$loc,
        stats = data.frame (
            doclines = doclines,
            npars = npars,
            loc = loc,
            ext_calls = ext_calls
        )
    )
}
