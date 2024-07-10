run_one_pkgstats <- function (path, pkg_date) {

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

    fn_nms <- unique (fns [, c ("fn_name", "exported")])

    package <- NULL # Suppress 'no visible binding' note.
    ext_calls <- s$external_calls |>
        dplyr::select ("call", "package") |>
        dplyr::group_by (package) |>
        dplyr::count (package) |>
        dplyr::filter (package != s$desc$package)

    base_calls <- null2na_int (ext_calls$n [ext_calls$package == "base"])
    n_ext_pkgs <- null2na_int (nrow (ext_calls)) - 1L
    ext_calls <- mn_med_sum (ext_calls$n [ext_calls$package != "base"])

    s$loc <- cbind (
        package = s$desc$package,
        version = s$desc$version,
        date = pkg_date,
        s$loc
    )

    list (
        package = s$desc$package,
        version = s$desc$version,
        date = pkg_date,
        n_aut = s$desc$aut,
        n_ctb = s$desc$ctb,
        n_fns_tot = nrow (fn_nms),
        n_fns_exp = length (which (fn_nms$exported)),
        n_ext_pkgs = n_ext_pkgs,
        base_calls = base_calls,
        loc = s$loc,
        stats = data.frame (
            package = s$desc$package,
            version = s$desc$version,
            date = pkg_date,
            doclines = doclines,
            npars = npars,
            loc = loc,
            ext_calls = ext_calls
        )
    )
}
