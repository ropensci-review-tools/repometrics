#' Main function to return a function call network between all packages defined
#' in `org_paths`.
#'
#' Takes the result of `rm_org_data_fn_calls`, which includes details of all
#' actual functions called, and reduces down to two summary metrics of
#' connections between packages in terms of total numbers of functions called
#' by each pair of packages, and numbers of actual calls made.
#' @noRd
rm_org_data_fn_call_network <- function (org_paths) {

    # Suppress no visible binding notes:
    package <- n <- NULL

    fn_calls <- rm_org_data_fn_calls (org_paths)
    if (!is.null (fn_calls)) {
        fn_calls <- fn_calls |>
            dplyr::group_by (source, package) |>
            dplyr::summarise (
                num_fns = dplyr::n (),
                num_calls = sum (n),
                .groups = "keep"
            )
    }

    return (fn_calls)
}

#' Collate calls to all functions defined within packages of an organization.
#' @noRd
rm_org_data_fn_calls <- function (org_paths) {

    # Suppress no visible binding notes:
    fn <- NULL

    requireNamespace ("pkgmatch")

    pkg_names <- get_all_pkg_names (org_paths)

    fn_calls <- pbapply::pblapply (pkg_names$path, function (p) {

        res <- get_pkg_fn_calls (p, pkg_names)
        if (!is.null (res)) {
            res <- res |>
                dplyr::mutate (source = get_pkg_name (p), .before = 1L) |>
                dplyr::mutate (fn = gsub ("^.*\\:\\:", "", fn))
        }
        return (res)
    })

    return (do.call (rbind, fn_calls))
}

get_all_pkg_names <- function (org_paths = NULL) {

    checkmate::assert_directory_exists (org_paths)

    pkgs <- fs::dir_ls (org_paths, type = "directory", recurse = FALSE)
    pkg_names <- vapply (pkgs, function (i) {
        get_pkg_name (i)
    }, character (1L), USE.NAMES = FALSE)
    data.frame (path = as.character (pkgs), pkg_name = pkg_names)
}

get_pkg_name <- function (path) {
    tryCatch (
        suppressWarnings (
            desc::desc_get_field (key = "Package", file = path)
        ),
        error = function (e) ""
    )
}

#' Get calls within single package to all packages named in `pkg_names`.
#' @noRd
get_pkg_fn_calls_internal <- function (path, pkg_names) {

    # Suppress no visible binding notes:
    package <- name <- fn <- NULL

    fns <- pkgmatch::pkgmatch_treesitter_fn_tags (path)
    if (nrow (fns) == 0L) {
        return (NULL)
    }

    fns <- fns |>
        dplyr::mutate (package = gsub ("\\:\\:.*$", "", name), .after = name) |>
        dplyr::filter (package %in% pkg_names$pkg_name) |>
        dplyr::group_by (fn, name, package) |>
        dplyr::summarise (n = dplyr::n (), .groups = "keep") |>
        dplyr::filter (!package == get_pkg_name (path))

    return (fns)
}
get_pkg_fn_calls <- memoise::memoise (get_pkg_fn_calls_internal)
