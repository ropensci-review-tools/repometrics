#' Collate calls to all functions defined within packages of an organization.
#' @noRd
rm_data_fn_calls <- function (org_paths) {

    requireNamespace ("pkgmatch")

    pkg_names <- get_all_pkg_names (org_paths)

    pkg_paths <- fs::dir_ls (org_paths, type = "directory", recurse = FALSE)
    fn_calls <- pbapply::pblapply (pkg_paths, function (p) {

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
get_pkg_fn_calls <- function (path, pkg_names) {

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
