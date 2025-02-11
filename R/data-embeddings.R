rm_org_data_embeddings <- function (org_paths, embeddings_data = NULL) {

    requireNamespace ("pkgmatch")

    pkg_names <- get_all_pkg_names (org_paths)

    embeddings <- pkgmatch::pkgmatch_embeddings_from_pkgs (pkg_names$paths)

    return (embeddings)
}

check_embeddings_param <- function (embeddings_data) {

    expected_emb_len <- 768L
    checkmate::expect_list (embeddings_data)
    checkmate::expect_names (
        names (embeddings_data),
        identical.to = c ("text_with_fns", "text_wo_fns", "code")
    )
    nrow <- unique (vapply (embeddings_data, nrow, integer (1L)))
    if (length (nrow) > 1L) {
        cli::cli_abort ("embeddings data must have same numbers of rows")
    }
    if (nrow != expected_emb_len) {
        cli::cli_abort ("embeddings data should have {expected_emb_len} rows")
    }
    ncol <- unique (vapply (embeddings_data, ncol, integer (1L)))
    if (length (ncol) > 1L) {
        cli::cli_abort ("embeddings data must have same numbers of columns")
    }
}
