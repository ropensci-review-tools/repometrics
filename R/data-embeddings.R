rm_org_data_embeddings <- function (org_paths) {

    requireNamespace ("pkgmatch")

    pkg_names <- get_all_pkg_names (org_paths)

    embeddings <- pkgmatch::pkgmatch_embeddings_from_pkgs (pkg_names$paths)

    return (embeddings)
}

rm_org_emb_distances <- function (org_paths, embeddings_data = NULL, what = "code") {

    what <- match.arg (what, c ("text_with_fns", "text_wo_fns", "code"))

    if (is.null (embeddings_data)) {
        embeddings_data <- rm_org_data_embeddings (org_paths)
    } else {
        check_embeddings_param (embeddings_data)
    }

    this_mat <- embeddings_data [[what]]
    npkgs <- ncol (embeddings_data [[what]])
    embeddings <- lapply (seq_len (npkgs), function (i) {
        this_vec <- this_mat [, i]
        cosine_similarity (this_vec, this_mat)
    })
    emb_matrix <- do.call (cbind, embeddings)
    diag (emb_matrix) <- NA_real_

    return (emb_matrix)
}

# Adapted from pkgmatch/R/similarity-metrics.R, but to just return vector of
# similarities.
cosine_similarity <- function (this_vec, this_mat) {

    nrow <- length (this_vec)
    ncol <- ncol (this_mat)
    emb_mat <- matrix (this_vec, nrow = nrow, ncol = ncol)

    cs_num <- colSums (emb_mat * this_mat)
    cs_denom <- sqrt (colSums (emb_mat^2) * colSums (this_mat^2))
    cs <- cs_num / cs_denom

    return (cs)
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
