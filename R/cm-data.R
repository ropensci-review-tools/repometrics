cm_data <- function (path) {

    data_fns <- get_cm_data_fns ()

    if (all_cm_data_fns_memoised (data_fns, path)) {
        res <- lapply (data_fns, function (i) {
            do.call (i, list (path))
        })
    } else {
        res <- pbapply::pblapply (data_fns, function (i) {
            do.call (i, list (path))
        })
    }
    names (res) <- gsub ("^cm\\_data\\_", "", data_fns)

    return (res)
}

get_cm_data_fns <- function () {

    pkg_fns <- ls (envir = asNamespace ("repometrics"))
    data_fns <- grep ("^cm\\_data\\_", pkg_fns, value = TRUE)
    data_fns [which (!grepl ("\\_internal$", data_fns))]
}

all_cm_data_fns_memoised <- function (data_fns, path) {
    is_memoised <- vapply (data_fns, function (i) {
        tryCatch (
            memoise::has_cache (get (i)) (path),
            error = function (e) FALSE
        )
    }, logical (1L))

    length (which (is_memoised)) > (length (data_fns) / 2)
}
