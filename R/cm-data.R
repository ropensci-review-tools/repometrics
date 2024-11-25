cm_data <- function (path) {

    data_fns <- get_cm_data_fns ()

    res <- pbapply::pblapply (data_fns, function (i) {
        do.call (i, list (path))
    })
    names (res) <- gsub ("^cm\\_data\\_", "", data_fns)

    return (res)
}

get_cm_data_fns <- function () {

    pkg_fns <- ls (envir = asNamespace ("repometrics"))
    data_fns <- grep ("^cm\\_data\\_", pkg_fns, value = TRUE)
    data_fns [which (!grepl ("\\_internal$", data_fns))]
}
