generate_test_org_data <- function () {

    path <- generate_test_pkg ()

    org_dir <- fs::path (fs::path_temp (), "org")
    if (!fs::dir_exists (org_dir)) {
        fs::dir_create (org_dir)
    }
    path1 <- fs::dir_copy (path, fs::path (org_dir, "testpkg1"))
    path2 <- fs::dir_copy (path, fs::path (org_dir, "testpkg2"))
    fs::dir_delete (path)

    d1 <- desc::desc_set ("Package" = "testpkg1", file = path1)
    d2 <- desc::desc_set ("Package" = "testpkg2", file = path2)

    pkg_paths <- fs::dir_ls (org_dir, type = "directory", recurse = FALSE)

    # Then modify each package to call the other one:
    modify_one <- function (path, pkg_to_add = "testpkg1") {
        f <- fs::path (path, "R", "utils.R")
        u <- readLines (f)
        i <- grep ("return\\s\\(x\\)$", u)
        u [i] <- paste0 ("    return (", pkg_to_add, "::f (x))")
        writeLines (u, f)
    }
    modify_one (path1, "testpkg2")
    modify_one (path2, "testpkg1")

    return (pkg_paths)
}
