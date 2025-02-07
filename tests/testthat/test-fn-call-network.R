end_date <- as.Date ("2024-08-01")

test_that ("function call network", {

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

    org_paths <- fs::dir_ls (org_dir, type = "directory", recurse = FALSE)
    fn_calls <- rm_data_fn_call_network (org_paths)

    fs::dir_delete (c (path1, path2))

    expect_null (fn_calls)
})
