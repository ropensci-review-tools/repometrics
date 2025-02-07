test_that ("function call network", {

    pkg_paths <- generate_test_org_data ()
    org_path <- unique (fs::path_dir (pkg_paths))

    fn_calls <- rm_org_data_fn_call_network (org_path)

    fs::dir_delete (org_path)

    expect_s3_class (fn_calls, "data.frame")
    expect_true (nrow (fn_calls) > 0L)
    expect_named (fn_calls, c ("source", "package", "num_fns", "num_calls"))
    expect_type (fn_calls$source, "character")
    expect_type (fn_calls$package, "character")
    expect_type (fn_calls$num_fns, "integer")
    expect_type (fn_calls$num_calls, "integer")
    expect_true (all (fn_calls$num_fns) > 0L)
    expect_true (all (fn_calls$num_calls) > 0L)
})
