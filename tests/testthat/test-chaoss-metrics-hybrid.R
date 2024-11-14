test_that ("chaoss metric has_ci", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    pkg <- system.file ("extdata", "testpkg.zip", package = "repometrics")
    flist <- unzip (pkg, exdir = fs::path_temp ())
    path <- fs::path_dir (flist [1])

    chk <- repo_has_ci_files (path)
    expect_length (chk, 0L)

    d <- fs::dir_create (fs::path (path, ".github", "workflows"))
    f <- fs::path (d, "workflow.yaml")
    writeLines ("a", f)

    chk <- repo_has_ci_files (path)
    expect_length (chk, 1L)
    expect_equal (chk, "github")

    # Test cli::cli_alert_warning output:
    expect_snapshot (chk <- chaoss_metric_has_ci (path))
    expect_true (chk)

    fs::dir_delete (path)
})
