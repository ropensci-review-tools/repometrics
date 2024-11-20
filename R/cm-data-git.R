cm_data_gitlog_internal <- function (path) {

    cmt <- git2r::commits (repo = path)

    hash <- vapply (cmt, function (i) i$sha, character (1L))
    aut_name <- vapply (cmt, function (i) i$author$name, character (1L))
    aut_email <- vapply (cmt, function (i) i$author$email, character (1L))
    timestamp <- vapply (cmt, function (i) as.character (i$author$when), character (1L))
    cmt_message <- vapply (cmt, function (i) i$message, character (1L))

    stats <- lapply (
        hash,
        function (i) gert::git_commit_stats (ref = i, repo = path)
    )

    lines_added <- vapply (stats, function (i) i$insertions, integer (1L))
    lines_removed <- vapply (stats, function (i) i$deletions, integer (1L))
    nfiles_changed <- vapply (stats, function (i) i$files, integer (1L))

    diffs <- lapply (
        hash,
        function (i) gert::git_diff (ref = i, repo = path)
    )

    files_changed <- lapply (diffs, function (i) i$new)

    # bench::marking shows this form is quicker than either:
    # `length (grep ("^(\\-|\\+)$", j))` or
    # `length (which (j %in% c ("-", "+")))`.
    whitespace <- vapply (diffs, function (i) {
        patch_i <- strsplit (i$patch, "\\n")
        res <- vapply (patch_i, function (j) {
            c (length (which (j == "+")), length (which (j == "-")))
        }, integer (2L))
        as.integer (rowSums (res))
    }, integer (2L))

    data.frame (
        hash = hash,
        aut_name = aut_name,
        aut_email = aut_email,
        timestamp = timestamp,
        message = cmt_message,
        nfiles_changed = nfiles_changed,
        lines_added = lines_added,
        lines_removed = lines_removed,
        whitespace_added = whitespace [1, ],
        whitespace_removed = whitespace [2, ]
    )
}
cm_data_gitlog <- memoise::memoise (cm_data_gitlog_internal)
