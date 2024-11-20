cm_data_gitlog <- function (path) {

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

    whitespace <- vapply (diffs, function (i) {
        patch_i <- strsplit (i$patch, "\\n")
        sum (vapply (patch_i, function (j) {
            length (grep ("^(\\-|\\+)$", j))
        }, integer (1L)))
    }, integer (1L))


    data.frame (
        hash = hash,
        aut_name = aut_name,
        aut_email = aut_email,
        timestamp = timestamp,
        message = cmt_message,
        nfiles_changed = nfiles_changed,
        lines_added = lines_added,
        lines_removed = lines_removed,
        whitespace = whitespace
    )
}
