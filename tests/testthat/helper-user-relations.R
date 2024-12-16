# Mock version of data constructed in data-gh-user.R
mock_user_rel_data <- function () {

    general <- list (
        user = data.frame (
            login = "me",
            name = "me too",
            email = "me@here.com",
            location = "somewhere",
            company = "noway",
            bio = NA_character_,
            avatarUrl = NA_character_,
            num_repositories = 1L,
            repos_contributed_to = 2L,
            num_starred_repos = 3L
        ),
        orgs = data.frame (
            name = "org",
            gh_org = "org",
            url = "https://github.com/org",
            web_url = NA_character_,
            location = NA_character_,
            num_members = 0L
        )
    )

    randchars <- function (len = 6L) {
        x <- sample (c (letters, LETTERS), size = len, replace = TRUE)
        paste0 (x, collapse = "")
    }
    followers <- vapply (1:10, function (i) randchars (), character (1L))
    following <- vapply (1:5, function (i) randchars (), character (1L))

    timestamp <- as.POSIXct ("2024-01-01T00:00:01")
    timestamp_minus_year <- as.POSIXct ("2023-01-01T00:00:01")

    commits <- data.frame (
        repo = paste0 ("org", c ("one", "two")),
        num_commits = 1:2,
        date = rep (timestamp, 2L)
    )

    commit_cmt <- data.frame (
        repo = commits$repo,
        num_commits = 1:2,
        date = rep (timestamp, 2L)
    )
    attr (commit_cmt, "started_at") <- timestamp_minus_year
    attr (commit_cmt, "ended_at") <- timestamp

    issues <- data.frame (
        opened_at = rep (timestamp, 2L),
        closed_at = rep (timestamp, 2L),
        org_repo = commits$repo,
        issue_num = 1:2,
        num_issue_comments = 3:4,
        num_issue_participants = 5:6,
        num_repo_languages = 7:8,
        repo_languages = I (c ("R", "C"))
    )
    attr (issues, "started_at") <- timestamp_minus_year
    attr (issues, "ended_at") <- timestamp

    issue_cmts <- data.frame (
        org_repo = commits$repo,
        issue_num = 1:2,
        created_at = rep (timestamp, 2L),
        num_comments = 1:2,
        num_participants = 3:4
    )

    # Then assemble all:
    list (
        general = general,
        commit_cmt = commit_cmt,
        commits = commits,
        followers = followers,
        following = following,
        issue_cmts = issue_cmts,
        issues = issues
    )
}
