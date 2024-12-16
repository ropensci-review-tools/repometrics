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
}
