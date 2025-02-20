<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/repometrics/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/repometrics/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/repometrics/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci-review-tools/repometrics)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# repometrics

Metrics for your code repository. A single function collates a wealth of data
from commit histories and GitHub interactions, converts it all to standardised
metrics, and displays the result as a single, interactive dashboard of your
repository.

## How?

### Installation

First, install the package either via [`r-universe`](https://r-universe.dev):

``` r
options (repos = c (
    ropenscireviewtools = "https://ropensci-review-tools.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
))
install.packages ("repometrics")
```
or directly from GitHub with one of these two lines:

``` r
remotes::install_github ("ropensci-review-tools/repometrics")
pak::pkg_install ("ropensci-review-tools/repometrics")
```

### Use

The dashboard requires two sources of data, beginning with core data of a focal
repository. Code for that repository must exist in a local directory
corresponding to the single input parameter, `path`:

``` r
data_repo <- repometrics_data_repo (path)
```

The second data source is then information on all people who have contributed
to the repository, both by direct code commits and via GitHub. The following
lines extract information on each of those contributors:

``` r
ctbs <- data_repo$rm$contribs_from_gh_api$login
data_ctbs <- lapply (ctbs, repometrics_data_user)
names (data_ctbs) <- ctbs
```

A `repometrics` dashboard for the repository can then be launched with the
following line:

``` r
repometrics_dashboard (data_repo, data_ctbs)
```

The dashboard will automatically open in your default browser.

## Prior Art

There are lots of tools for collating metrics of software repositories, most of
which are commercial and not open source. Notable open source tools include
those in the [github.com/chaoss organization](https://github.com/chaoss),
especially their [augur](https://github.com/chaoss/augur) and
[grimoirelab](https://github.com/chaoss/grimoirelab) tools. Both of these tools
are huge and comprehensive. Although intended to be highly configurable and
customizable, they can be difficult both to set up and to use. The [OpenSSF
Scorecard](https://github.com/ossf/scorecard) is a lightweight system focussed
on metrics of "security health".

Plus a host of semi-private and commercial offerings like
[codescene](https://codescene.com/), [SonarQube](https://www.sonarsource.com/),
[SonarGraph](https://www.hello2morrow.com/products/sonargraph),
[Teamscale](https://teamscale.com/), and [Understand](https://scitools.com/).

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
