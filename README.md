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

The main data-gathering function requires just one parameter specifying the
path to a local source repository:

``` r
data <- repometrics_data_repo (path)
```

The results can then be visualised as an interactive dashboard by running this
line:

``` r
repometrics_dashboard (data)
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

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
