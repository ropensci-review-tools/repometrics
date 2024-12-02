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
