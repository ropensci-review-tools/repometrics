<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/repometrics/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/repometrics/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/repometrics/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci-review-tools/repometrics)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# repometrics

Metrics for your code repository.

## But why?

There are lots of tools for collating metrics of software repositories, most of
which are commercial and not open source. Notable open source tools include
those in the [github.com/chaoss organization](https://github.com/chaoss),
especially their [augur](https://github.com/chaoss/augur) and
[grimoirelab](https://github.com/chaoss/grimoirelab) tools. Both of these tools
are huge and comprehensive. They are intended to be highly configurable and
customizable, but can be difficult both to set up and to use.

The `repometrics` package is a lightweight alternative to CHAOSS tools. Where
those tools use large database and data broker libraries, this package stores
all data in raw JSON form as directly returned from APIs, and uses tools such
as [`jq`](https://jqlang.github.io/jq/) for extraction and filtering. Unlike
the more general CHAOSS tools, the `repometrics` package is also restricted to
analysing GitHub repositories only.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
