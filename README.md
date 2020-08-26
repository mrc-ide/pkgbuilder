## pkgbuilder

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/pkgbuilder/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/pkgbuilder/actions)
[![codecov.io](https://codecov.io/github/mrc-ide/pkgbuilder/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/pkgbuilder?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/mrc-ide/pkgbuilder/badge)](https://www.codefactor.io/repository/github/mrc-ide/pkgbuilder)
<!-- badges: end -->

## Installation

We require the in-development version of `pak` with its bundled library, at least for now.  Our installation instructions are a bit unusual as a result:

```r
install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
pak::pkg_install("mrc-ide/pkgbuilder")
```

## License

MIT © Imperial College of Science, Technology and Medicine
