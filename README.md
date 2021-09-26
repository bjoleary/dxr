
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dxr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dxr)](https://CRAN.R-project.org/package=dxr)
[![R-CMD-check](https://github.com/bjoleary/dxr/workflows/R-CMD-check/badge.svg)](https://github.com/bjoleary/dxr/actions?query=workflow%3AR-CMD-check)
[![lint](https://github.com/bjoleary/dxr/workflows/lint/badge.svg)](https://github.com/bjoleary/dxr/actions?query=workflow%3Alint)
<!-- badges: end -->

The goal of dxr is to …

## Installation

You can install the development version of dxr from
[GitHub](https://github.com/bjoleary/dxr) with:

``` r
# install.packages("devtools")
devtools::install_github("bjoleary/dxr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dxr)
sensitivity(
  true_positives = 29,
  false_negatives = 1,
  digits = 0.1, 
  interval = 0.95
  )
#> $estimate
#> [1] 0.9666667
#> 
#> $estimate_percent
#> [1] "96.7%"
#> 
#> $ratio
#> [1] "29/30"
#> 
#> $lower
#> [1] 0.8332961
#> 
#> $lower_percent
#> [1] "83.3%"
#> 
#> $upper
#> [1] 0.9940914
#> 
#> $upper_percent
#> [1] "99.4%"
#> 
#> $confidence_interval
#> [1] "(95% CI: 83.3%; 99.4%)"
#> 
#> $string_two_line
#> [1] "96.7% (29/30) \n(95% CI: 83.3%; 99.4%)"
#> 
#> $string
#> [1] "29/30 = 96.7% (95% CI: 83.3%; 99.4%)"
```
