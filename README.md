
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multivaR

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/Echisholm21/multivaR.svg?branch=master)](https://travis-ci.com/Echisholm21/multivaR)
<!-- badges: end -->

The goal of multivaR is to organize and simplify the process of creating
a multivariate analysis. This package can format data as well as
caluclate anomalies from raw data, once data is formatted the package
can be used to run Principal Components Analysis on multiple variables.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Echisholm21/multivaR")
```

multivaR is designed to work with other packages developed by the Data
Access team at Bedford Institute of Oceanogrpahy (BIO). Specifically,
this package is designed to work with data which is supplied through the
`azmpdata` package. This supplementary package can be installed by
using:

``` r
devtools::install_github("casaultb/azmpdata")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(multivaR)
## basic example code
```