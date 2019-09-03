
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inteRmodel

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/llrs/inteRmodel.svg?branch=master)](https://travis-ci.org/llrs/inteRmodel)
[![Codecov test
coverage](https://codecov.io/gh/llrs/inteRmodel/branch/master/graph/badge.svg)](https://codecov.io/gh/llrs/inteRmodel?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of inteRmodel is to produce interaction models using
[RGCCA](https://cran.r-project.org/package=RGCCA).

If the CRAN version is too slow you could try [my
fork](https://www.github.com/llrs/RGCCA) which has some more
dependencies but is much faster.

## Installation

You can install the released version of inteRmodel from
[Github](https://www.github.com/llrs/inteRmodel) with:

``` r
devtools::install_github("llrs/inteRmodel")
```

## Example

This is a basic example which shows you how to apply the bootstraping on
this analysis:

``` r
library(inteRmodel)
#> 
data("Russett", package = "RGCCA")
X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                  "dictator")])
A <- list(X_agric, X_ind, X_polit)
C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
out <- boot_samples_sgcca(A = A, C = C, c1 = rep(1, 3), nb_boot = 10)
```

We can see some results simply by using:

``` r
head(out$AVE)
#>          inner     outer
#> [1,] 0.3109716 0.7092233
#> [2,] 0.3316143 0.6345073
#> [3,] 0.4731394 0.6667931
#> [4,] 0.4328954 0.6360925
#> [5,] 0.3979463 0.6598980
#> [6,] 0.4219517 0.7091110
```
