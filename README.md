
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
#> Warning: replacing previous import 'WGCNA::cor' by 'stats::cor' when
#> loading 'RGCCA'
data("Russett", package = "RGCCA")
X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                  "dictator")])
A <- list(X_agric, X_ind, X_polit)
C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
out <- boot_sgcca(A, C, shrinkage = rep(1, 3),  nb_boot = 10)
#> 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |======                                                           |  10%
  |                                                                       
  |=============                                                    |  20%
  |                                                                       
  |====================                                             |  30%
  |                                                                       
  |==========================                                       |  40%
  |                                                                       
  |================================                                 |  50%
  |                                                                       
  |=======================================                          |  60%
  |                                                                       
  |==============================================                   |  70%
  |                                                                       
  |====================================================             |  80%
  |                                                                       
  |==========================================================       |  90%
  |                                                                       
  |=================================================================| 100%
head(out$AVE)
#>          inner     outer
#> [1,] 0.4257665 0.6336465
#> [2,] 0.4088541 0.6191353
#> [3,] 0.3948845 0.6079401
#> [4,] 0.4266925 0.6330866
#> [5,] 0.4210275 0.6084679
#> [6,] 0.4076627 0.6610109
```
