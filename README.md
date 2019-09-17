
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

The goal of inteRmodel is to help you with interaction models using
[RGCCA](https://cran.r-project.org/package=RGCCA) to asses the stability
of the model and the best model possible given the data provided. The
package assumes that the blocks are all connected.

You can apply bootstraping to the models with `search_model`, then
`iterate_model` or bootstrap the samples with `boot_samples_sgcca` and
`boot_index_sgcca`.

For further information about the regularized canonical correlations and
the interpretation read the
[RGCCA](https://cran.r-project.org/package=RGCCA) vignette and the
associated articles.

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
set.seed(879138)
boots <- 10
C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
boot_i <- boot_samples_sgcca(A = A, C = C, c1 = rep(1, 3), nb_boot = boots)
```

We can see the
[AVE](https://en.wikipedia.org/wiki/Average_variance_extracted) of the
bootstraps by using:

``` r
head(boot_i$AVE)
#>          inner     outer
#> [1,] 0.3003373 0.6522110
#> [2,] 0.3871677 0.6821278
#> [3,] 0.4648659 0.6416236
#> [4,] 0.4732215 0.7039174
#> [5,] 0.4903151 0.6333550
#> [6,] 0.3474681 0.6248818
```

The AVE scores is for each bootstrap sample, which help to decide which
is the stability of the model.

See the vignette for a full example.
