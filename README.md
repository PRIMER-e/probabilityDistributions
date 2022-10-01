
<!-- README.md is generated from README.Rmd. 
     Please edit that file and use `devtools::build_readme()` to rebuild README.md -->

# probabilityDistributions

<!-- badges: start -->

[![R-CMD-check](https://github.com/PRIMER-e/probabilityDistributions/workflows/R-CMD-check/badge.svg)](https://github.com/PRIMER-e/probabilityDistributions/actions)
[![Codecov test
coverage](https://codecov.io/gh/PRIMER-e/probabilityDistributions/branch/main/graph/badge.svg)](https://app.codecov.io/gh/PRIMER-e/probabilityDistributions?branch=main)
<!-- badges: end -->

The goal of probabilityDistributions is to provide a library of useful
probability distributions for both R and Stan.

## Installation

You can install the development version of probabilityDistributions from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PRIMER-e/probabilityDistributions")
```

## Example

This is a basic example which shows you how to calculate a Zero-Inflated
Poisson (ZIP) probability mass using both R and Stan:

``` r
library(probabilityDistributions)
library(rstan)
#> Loading required package: StanHeaders
#> 
#> rstan version 2.26.13 (Stan version 2.26.1)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores()).
#> To avoid recompilation of unchanged Stan programs, we recommend calling
#> rstan_options(auto_write = TRUE)
#> For within-chain threading using `reduce_sum()` or `map_rect()` Stan functions,
#> change `threads_per_chain` option:
#> rstan_options(threads_per_chain = 1)
#> Do not specify '-march=native' in 'LOCAL_CPPFLAGS' or a Makevars file

stan_model_code <- paste0("functions { ", 
                          zi_poisson_lpmf_stan[["source_code"]], 
                          " }",
                          sep = "\n")

model_listing <- stanc(model_code = stan_model_code)

stan_function_names <- expose_stan_functions(model_listing)

print(stan_function_names)
#> [1] "zi_poisson_lpmf"

zi_poisson_lpmf(5, 7.5, 0.2)
#> [1] -2.43612

dzip(5, 7.5, 0.2, log = TRUE)
#> [1] -2.43612
```

## Development

After making changes run the following command before committing to
rebuild the package readme, which is also the home page.

``` r
devtools::build_readme()
```
