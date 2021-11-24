
<!-- README.md is generated from README.Rmd. Please edit that file -->

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
Poisson (ZIP) probability mass in both R and Stan:

``` r
library(probabilityDistributions)

stan_model_code <- paste0("functions { ", dzip_stan(), " }")

model_listing <- rstan::stanc(model_code = stan_model_code)

stan_function_names <- rstan::expose_stan_functions(model_listing)

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
