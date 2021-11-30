library(probabilityDistributions)

# I've put all the tests of Stan code into the same file
# so that the tests run faster.
# Compiling a Stan model takes about a minute,
# so compiling each function individually in its own test would be frustrating.

# TODO: Produce a warning if rstan is not installed.
if (requireNamespace("rstan", quietly = TRUE)) {

    stan_model_code <- paste("functions {\n",
                             zi_poisson_lpmf_stan[["source_code"]],
                             "}\n",
                             sep = "\n")

    model_listing <- rstan::stanc(model_code = stan_model_code)

    function_names <- rstan::expose_stan_functions(model_listing)

    test_that("zi_poisson_lpmf is included in the stan model", {
      expect_true("zi_poisson_lpmf" %in% function_names)
    })

    test_that("zi_poisson_lpmf matches dzip for multiple values", {
        x = sample.int(100, 1)
        lambda = runif(1, 0, 100)
        pi = runif(1, 0, 1)
        expect_equal(zi_poisson_lpmf(x, lambda, pi), dzip(x, lambda, pi, log = TRUE))
    })

    test_that("zi_poisson_lpmf matches dzip for a single value", {
        expect_equal(zi_poisson_lpmf(5, 7, 0.2), dzip(5, 7, 0.2, log = TRUE))
    })
}
