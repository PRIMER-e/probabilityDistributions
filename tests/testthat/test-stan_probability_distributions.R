library(probabilityDistributions)

# TODO: Produce a warning if rstan is not installed.
if (requireNamespace("rstan", quietly = TRUE)) {

    stan_function_names <- available_stan_probability_distributions[["function_name"]]

    stan_function_definitions <- vapply(stan_function_names,
                                        function(function_name) stan_probability_distribution(function_name),
                                        character(1))

    stan_model_code <- paste("functions {\n",
                             stan_function_definitions,
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
