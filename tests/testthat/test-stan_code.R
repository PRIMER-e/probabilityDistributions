library(probabilityDistributions)

# I've put all the tests of Stan code into the same file
# so that the tests run faster.
# Compiling a Stan model takes about a minute,
# so compiling each function individually in its own test would be frustrating.

# TODO: Produce a warning if rstan is not installed.
if (requireNamespace("rstan", quietly = TRUE)) {

    # These stubs just let us call stan built-in functions from R.
    stubs <-
"
real neg_binomial_2_lpmf_stub(int[] n, vector mu, vector phi) {
  return neg_binomial_2_lpmf(n | mu, phi);
}
"

    stan_model_code <- paste("functions {\n",
                             zi_poisson_lpmf_stan[["source_code"]],
                             zi_poisson_linked_lpmf_stan[["source_code"]],
                             zi_poisson_log_linked_lpmf_stan[["source_code"]],
                             zi_neg_binomial_2_lpmf_stan[["source_code"]],
                             zi_neg_binomial_linked_2_lpmf_stan[["source_code"]],
                             zi_neg_binomial_log_linked_2_lpmf_stan[["source_code"]],
                             stubs,
                             "}\n",
                             sep = "\n")

    model_listing <- rstan::stanc(model_code = stan_model_code)

    function_names <- rstan::expose_stan_functions(model_listing)

    # Zero-inflated Poisson

    test_that("zi_poisson_lpmf is included in the stan model", {
      expect_true("zi_poisson_lpmf" %in% function_names)
    })

    test_that("zi_poisson_lpmf matches dzip for multiple values", {
        x = sample.int(n = 20, size = 100, replace = TRUE)
        lambda = runif(n = 100, min = 0.1, max = 20)
        pi = runif(n = 100, min = 0, max = 1)
        expect_equal(zi_poisson_lpmf(x, lambda, pi),
                     sum(dzip(x = x, lambda = lambda, pi = pi, log = TRUE)))
    })

    test_that("zi_poisson_lpmf matches dzip for a single value", {
        expect_equal(zi_poisson_lpmf(5, 7, 0.2), dzip(5, 7, 0.2, log = TRUE))
    })

    # Zero-inflated Poisson Linked

    test_that("zi_poisson_linked_lpmf is included in the stan model", {
      expect_true("zi_poisson_linked_lpmf" %in% function_names)
    })

    test_that("zi_poisson_linked_lpmf matches dzipl for multiple values", {
        x = sample.int(n = 20, size = 100, replace = TRUE)
        lambda = runif(n = 100, min = 0.1, max = 20)
        gamma0 = runif(n = 100, min = -5, max = 5)
        gamma1 = runif(n = 100, min = -2, max = 2)
        expect_equal(zi_poisson_linked_lpmf(x, lambda, gamma0, gamma1),
                     sum(dzipl(x = x, lambda = lambda, gamma0 = gamma0, gamma1 = gamma1, log = TRUE)))
    })

    test_that("zi_poisson_lpmf matches dzipl for a single value", {
        expect_equal(zi_poisson_linked_lpmf(5, 7, 3.5, -1.5),
                     dzipl(x = 5, lambda = 7, gamma0 = 3.5, gamma1 = -1.5, log = TRUE))
    })

    # Zero-inflated Poisson Log-linked

    test_that("zi_poisson_log_linked_lpmf is included in the stan model", {
      expect_true("zi_poisson_log_linked_lpmf" %in% function_names)
    })

    test_that("zi_poisson_log_linked_lpmf matches dzipll for multiple values", {
        x = sample.int(n = 20, size = 100, replace = TRUE)
        lambda = runif(n = 100, min = 0.1, max = 20)
        gamma0 = runif(n = 100, min = -5, max = 5)
        gamma1 = runif(n = 100, min = -2, max = 2)
        expect_equal(zi_poisson_log_linked_lpmf(x, lambda, gamma0, gamma1),
                     sum(dzipll(x = x, lambda = lambda, gamma0 = gamma0, gamma1 = gamma1, log = TRUE)))
    })

    test_that("zi_poisson_lpmf matches dzipl for a single value", {
        expect_equal(zi_poisson_log_linked_lpmf(5, 7, 3.5, -1.5),
                     dzipll(x = 5, lambda = 7, gamma0 = 3.5, gamma1 = -1.5, log = TRUE))
    })

    # Negative Binomial

    # neg_binomial_2_lpmf is a Stan built-in

    test_that("neg_binomial_2_lpmf matches dneg_binomial_2 for multiple values", {
        x = sample.int(n = 20, size = 100, replace = TRUE)
        mu = runif(n = 100, min = 0.1, max = 20)
        phi = runif(n = 100, min = 0, max = 1)
        expect_equal(neg_binomial_2_lpmf_stub(x, mu, phi),
                     sum(dneg_binomial_2(x = x, mu = mu, phi = phi, log = TRUE)))
    })

    test_that("neg_binomial_2_lpmf matches dneg_binomial for a single value", {
        expect_equal(neg_binomial_2_lpmf_stub(5, 7, 0.2),
                     dneg_binomial_2(5, 7, 0.2, log = TRUE))
    })

    # Zero-inflated Negative Binomial

    test_that("zi_neg_binomial_2_lpmf is included in the stan model", {
      expect_true("zi_neg_binomial_2_lpmf" %in% function_names)
    })

    test_that("zi_neg_binomial_2_lpmf matches dzinb_2 for multiple values", {
        x = sample.int(n = 20, size = 100, replace = TRUE)
        mu = runif(n = 100, min = 0.1, max = 20)
        phi = runif(n = 100, min = 0.1, max = 5)
        pi = runif(n = 100, min = 0, max = 1)
        expect_equal(zi_neg_binomial_2_lpmf(x, mu, phi, pi),
                     sum(dzinb_2(x = x, mu = mu, phi = phi, pi = pi, log = TRUE)))
    })

    test_that("zi_neg_binomial_2_lpmf matches dzinb_2 for a single value", {
        expect_equal(zi_neg_binomial_2_lpmf(5, 7, 2.5, 0.2), dzinb_2(x = 5, mu = 7, phi = 2.5, pi = 0.2, log = TRUE))
    })

    # Zero-inflated Negative Binomial Linked

    test_that("zi_neg_binomial_linked_2_lpmf is included in the stan model", {
      expect_true("zi_neg_binomial_linked_2_lpmf" %in% function_names)
    })

    test_that("zi_neg_binomial_linked_2_lpmf matches dzinbl_2 for multiple values", {
        x = sample.int(n = 20, size = 100, replace = TRUE)
        mu = runif(n = 100, min = 0.1, max = 20)
        phi = runif(n = 100, min = 0.1, max = 5)
        gamma0 = runif(n = 100, min = -5, max = 5)
        gamma1 = runif(n = 100, min = -2, max = 2)
        expect_equal(zi_neg_binomial_linked_2_lpmf(x, mu, phi, gamma0, gamma1),
                     sum(dzinbl_2(x = x, mu = mu, phi = phi, gamma0 = gamma0, gamma1 = gamma1, log = TRUE)))
    })

    test_that("zi_neg_binomial_2_lpmf matches dzinb_2 for a single value", {
        expect_equal(zi_neg_binomial_linked_2_lpmf(5, 7, 2.5, 3.5, -1.5),
                     dzinbl_2(x = 5, mu = 7, phi = 2.5, gamma0 = 3.5, gamma1 = -1.5, log = TRUE))
    })

    # Zero-inflated Negative Binomial Log-linked

    test_that("zi_neg_binomial_log_linked_2_lpmf is included in the stan model", {
      expect_true("zi_neg_binomial_log_linked_2_lpmf" %in% function_names)
    })

    test_that("zi_neg_binomial_log_linked_2_lpmf matches dzinbl_2 for multiple values", {
        x = sample.int(n = 20, size = 100, replace = TRUE)
        mu = runif(n = 100, min = 0.1, max = 20)
        phi = runif(n = 100, min = 0.1, max = 5)
        gamma0 = runif(n = 100, min = -5, max = 5)
        gamma1 = runif(n = 100, min = -2, max = 2)
        expect_equal(zi_neg_binomial_log_linked_2_lpmf(x, mu, phi, gamma0, gamma1),
                     sum(dzinbll_2(x = x, mu = mu, phi = phi, gamma0 = gamma0, gamma1 = gamma1, log = TRUE)))
    })

    test_that("zi_neg_binomial_2_lpmf matches dzinb_2 for a single value", {
        expect_equal(zi_neg_binomial_log_linked_2_lpmf(5, 7, 2.5, 3.5, -1.5),
                     dzinbll_2(x = 5, mu = 7, phi = 2.5, gamma0 = 3.5, gamma1 = -1.5, log = TRUE))
    })
}
