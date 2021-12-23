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
        for (i in 1:100) {
          expect_equal(zi_poisson_lpmf(x[[i]], lambda[[i]], pi[[i]]),
                       dzip(x = x[[i]], lambda = lambda[[i]], pi = pi[[i]], log = TRUE))

        }
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
        for (i in 1:100) {
          expect_equal(zi_poisson_linked_lpmf(x[[i]], lambda[[i]], gamma0[[i]], gamma1[[i]]),
                       dzipl(x = x[[i]], lambda = lambda[[i]], gamma0 = gamma0[[i]], gamma1 = gamma1[[i]], log = TRUE))

        }
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
        for (i in 1:100) {
          expect_equal(zi_poisson_log_linked_lpmf(x[[i]], lambda[[i]], gamma0[[i]], gamma1[[i]]),
                       dzipll(x = x[[i]], lambda = lambda[[i]], gamma0 = gamma0[[i]], gamma1 = gamma1[[i]], log = TRUE))

        }
    })

    # Negative Binomial

    # neg_binomial_2_lpmf is a Stan built-in

    test_that("neg_binomial_2_lpmf matches dneg_binomial_2 for multiple values", {
        x = sample.int(n = 20, size = 100, replace = TRUE)
        mu = runif(n = 100, min = 0.1, max = 20)
        phi = runif(n = 100, min = 0, max = 1)
        for (i in 1:100) {
          expect_equal(neg_binomial_2_lpmf_stub(x[[i]], mu[[i]], phi[[i]]),
                       dneg_binomial_2(x = x[[i]], mu = mu[[i]], phi = phi[[i]], log = TRUE))

        }
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
        for (i in 1:100) {
          expect_equal(zi_neg_binomial_2_lpmf(x[[i]], mu[[i]], phi[[i]], pi[[i]]),
                       dzinb_2(x = x[[i]], mu = mu[[i]], phi = phi[[i]], pi = pi[[i]], log = TRUE))

        }
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
        for (i in 1:100) {
          expect_equal(zi_neg_binomial_linked_2_lpmf(x[[i]], mu[[i]], phi[[i]], gamma0[[i]], gamma1[[i]]),
                       dzinbl_2(x = x[[i]], mu = mu[[i]], phi = phi[[i]], gamma0 = gamma0[[i]], gamma1 = gamma1[[i]], log = TRUE))

        }
    })

    # Zero-inflated Negative Binomial Log-linked

    test_that("zi_neg_binomial_log_linked_2_lpmf is included in the stan model", {
      expect_true("zi_neg_binomial_log_linked_2_lpmf" %in% function_names)
    })

    test_that("zi_neg_binomial_log_linked_2_lpmf matches dzinbl_2", {
        x = sample.int(n = 20, size = 100, replace = TRUE)
        mu = runif(n = 100, min = 0.1, max = 20)
        phi = runif(n = 100, min = 0.1, max = 5)
        gamma0 = runif(n = 100, min = -5, max = 5)
        gamma1 = runif(n = 100, min = -2, max = 2)

        for (i in 1:100) {
          expect_equal(zi_neg_binomial_log_linked_2_lpmf(x[[i]], mu[[i]], phi[[i]], gamma0[[i]], gamma1[[i]]),
                       dzinbll_2(x = x[[i]], mu = mu[[i]], phi = phi[[i]], gamma0 = gamma0[[i]], gamma1 = gamma1[[i]], log = TRUE))

        }
    })
}
