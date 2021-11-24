library(probabilityDistributions)

test_that("dzip(x, lambda, 0) == dpois(x, lambda, 0)", {
  x = sample.int(100)
  lambda = runif(100, 0, 100)

  expect_equal(dzip(x, lambda, 0), dpois(x, lambda))
})

test_that("dzip(0, lambda, 1) == 1", {
  lambda = runif(100, 0, 100)

  expect_equal(dzip(0, lambda, 1), rep(1, 100))
})


test_that("dzip(x, lambda, 1) == 0 when x > 0", {
  x = sample.int(100)
  lambda = runif(100, 0, 100)

  expect_equal(dzip(x, lambda, 1), rep(0, 100))
})

test_that("dzip(x, lambda, pi) == (1 - pi) * dpois(x, lambda)  when x > 0", {
  x = sample.int(100)
  lambda = runif(100, 0, 100)
  pi = runif(100, 0, 1)

  expect_equal(dzip(x, lambda, pi), (1 - pi) * dpois(x, lambda))
})

test_that("dzip(0, lambda, pi) == pi + dpois(0, lambda)", {
  lambda = runif(100, 0, 100)
  pi = runif(100, 0, 1)

  expect_equal(dzip(0, lambda, pi), pi + (1 - pi) * dpois(0, lambda))
})

test_that("dzip(x, lambda, pi, log = TRUE) == log((1 - pi) * dpois(x, lambda))  when x > 0", {
  x = sample.int(100)
  lambda = runif(100, 0, 100)
  pi = runif(100, 0, 1)

  expect_equal(dzip(x, lambda, pi, log = TRUE), log((1 - pi) * dpois(x, lambda)))
})

test_that("dzip(x, lambda, 1.5) fails", {
  expect_error(dzip(5, 7, 1.5))
})

if (requireNamespace("rstan", quietly = TRUE)) {
    stan_model_code <- paste0("functions { ", dzip_stan(), " }")

    model_listing <- rstan::stanc(model_code = stan_model_code)

    function_names <- rstan::expose_stan_functions(model_listing)

    test_that("dzip_stan produces valid Stan code", {
      expect_equal(function_names[[1]], "zi_poisson_lpmf")
    })

    test_that("dzip_stan matches dzip", {
      for (i in 1:100) {
        x = sample.int(100, 1)
        lambda = runif(1, 0, 100)
        pi = runif(1, 0, 1)
        expect_equal(zi_poisson_lpmf(x, lambda, pi), dzip(x, lambda, pi, log = TRUE))
      }
    })
}
