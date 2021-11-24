library(probabilityDistributions)

test_that("zip(x, lambda, 0) == dpois(x, lambda, 0)", {
  x = sample.int(100)
  lambda = runif(100, 0, 100)

  expect_equal(dzip(x, lambda, 0), dpois(x, lambda))
})

test_that("zip(0, lambda, 1) == 1", {
  lambda = runif(100, 0, 100)

  expect_equal(dzip(0, lambda, 1), rep(1, 100))
})


test_that("zip(x, lambda, 1) == 0 when x > 0", {
  x = sample.int(100)
  lambda = runif(100, 0, 100)

  expect_equal(dzip(x, lambda, 1), rep(0, 100))
})

test_that("zip(x, lambda, pi) == (1 - pi) * dpois(x, lambda)  when x > 0", {
  x = sample.int(100)
  lambda = runif(100, 0, 100)
  pi = runif(100, 0, 1)

  expect_equal(dzip(x, lambda, pi), (1 - pi) * dpois(x, lambda))
})

test_that("zip(0, lambda, pi) == pi + dpois(0, lambda)", {
  lambda = runif(100, 0, 100)
  pi = runif(100, 0, 1)

  expect_equal(dzip(0, lambda, pi), pi + (1 - pi) * dpois(0, lambda))
})

test_that("zip(x, lambda, pi, log = TRUE) == log((1 - pi) * dpois(x, lambda))  when x > 0", {
  x = sample.int(100)
  lambda = runif(100, 0, 100)
  pi = runif(100, 0, 1)

  expect_equal(dzip(x, lambda, pi, log = TRUE), log((1 - pi) * dpois(x, lambda)))
})
