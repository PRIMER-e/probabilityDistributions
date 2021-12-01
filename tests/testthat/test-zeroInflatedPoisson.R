library(probabilityDistributions)

test_that("dzip(x, lambda, 0) == dpois(x, lambda)", {
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

test_that("qzip(x, lambda, 0) == qpois(x, lambda)", {
  p <- runif(n = 100, min = 0, max = 1)
  pi <- rep(0, 100)
  lambda <- runif(100, 0, 100)
  log.p <- FALSE
  lower.tail <- TRUE


  expect_equal(qzip(p, lambda, pi, lower.tail = lower.tail, log.p = log.p),
               qpois(p, lambda, lower.tail = lower.tail, log.p = log.p))
})

test_that("qzip(x, lambda, 0, log.p = TRUE) == qpois(x, lambda, log.p = TRUE)", {
  p <- log(runif(n = 100, min = 0, max = 1))
  pi <- rep(0, 100)
  lambda <- runif(100, 0, 100)
  log.p <- TRUE
  lower.tail <- TRUE

  expect_equal(qzip(p, lambda, pi, lower.tail = lower.tail, log.p = log.p),
               qpois(p, lambda, lower.tail = lower.tail, log.p = log.p))
})

test_that("qzip(x, lambda, 0, lower.tail = FALSE) == qpois(x, lambda, lower.tail = FALSE)", {
  p <- runif(n = 100, min = 0, max = 1)
  pi <- rep(0, 100)
  lambda <- runif(100, 0, 100)
  log.p <- FALSE
  lower.tail <- FALSE

  expect_equal(qzip(p, lambda, pi, lower.tail = lower.tail, log.p = log.p),
               qpois(p, lambda, lower.tail = lower.tail, log.p = log.p))
})

test_that("qzip(x, lambda, 0, lower.tail = FALSE, log.p = TRUE) == qpois(x, lambda, lower.tail = FALSE, log.p = TRUE)", {
  p <- log(runif(n = 100, min = 0, max = 1))
  pi <- rep(0, 100)
  lambda <- runif(100, 0, 100)
  lower.tail <- FALSE
  log.p <- TRUE

  expect_equal(qzip(p, lambda, pi, lower.tail = lower.tail, log.p = log.p),
               qpois(p, lambda, lower.tail = lower.tail, log.p = log.p))
})

test_that("qzip(0.5, 7, 0.2) is correct", {
  p <- 0.5
  lambda <- 7
  pi <- 0.2

  expected <- 5

  expect_equal(qzip(p, lambda, pi), expected)
  expect_equal(qzip(1 - p, lambda, pi, lower.tail = FALSE), expected)
  expect_equal(qzip(log(p), lambda, pi, log.p = TRUE), expected)
  expect_equal(qzip(log(1 - p), lambda, pi, lower.tail = FALSE, log.p = TRUE), expected)

})

test_that("qzip(0.1, 7, 0.2) is correct", {
  p <- 0.1
  lambda <- 7
  pi <- 0.2

  expected <- 0

  expect_equal(qzip(p, lambda, pi), expected)
  expect_equal(qzip(1 - p, lambda, pi, lower.tail = FALSE), expected)
  expect_equal(qzip(log(p), lambda, pi, log.p = TRUE), expected)
  expect_equal(qzip(log(1 - p), lambda, pi, lower.tail = FALSE, log.p = TRUE), expected)

})
