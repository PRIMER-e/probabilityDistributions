test_that("dneg_binomial_2 matches dnbinom", {
  x <- sample.int(n = 100, size = 20, replace = TRUE)
  mu <- runif(n = 100, min = 5, max = 15)
  phi <- runif(n = 100, min = 0.1, max = 5)

  expect_equal(dneg_binomial_2(x = x, mu = mu, phi = phi),
               dnbinom(x = x, size = phi, mu = mu))
})

test_that("log dneg_binomial_2 matches log dnbinom", {
  x <- sample.int(n = 100, size = 20, replace = TRUE)
  mu <- runif(n = 100, min = 5, max = 15)
  phi <- runif(n = 100, min = 0.1, max = 5)

  expect_equal(dneg_binomial_2(x = x, mu = mu, phi = phi, log = TRUE),
               dnbinom(x = x, size = phi, mu = mu, log = TRUE))
})

test_that("qneg_binomial_2 matches qnbinom", {
  p <- runif(n = 100, min = 0, max = 1)
  mu <- runif(n = 100, min = 5, max = 15)
  phi <- runif(n = 100, min = 0.1, max = 5)

  expect_equal(qneg_binomial_2(p = p, mu = mu, phi = phi),
               qnbinom(p = p, size = phi, mu = mu))
})
