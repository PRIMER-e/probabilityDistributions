# re-parameterising functions ----
test_that("gamma_mu_to_rate and gamma_rate_to_mu agree", {
  mu <- runif(1000, 0, 100)
  phi <- rexp(1, 0.5)
  sr <- gamma_mu_to_rate(mu, phi)
  mp <- do.call(gamma_rate_to_mu, sr)
  expect_equal(list(mu = mu, phi = phi), mp)
  # mean of gamma shape rate parameterisation is shape / rate
  expect_equal(mu, sr$shape / sr$rate)

  rate <- 1 / runif(100, 0, 100)
  shape <- 1 / rexp(1, 0.5)
  mp <- gamma_rate_to_mu(shape, rate)
  sr <- do.call(gamma_mu_to_rate, mp)
  expect_equal(list(shape = shape, rate = rate), sr)
  expect_equal(mp$mu, shape / rate)

  expect_error(gamma_mu_to_rate(1:10, 1:3))
  expect_error(gamma_rate_to_mu(1:10, 1:3))
})

# density function ----
test_that("dzag checks correct inputs", {
  expect_error(dzag(0, 1:10, 1:2, 1))
  expect_error(dzag(0, 1:10, 1, c(0, 1)))
  expect_error(dzag(1, -10, 1, 0.5))
  expect_error(dzag(1, 1, -1, 0.5))
  expect_error(dzag(1, 1, 1, -0.5))
  expect_error(dzag(1, 1, 1, 1.5))
})
test_that("dzag with pi = 0 matches dgamma for x > 0", {
  x <- seq(1, 100, length.out = 1000)
  mu <- runif(1000, 0, 100)
  phi <- rexp(1, 0.5)
  sr <- gamma_mu_to_rate(mu, phi)
  expect_equal(dzag(x, mu, phi, 0), dgamma(x, sr$shape, sr$rate))
  expect_equal(dzag(x, mu, phi, 0, log = TRUE),
               dgamma(x, sr$shape, sr$rate, log = TRUE))
})
test_that("dzag == 1 when pi = 1 & x = 0", {
  mu <- runif(1000, 0, 100)
  phi <- rexp(1, 0.5)
  expect_equal(dzag(0, mu, phi, pi = 1), rep(1, 1000))
  expect_equal(dzag(0, mu, phi, pi = 1, log = TRUE), rep(log(1), 1000))
})
test_that("dzag == 0 when pi = 1 & x > 0", {
  x <- seq(0, 100, length.out = 1000)
  mu <- runif(1000, 0, 100)
  phi <- rexp(1, 0.5)
  dens <- dzag(x, mu, phi, pi = 1)
  expect_equal(dens[x == 0], rep(1, sum(x == 0)))
  expect_equal(dens[x > 0], rep(0, sum(x > 0)))
  dens <- dzag(x, mu, phi, pi = 1, log = TRUE)
  expect_equal(dens[x == 0], rep(log(1), sum(x == 0)))
  expect_equal(dens[x > 0], rep(log(0), sum(x > 0)))
})
test_that("dzag == (1 - pi) * dgamma when x > 0", {
  x <- seq(0.00001, 30, length.out = 1000)
  x <- seq(1, 100, length.out = 1000)
  mu <- runif(1000, 0, 100)
  phi <- rexp(1, 0.5)
  sr <- sr <- gamma_mu_to_rate(mu, phi)
  pi <- runif(1000, 0, 1)
  expect_equal(dzag(x, mu, phi, pi), (1 - pi) * dgamma(x, sr$shape, sr$rate))
  expect_equal(dzag(x, mu, phi, pi, log = TRUE),
               log((1 - pi) * dgamma(x, sr$shape, sr$rate)))
})
# test_that("our dzag matches gamlss::dzaga", {
#   # assuming they're correct
#   # would this break cran if gamlss deps not in dependencies?
#   dGA <- gamlss.dist::dGA
#   dZAGA <- gamlss.inf::Zadj.d('GA')
#
#   x <- c(rep(0, 200), seq(0.00001, 30, length.out = 800))
#   mu <- runif(1000, 0, 100)
#   phi <- rexp(1, 0.5)
#   sr <- gamma_mu_to_rate(mu, phi)
#   pi <- runif(1000, 0, 1)
#   expect_equal(dzag(x, mu, phi, pi),
#                dZAGA(x, mu, sqrt(phi), pi))
#   expect_equal(dzag(x, mu, phi, pi, log = TRUE),
#                dZAGA(x, mu, sqrt(phi), pi, log = TRUE))
# })

# quantile function ----
test_that("qzag checks correct inputs", {
  expect_error(qzag(0, 1:10, 1:2, 1))
  expect_error(qzag(0, 1:10, 1, c(0, 1)))
  expect_error(qzag(1, -10, 1, 0.5))
  expect_error(qzag(1, 1, -1, 0.5))
  expect_error(qzag(1, 1, 1, -0.5))
  expect_error(qzag(1, 1, 1, 1.5))
  expect_error(qzag(1.5, 1, 1, 0.5))
  expect_error(qzag(-0.5, 1, 1, 0.5))
  expect_error(qzag(log(1.5), 1, 1, 0.5, log.p = TRUE))
})
test_that("qzag with pi = 0 matches qgamma", {
  p <- seq(0, 1, length.out = 1000)
  mu <- runif(1000, 0, 100)
  phi <- rexp(1, 0.5)
  sr <- gamma_mu_to_rate(mu, phi)
  expect_equal(qzag(p, mu, phi, 0), qgamma(p, sr$shape, sr$rate))
  expect_equal(qzag(p, mu, phi, 0, lower.tail = FALSE),
               qgamma(p, sr$shape, sr$rate, lower.tail = FALSE))
  expect_equal(qzag(log(p), mu, phi, 0, log.p = TRUE),
               qgamma(log(p), sr$shape, sr$rate, log.p = TRUE))
  expect_equal(qzag(log(p), mu, phi, 0, log.p = TRUE, lower.tail = FALSE),
               qgamma(log(p), sr$shape, sr$rate, log.p = TRUE,
                      lower.tail = FALSE))
})
# test_that("our qzag matches gamlss::qzaga", {
#   qGA <- gamlss.dist::qGA
#   qZAGA <- gamlss.inf::Zadj.q('GA')
#
#   N <- 1000
#   p <- seq(0, 1, length.out = N)
#   mu <- runif(N, 0, 100)
#   phi <- rexp(1, 0.5)
#   sr <- gamma_mu_to_rate(mu, phi)
#   pi <- runif(N, 0, 1)
#   expect_equal(qzag(p, mu, phi, pi), qZAGA(p, mu, sqrt(phi), pi))
#   # see note below
#   # expect_equal(qzag(p, mu, phi, pi, lower.tail = FALSE),
#   #              qZAGA(p, mu, sqrt(phi), pi, lower.tail = FALSE))
#   # expect_equal(qzag(log(p), mu, phi, pi, log.p = TRUE),
#   #              qZAGA(log(p), mu, sqrt(phi), pi, log.p = TRUE))
#   # expect_equal(qzag(log(p), mu, phi, pi, log.p = TRUE, lower.tail = FALSE),
#   #              qZAGA(log(p), mu, sqrt(phi), pi, log.p = TRUE,
#   #                    lower.tail = FALSE))
# })

# Note on gamlss::qZAGA, their lower tail & log.p implementations may be wrong?
# they calculate the quantile with lower tail = TRUE, then invert the quantile
# as opposed to us inverting the probability then calculating quantile
# i.e. we both do p_gamma = (p_zag - pi) / (1 - pi) for inv cdf but then
# gamlss does q <- qgamma(p_gamma, shape, rate, lower.tail = TRUE, log.p = F)
# and if (!lower.tail) q <- 1 - q (assuming symmetry of quantile function?)
# as opposed to us doing if (!lower.tail) p_zag_lower <- 1 - p_zag first then
# deriving p_gamma_lower and then
# calc q <- qgamma(p_gamma_lower, shape, rate, lower.tail = TRUE, log.p = F)
# similar for log.p, they do log on the quantile, not probability?
# local({
#   op <- par(mfrow = c(1, 2))
#   N <- 1000
#   to <- 30
#   mu <- 5
#   phi <- 0.5
#   p <- c(0.01, 0.05, 0.1)
#   pi <- 0.25
#   curve(dzag(x, mu, phi, pi), 0, to, N)
#   lapply(p, \(pp) abline(v = qzag(pp, mu, phi, pi, lower.tail = FALSE),
#                          col = 2, lty = 2))
#   abline(v = qzag(log(0.5), mu, phi, pi, log.p = TRUE), col = 4, lty = 4)
#   curve(dZAGA(x, mu, sqrt(phi), pi), 0, to, N)
#   lapply(p, \(pp) abline(v = qZAGA(pp, mu, sqrt(phi), pi, lower.tail = FALSE),
#                          col = 2, lty = 2))
#   abline(v = qZAGA(log(0.5), mu, sqrt(phi), pi, log.p = TRUE), col = 4, lty = 4)
# })
