# re-parameterising functions ----
test_that("beta_mu_to_shape and beta_shape_to_mu agree", {
  mu <- runif(1000, 0.001, 0.999)
  phi <- rexp(1, 0.5)
  ss <- beta_mu_to_shape(mu, phi)
  mp <- do.call(beta_shape_to_mu, ss)
  # where is the precision lost? for really small shapes?
  expect_true(all(abs(phi - mp$phi) < 10 * .Machine$double.eps))
  expect_true(all(abs(mu - mp$mu) < 10 * .Machine$double.eps))
  # mean of beta shape shape parameterisation
  expect_equal(mu, ss$shape1 / (ss$shape1 + ss$shape2))

  shape1 <- rexp(100, 0.5)
  shape2 <- rexp(100, 0.5)
  mp <- beta_shape_to_mu(shape1, shape2)
  ss <- do.call(beta_mu_to_shape, mp)
  expect_equal(list(shape1 = shape1, shape2 = shape2), ss)
  expect_equal(mp$mu, shape1 / (shape1 + shape2))

  expect_error(beta_mu_to_shape(1:10, 1:3))
  expect_error(beta_shape_to_mu(1:10, 1:3))
})
test_that("tab functions check correct inputs", {
  mus <- 2:9 / 10
  expect_error(dtab(x = 0,   mu = 1:10, phi = 1,   delta = 0.25))
  expect_error(dtab(x = 0,   mu = mus,  phi = 1:2, delta = 0.25))
  expect_error(dtab(x = 0:2, mu = mus,  phi = 1,   delta = 0.25))
  expect_silent(dtab(x = mus, mu = mus,  phi = 1,   delta = 0.25))
  expect_error(dtab(x = mus, mu = mus,  phi = 1,   delta = 0.5))
  expect_error(dtab(x = 1,   mu = mus,  phi = 1,   delta = 0.5))
  expect_error(dtab(x = 1,   mu = mus,  phi = -1,  delta = 0.25))
  expect_error(dtab(x = 1,   mu = mus,  phi = 1,   delta = 0.25,
                    log = c(TRUE, FALSE)))

  expect_error(qtab(p = 0,   mu = 1:10, phi = 1,   delta = 0.25))
  expect_error(qtab(p = 0,   mu = mus,  phi = 1:2, delta = 0.25))
  expect_error(qtab(p = 0:2, mu = mus,  phi = 1,   delta = 0.25))
  expect_error(qtab(p = 1,   mu = mus,  phi = 1,   delta = 0.5))
  expect_error(qtab(p = 1,   mu = mus,  phi = -1,  delta = 0.25))
  expect_error(qtab(p = 1.5, mu = mus,  phi = 1,   delta = 0.25))
  expect_error(qtab(p = -.5, mu = mus,  phi = 1,   delta = 0.25))
  expect_error(qtab(p = 1,   mu = mus,  phi = 1,   delta = 0.25,
                    lower.tail = c(TRUE, FALSE),
                    log = TRUE))
  expect_error(qtab(p = 1,   mu = mus, phi = 1,   delta = 0.25,
                    lower.tail = FALSE,
                    log = c(TRUE, FALSE)))

  expect_error(dzitab(x = 0,   mu = 1:10, phi = 1,   delta = 0.25, pi = 1))
  expect_error(dzitab(x = 0,   mu = mus,  phi = 1:2, delta = 0.25, pi = 1))
  expect_error(dzitab(x = 0:2, mu = mus,  phi = 1,   delta = 0.25, pi = 1))
  expect_error(dzitab(x = 1,   mu = mus,  phi = 1,   delta = 0.5,  pi = 1))
  expect_error(dzitab(x = 1,   mu = mus,  phi = -1,  delta = 0.25, pi = 1))
  expect_error(dzitab(x = 1,   mu = mus,  phi = 1,   delta = 0.25, pi = 1,
                      log = c(TRUE, FALSE)))
  expect_error(dzitab(x = 1, mu = mus, phi = 1, delta = 0.2, pi = -1))
  expect_error(dzitab(x = 1, mu = mus, phi = 1, delta = 0.2, pi = 1.5))
  expect_error(dzitab(x = 1, mu = mus, phi = 1, delta = 0.2, pi = c(0.5, 0.75)))
  expect_silent(dzitab(x = 1,   mu = mus,  phi = 1,   delta = 0.25, pi = mus))

  expect_error(qzitab(p = 0,   mu = 1:10, phi = 1,   delta = 0.25, pi = 1))
  expect_error(qzitab(p = 0,   mu = mus,  phi = 1:2, delta = 0.25, pi = 1))
  expect_error(qzitab(p = 0:2, mu = mus,  phi = 1,   delta = 0.25, pi = 1))
  expect_error(qzitab(p = 1,   mu = mus,  phi = 1,   delta = 0.5,  pi = 1))
  expect_error(qzitab(p = 1,   mu = mus,  phi = -1,  delta = 0.25, pi = 1))
  expect_error(qzitab(p = 1.5, mu = mus,  phi = 1,   delta = 0.25, pi = 1))
  expect_error(qzitab(p = -.5, mu = mus,  phi = 1,   delta = 0.25, pi = 1))
  expect_error(qzitab(p = 1,   mu = mus,  phi = 1,   delta = 0.25, pi = 1,
                      lower.tail = c(TRUE, FALSE),
                      log = TRUE))
  expect_error(qzitab(p = 1,   mu = mus, phi = 1,   delta = 0.25, pi = 1,
                      lower.tail = FALSE,
                      log = c(TRUE, FALSE)))

  expect_error(qzitab(p = 1, mu = mus, phi = 1, delta = 0.2, pi = -1))
  expect_error(qzitab(p = 1, mu = mus, phi = 1, delta = 0.2, pi = 1.5))
  expect_error(qzitab(p = 1, mu = mus, phi = 1, delta = 0.2, pi = c(0.5, 0.75)))
  expect_silent(qzitab(p = 1, mu = mus,  phi = 1,   delta = 0.25, pi = mus))
})
test_that("tab & zitab(p = 0) approximate beta as delta -> 0", {
  x <- seq(0.0001, 0.9999, length.out = 1000)
  mu <- runif(1000, 0.0001, 0.9999)
  phi <- rexp(1, 0.5)
  ss <- beta_mu_to_shape(mu, phi)
  delta <- .Machine$double.eps
  expect_equal(dtab(x, mu, phi, delta), dbeta(x, ss$shape1, ss$shape2))
  expect_equal(dtab(x, mu, phi, delta, log = TRUE),
               dbeta(x, ss$shape1, ss$shape2, log = TRUE))

  expect_equal(dzitab(x, mu, phi, delta, pi = 0), dbeta(x, ss$shape1, ss$shape2))
  expect_equal(dzitab(x, mu, phi, delta, pi = 0, log = TRUE),
               dbeta(x, ss$shape1, ss$shape2, log = TRUE))

  expect_equal(qtab(x, mu, phi, delta), qbeta(x, ss$shape1, ss$shape2))
  expect_equal(qzitab(x, mu, phi, delta, pi = 0), qbeta(x, ss$shape1, ss$shape2))
  expect_equal(qtab(x, mu, phi, delta, lower.tail = FALSE),
               qbeta(x, ss$shape1, ss$shape2, lower.tail = FALSE))
  expect_equal(qzitab(x, mu, phi, delta, pi = 0, lower.tail = FALSE),
               qbeta(x, ss$shape1, ss$shape2, lower.tail = FALSE))
  expect_equal(qtab(log(x), mu, phi, delta, log.p = TRUE),
               qbeta(log(x), ss$shape1, ss$shape2, log.p = TRUE))
  expect_equal(qzitab(log(x), mu, phi, delta, pi = 0, log.p = TRUE),
               qbeta(log(x), ss$shape1, ss$shape2, log.p = TRUE))
  expect_equal(qtab(log(x), mu, phi, delta, lower.tail = FALSE, log.p = TRUE),
               qbeta(log(x), ss$shape1, ss$shape2, lower.tail = FALSE, log.p = TRUE))
  expect_equal(qzitab(log(x), mu, phi, delta, pi = 0, lower.tail = FALSE, log.p = TRUE),
               qbeta(log(x), ss$shape1, ss$shape2, lower.tail = FALSE, log.p = TRUE))
})
test_that("tab == zitab(p = 0)", {
  x <- seq(0, 1, length.out = 1000)
  mu <- runif(1000, 0.0001, 0.9999)
  phi <- 1 - mu
  delta <- .Machine$double.eps
  expect_equal(dtab(x, mu, phi, delta), dzitab(x, mu, phi, delta, pi = 0))
  expect_equal(dtab(x, mu, phi, delta, log = TRUE),
               dzitab(x, mu, phi, delta, pi = 0, log = TRUE))

  expect_equal(qtab(x, mu, phi, delta), qzitab(x, mu, phi, delta, pi = 0))
  expect_equal(qtab(x, mu, phi, delta, lower.tail = FALSE),
               qzitab(x, mu, phi, delta, pi = 0, lower.tail = FALSE))
  expect_equal(qtab(log(x), mu, phi, delta, lower.tail = TRUE, log.p = TRUE),
               qzitab(log(x), mu, phi, delta, pi = 0, lower.tail = TRUE, log.p = TRUE))
  expect_equal(qtab(log(x), mu, phi, delta, lower.tail = FALSE, log.p = TRUE),
               qzitab(log(x), mu, phi, delta, pi = 0, lower.tail = FALSE, log.p = TRUE))
})

if (FALSE) {
  # plot checks
  # library(dplyr)
  # library(tidyr)
  # library(ggplot2)
  # theme_set(bayesplot::theme_default())
  # tibble(mu = 0.3,
  #        phi = 0.1,
  #        # delta = .Machine$double.eps,
  #        # pi = 0) %>%
  #        delta = 0.2,
  #        pi = 0.5) %>%
  #   mutate(shapes = list(beta_mu_to_shape(mu, phi))) %>%
  #   unnest_wider(shapes) %>%
  #   mutate(x = list(seq(0, 1, length.out = 100))) %>%
  #   unnest(x) %>%
  #   mutate(fy_tab = dtab(x, mu, phi, delta),
  #          fy_zitab = dzitab(x, mu, phi, delta, pi),
  #          fy_beta = dbeta(x, shape1, shape2),
  #          Fy_tab = cumsum(fy_tab) / 100,
  #          Fy_zitab = cumsum(fy_zitab) / 100,
  #          Fy_beta = pbeta(x, shape1, shape2),
  #          Qy_tab = qtab(x, mu, phi, delta),
  #          Qy_zitab = qzitab(x, mu, phi, delta, pi),
  #          Qy_beta = qbeta(x, shape1, shape2)) %>%
  #   pivot_longer(matches('fy|Fy|Qy')) %>%
  #   separate(name, c('name', 'dist'), sep = '_') %>%
  #   ggplot(aes(x, value, colour = dist)) +
  #   geom_line() +
  #   facet_wrap(~ name, scales = 'free_y')
}
