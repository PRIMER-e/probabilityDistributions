test_that("qzinb_2 matches VGAM::qzinegbin", {
  p <- c(runif(200, 0, 1), 0, 1)
  # VGAM::qzinegbin requires that length(munb) == length(size)
  mu <- runif(300, 0, 20)
  phi <- runif(300, 0, 20)
  pi <- c(runif(700, 0, 1), 0, 1)

  actual <- qzinb_2(p, mu, phi, pi)
  expected <- VGAM::qzinegbin(p, size = phi, munb = mu, pstr0 = pi)
  expect_equal(actual, expected)
})
