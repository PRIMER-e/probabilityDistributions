#' Zero-inflated Tail-adjusted Beta Probability Density Function
#'
#' @param x vector of (non-negative) values.
#' @param mu mean/s in (0, 1) for the beta distribution.
#' @param phi dispersion parameter (positive) for the beta distribution.
#' @param delta rounding parameter in (0, 0.5) for the tail adjustment.
#' @param pi vector of (real lying in \[0, 1\]) zero-inflation parameters.
#' @param log logical; if TRUE, probabilities, p, are given as log(p).
#'
#' @return The (log) probability mass at x, given mu, phi, delta and pi.
#' @export
dzitab <- function(x, mu, phi, delta, pi, log = FALSE) {
  if (length(log) != 1) {
    stop("log should be length 1.")
  }
  if(any(pi < 0 | pi > 1)) {
    stop(("argument pi contains values that lie outside the  interval [0, 1]"))
  }
  if (any(delta <= 0 | delta >= 0.5)) {
    stop("delta should be in (0, 0.5)")
  }
  output_length <- check_lengths(x, mu, phi, delta, pi)
  x <- pad_arg(x, output_length)
  mu <- pad_arg(mu, output_length)
  phi <- pad_arg(phi, output_length)
  delta <- pad_arg(delta, output_length)
  pi <- pad_arg(pi, output_length)

  if (log) {
    p_dens <- log(1 - pi) + dtab(x, mu, phi, delta, log = TRUE)
  } else {
    p_dens <- (1 - pi) * dtab(x, mu, phi, delta, log = FALSE)
  }

  xltd <- x < delta
  # this only works for non-zero zi, otherwise log(exp(large -ve)) -> -Inf
  if (any(xltd) & any(pi > 0)) {
    if (log) {
      p_dens[xltd] <- log(pi[xltd] / delta[xltd] + exp(p_dens[xltd]))
    } else {
      p_dens[xltd] <- pi[xltd] / delta[xltd] + p_dens[xltd]
    }
  }
  if (any(x < 0 | x > 1)) {
    if (log) {
      p_dens[x < 0 | x > 1] <- -Inf
    } else {
      p_dens[x < 0 | x > 1] <- 0
    }
  }
  p_dens
}

#' Zero-inflated Tail-adjusted Beta Quantile Function
#'
#' @param p vector of quantiles.
#' @param mu mean/s in (0, 1) for the beta distribution.
#' @param phi dispersion parameter (positive) for the beta distribution.
#' @param delta rounding parameter in (0, 0.5) for the tail adjustment.
#' @param pi vector of (in \eqn{[0, 1]}) zero-inflation parameters.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X <=
#'   x]}, otherwise, \eqn{P[X > x]}.
#' @param log.p logical; if TRUE, probabilities p. This doesn't affect pi.
#'
#' @return A vector of quantiles, each of which coincide with the respective
#'   probability in p.
#' @export
qzitab <- function(p, mu, phi, delta, pi, lower.tail = TRUE, log.p = FALSE) {
  if (length(lower.tail) != 1) {
    stop("lower.tail should be length 1.")
  }
  if (length(log.p) != 1) {
    stop("log.p should be length 1.")
  }
  if (any(delta <= 0 | delta >= 0.5)) {
    stop("delta should be in (0, 0.5)")
  }
  if (any(pi < 0 | pi > 1)) {
    stop("argument pi contains values that lie outside the  interval [0, 1]")
  }
  p_linear <- p
  if (log.p) {
    p_linear <- exp(p)
  }
  if (any(p_linear < 0 | p_linear > 1)) {
    stop("p contains values that represent probabilities outside of [0, 1]")
  }
  p_lower <- p_linear
  if (!lower.tail) {
    p_lower <- 1 - p_linear
  }

  # ensure equal length for vec later
  output_length <- check_lengths(p, mu, phi, delta, pi)
  ss <- beta_mu_to_shape(mu, phi)
  shape1 <- ss$shape1
  shape2 <- ss$shape2
  p_lower <- pad_arg(p_lower, output_length)
  shape1 <- pad_arg(shape1, output_length)
  shape2 <- pad_arg(shape2, output_length)
  delta <- pad_arg(delta, output_length)
  pi <- pad_arg(pi, output_length)
  q <- numeric(output_length)

  # prob less than delta or 1 minus delta
  Fd <- stats::pbeta(delta, shape1, shape2)
  F1md <- stats::pbeta(1 - delta, shape1, shape2)

  # piecewise bounds and indices solved for p
  lt_b <- pi + (1 - pi) * Fd
  lt_i <- p_lower < lt_b
  ut_b <- (1 - pi) * (1 - F1md)
  ut_i <- p_lower > 1 - ut_b
  md_i <- !(lt_i | ut_i)

  if (any(lt_i)) {
    q[lt_i] <- p_lower[lt_i] * delta[lt_i] / lt_b[lt_i]
  }
  if (any(md_i)) {
    q[md_i] <- stats::qbeta((p_lower[md_i] - pi[md_i]) / (1 - pi[md_i]),
                            shape1[md_i],
                            shape2[md_i],
                            lower.tail = TRUE,
                            log.p = FALSE)
  }
  if (any(ut_i)) {
    q[ut_i] <- (p_lower[ut_i] - 1) * delta[ut_i] / ut_b[ut_i] + 1
  }
  q
}
