#' Tail-adjusted Beta Probability Density Function
#'
#' @param x vector of (non-negative) values.
#' @param mu mean/s in (0, 1) for the beta distribution.
#' @param phi dispersion parameter (positive) for the beta distribution.
#' @param delta rounding parameter in (0, 0.5) for the tail adjustment.
#' @param log logical; if TRUE, probabilities, p, are given as log(p).
#'
#' @return The (log) probability mass at x, given lambda and pi.
#' @export
dtab <- function(x, mu, phi, delta, log = FALSE) {
  if (any(delta <= 0 | delta >= 0.5)) {
    stop("delta should be in (0, 0.5)")
  }
  # check lengths and pad if nec for vectors later
  output_length <- check_lengths(x, mu, phi, delta)
  ss <- beta_mu_to_shape(mu, phi)
  shape1 <- ss$shape1
  shape2 <- ss$shape2
  x <- pad_arg(x, output_length)
  shape1 <- pad_arg(shape1, output_length)
  shape2 <- pad_arg(shape2, output_length)
  delta <- pad_arg(delta, output_length)

  p_dens <- stats::dbeta(x,
                         shape1,
                         shape2,
                         log = log)
  xltd <- x < delta
  if (any(xltd)) {
    p_dens[xltd] <- stats::pbeta(delta[xltd],
                                      shape1[xltd],
                                      shape2[xltd],
                                      log.p = log)
    if (log) {
      p_dens[xltd] <- p_dens[xltd] - log(delta[xltd])
    } else {
      p_dens[xltd] <- p_dens[xltd] / delta[xltd]
    }
  }
  xgt1md <- x > (1 - delta)
  if (any(xgt1md)) {
    p_dens[xgt1md] <- stats::pbeta(1 - delta[xgt1md],
                                         shape1[xgt1md],
                                         shape2[xgt1md],
                                         lower.tail = FALSE,
                                         log.p = log)
    if (log) {
      p_dens[xgt1md] <- p_dens[xgt1md] - log(delta[xgt1md])
    } else {
      p_dens[xgt1md] <- p_dens[xgt1md] / delta[xgt1md]
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

#' Tail-adjusted Beta Quantile Function
#'
#' @param p vector of quantiles.
#' @param mu mean/s in (0, 1) for the beta distribution.
#' @param phi dispersion parameter (positive) for the beta distribution.
#' @param delta rounding parameter in (0, 0.5) for the tail adjustment.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X <=
#'   x]}, otherwise, \eqn{P[X > x]}.
#' @param log.p logical; if TRUE, probabilities p. This doesn't affect pi.
#'
#' @return A vector of quantiles, each of which coincide with the respective
#'   probability in p.
#' @export
qtab <- function(p, mu, phi, delta, lower.tail = TRUE, log.p = FALSE) {
  if (length(lower.tail) != 1) {
    stop("lower.tail should be length 1.")
  }
  if (length(log.p) != 1) {
    stop("log.p should be length 1.")
  }
  if (any(delta <= 0 | delta >= 0.5)) {
    stop("delta should be in (0, 0.5)")
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
  output_length <- check_lengths(p, mu, phi, delta)
  ss <- beta_mu_to_shape(mu, phi)
  shape1 <- ss$shape1
  shape2 <- ss$shape2
  p_lower <- pad_arg(p_lower, output_length)
  shape1 <- pad_arg(shape1, output_length)
  shape2 <- pad_arg(shape2, output_length)
  delta <- pad_arg(delta, output_length)

  # calc bulk q's first then adjust tails
  q <- stats::qbeta(p_lower,
                    shape1,
                    shape2,
                    lower.tail = TRUE,
                    log.p = FALSE)

  # prob less than delta or 1 minus delta
  Fd <- stats::pbeta(delta, shape1, shape2)
  F1md <- stats::pbeta(1 - delta, shape1, shape2)

  # piecewise indices solved for p
  lt_i <- p_lower < Fd
  ut_i <- p_lower > F1md

  if (any(lt_i)) {
    q[lt_i] <- p_lower[lt_i] * delta[lt_i] / Fd[lt_i]
  }
  if (any(ut_i)) {
    q[ut_i] <- (p_lower[ut_i] - 1) * delta[ut_i] /
      (1 - F1md[ut_i]) + 1
  }
  q
}

#' Convert Beta parameters between mean-dispersion and shape-shape
#'
#' @param mu mean/s (non-negative).
#' @param phi dispersion parameter.
#' @param shape1 first shape parameter (a.k.a. alpha).
#' @param shape2 second shape parameter (a.k.a. beta).
#'
#' See \link[stats]{dbeta} for more details.
#'
#' @return a list with the converted Beta parameters
#' @export
beta_mu_to_shape <- function(mu, phi) {
  check_lengths(mu, phi)
  if (any(mu <= 0 | mu >= 1)) {
    stop("mu should be in (0, 1)")
  }
  if (any(phi <= 0 | !is.finite(phi))) {
    stop("phi should finite and strictly positive")
  }
  alpha <- mu / phi
  beta <- (1 - mu) / phi
  list(shape1 = alpha, shape2 = beta)
}

#' @rdname beta_mu_to_shape
#' @export
beta_shape_to_mu <- function(shape1, shape2) {
  check_lengths(shape1, shape2)
  phi <- 1 / (shape1 + shape2)
  mu <- shape1 / (shape1 + shape2)
  list(mu = mu, phi = phi)
}
