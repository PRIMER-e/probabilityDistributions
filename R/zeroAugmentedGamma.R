#' Zero-augmented Gamma Probability Density Function
#'
#' @param x vector of (non-negative) values.
#' @param mu mean/s (non-negative) for the gamma distribution.
#' @param phi dispersion parameter for the gamma distribution.
#' @param pi vector of (real lying in \[0, 1\]) zero-inflation parameters.
#' @param log logical; if TRUE, probabilities, p, are given as log(p).
#'
#' @return The (log) probability mass at x, given mu, phi, and pi.
#' @export
dzag <- function(x, mu, phi, pi, log = FALSE) {
  if (any(mu <= 0) | any(phi <= 0)) {
    stop("mu and phi should both be > 0")
  }
  if (any(pi < 0 | pi > 1)) {
    stop("argument pi contains values that lie outside the  interval [0, 1]")
  }
  if (any(!is.finite(phi))) {
    stop('argument phi contains undefined values')
  }
  # check lengths and pad if neeed for vectors later
  output_length <- check_lengths(x, mu, phi, pi)
  sr <- gamma_mu_to_rate(mu, phi)
  shape <- sr$shape
  rate <- sr$rate
  x <- pad_arg(x, output_length)
  shape <- pad_arg(shape, output_length)
  rate <- pad_arg(rate, output_length)
  pi <- pad_arg(pi, output_length)

  # note dgamma sometimes defined at x = 0, can that be mixed in this dist?
  # shape > 1 => 0
  # shape = 1 => rate
  # shape < 1 => Inf
  p_dens <- numeric(output_length)
  if (log) {
    # doing r log on pdens created precision and 1 / 0 issues & test fails
    # so use dgamma log instead
    p_dens[x == 0] <- log(pi[x == 0])
    p_dens[x > 0] <- log((1 - pi[x > 0])) + stats::dgamma(x[x > 0],
                                                          shape[x > 0],
                                                          rate[x > 0],
                                                          log = TRUE)
  } else {
    p_dens[x == 0] <- pi[x == 0]
    p_dens[x > 0] <- (1 - pi[x > 0]) * stats::dgamma(x[x > 0],
                                                     shape[x > 0],
                                                     rate[x > 0])
  }
  p_dens
}

#' Zero-augmented Gamma Quantile Function
#'
#' @param p vector of quantiles.
#' @param mu mean/s (non-negative) for the gamma distribution.
#' @param phi dispersion parameter for the gamma distribution.
#' @param pi vector of (in \eqn{[0, 1]}) zero-inflation parameters.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X <=
#'   x]}, otherwise, \eqn{P[X > x]}.
#' @param log.p logical; if TRUE, probabilities p. This doesn't affect pi.
#'
#' @return A vector of quantiles, each of which coincide with the respective
#'   probability in p.
#' @export
qzag <- function(p, mu, phi, pi, lower.tail = TRUE, log.p = FALSE) {

  if (length(lower.tail) != 1) {
    stop("lower.tail should be length 1.")
  }
  if (length(log.p) != 1) {
    stop("log.p should be length 1.")
  }
  if (any(pi < 0 | pi > 1)) {
    stop("argument pi contains values that lie outside the  interval [0, 1]")
  }
  if ((length(mu) > 1 & length(pi) > 1 & length(mu) != length(pi)) |
      (length(mu) > 1 & length(p) > 1 & length(mu) != length(p)) |
      (length(pi) > 1 & length(p) > 1 & length(pi) != length(p))) {
    stop("vector recycling is discouraged, please use either the same length",
         " args or a vector for one single values for the others",
         " (iteratively if combinations are required)")
  }
  if (any(mu <= 0) | any(phi <= 0)) {
    stop("mu and phi should both be > 0")
  }

  p_linear <- p
  if (log.p) {
    p_linear <- exp(p)
  }

  if (any(p_linear < 0 | p_linear > 1)) {
    stop("argument p contains values that represent probabilities outside the interval [0, 1]")
  };

  p_lower <-  p_linear
  if (!lower.tail) {
    p_lower <- 1 - p_linear
  }

  # shape = alpha, rate = beta in stan (and wikipedia)
  sr <- gamma_mu_to_rate(mu, phi)

  output_length <- max(length(p), length(mu), length(pi))
  q <- numeric(output_length)

  for (i in 1:output_length) {
    p_lower_i <- p_lower[[(i - 1) %% length(p_lower) + 1]]
    pi_i <- pi[[(i - 1) %% length(pi) + 1]]
    rate_i <- sr$rate[[(i - 1) %% length(sr$rate) + 1]]
    shape_i <- sr$shape[[(i - 1) %% length(sr$shape) + 1]]

    if (p_lower_i <= pi_i) {
      q[[i]] <- 0
    } else {
      q[[i]] <- stats::qgamma(p = (p_lower_i - pi_i) / (1.0 - pi_i),
                              shape = shape_i,
                              rate = rate_i,
                              lower.tail = TRUE,
                              log.p = FALSE)
    }
  }
  q
}

#' Convert Gamma parameters between mean-dispersion and shape-rate
#'
#' @param mu mean/s (non-negative).
#' @param phi dispersion parameter.
#' @param shape shape parameter.
#' @param rate rate/s (or inverse scale/s).
#'
#' See \link[stats]{dgamma} for more details on shape and rate.
#'
#' @return a list with the converted Gamma parameters
#' @export
gamma_mu_to_rate <- function(mu, phi) {
  if (length(mu) > 1 & length(phi) > 1 & length(mu) != length(phi)) {
    stop("vector recycling is discouraged, please use either the same length",
         " mu & phi, or a single value for one and a vector for the other",
         " (iteratively if a combination is required)")
  }
  shape <- 1 / phi
  rate <- shape / mu
  list(shape = shape, rate = rate)
}

#' @rdname gamma_mu_to_rate
#' @export
gamma_rate_to_mu <- function(shape, rate) {
  if (length(shape) > 1 & length(rate) > 1 & length(shape) != length(rate)) {
    stop("vector recycling is discouraged, please use either the same length",
         " shape & rate, or a single value for one and a vector for the other",
         " (iteratively if a combination is required)")
  }
  phi <- 1 / shape
  mu <- shape / rate
  list(mu = mu, phi = phi)
}
