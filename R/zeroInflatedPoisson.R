#' Zero-inflated Poisson Probability Mass Function
#'
#' @param x vector of (non-negative integer) values.
#' @param lambda vector of (non-negative) means.
#' @param pi vector of (real lying in \[0, 1\]) zero-inflation parameters.
#' @param log logical; if TRUE, probabilities, p, are given as log(p).
#'
#' @return The (log) probability mass at x, given lambda and pi.
#' @export
dzip <- function(x, lambda, pi, log = FALSE) {
  if (any(pi < 0 | pi > 1)) {
    stop(("argument pi contains values that lie outside the  interval [0, 1]"))
  }

  probabilityMass = pi * (x == 0) + (1 - pi) * stats::dpois(x, lambda)

  if (log) {
    return(log(probabilityMass))
  } else {
    return(probabilityMass)
  }
}

#' Zero-inflated Poisson Quantile Function
#'
#' @param p vector of quantiles.
#' @param lambda vector of (non-negative) means.
#' @param pi vector of (in \eqn{[0, 1]}) zero-inflation parameters.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X <= x]}, otherwise, \eqn{P[X > x]}.
#' @param log.p logical; if TRUE, probabilities p. This doesn't affect pi.
#'
#' @return A vector of quantiles, each of which coincide with the respective probability in p.
#' @export
qzip <- function(p, lambda, pi, lower.tail = TRUE, log.p = FALSE) {
  # TODO: Should log.p also affect pi?

  if (length(lower.tail) != 1) {
    stop("lower.tail should be length 1.")
  }

  if (length(log.p) != 1) {
    stop("log.p should be length 1.")
  }

  if (any(pi < 0 | pi > 1)) {
    stop("argument pi contains values that lie outside the  interval [0, 1]")
  }


  p_linear <- p
  if (log.p) {
    p_linear <- exp(p)
  }

  if (any(p_linear < 0 | p_linear > 1)) {
    stop("argument p contains values that represent probabilities outside the interval [0, 1]")
  }

  p_lower <-  p_linear
  if (! lower.tail) {
    p_lower <- 1 - p_linear
  }

  output_length <- max(length(p), length(lambda), length(pi))
  q <- numeric(output_length)
  for (i in 1:output_length) {
    p_lower_i <- p_lower[[(i - 1) %% length(p_lower) + 1]]
    pi_i <- pi[[(i - 1) %% length(pi) + 1]]
    lambda_i <- lambda[[(i - 1) %% length(lambda) + 1]]

    if (p_lower_i <= pi_i) {
      q[[i]] <- 0
    } else {
      q[[i]] <- stats::qpois((p_lower_i - pi_i) / (1.0 - pi_i), lambda_i, lower.tail = TRUE, log.p = FALSE)
    }
  }

  q
}

#' Zero-inflated Poisson Log Probability Mass Function Stan Code
#'
#' Stan code for the zero-inflated Poisson distribution's log probability mass function.
#'
#' @usage NULL
#' @format NULL
#' @export
#' @examples
#' cat(zi_poisson_lpmf_stan[["source_code"]])
zi_poisson_lpmf_stan <- get_stan_function("zi_poisson_lpmf")

