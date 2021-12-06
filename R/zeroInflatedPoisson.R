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
    stop(glue::glue("argument pi contains values that lie outside the  interval [0, 1]"))
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

  p_linear <- ifelse(rep(log.p, length(p)),
              exp(p),
              p)

  if (any(p_linear < 0 | p_linear > 1)) {
    stop("argument p contains values that represent probabilities outside the interval [0, 1]")
  }

  p_lower <- ifelse(rep(lower.tail, length(p)),
              p_linear,
              1 - p_linear)

  q <- ifelse(p_lower < pi, 0, stats::qpois(p_lower - pi, lambda, lower.tail = TRUE, log.p = FALSE))

  q
}

#' Zero-inflated Poisson Log Probability Mass Function Stan Code
#'
#' @return A string containing Stan source-code
#' @export
#'
#' @examples
#' cat(zi_poisson_lpmf_stan[["source_code"]])
zi_poisson_lpmf_stan <- get_stan_function("zi_poisson_lpmf")

