#' Zero-inflated Negative Binomial Probability Mass
#'
#' @param x vector of (non-negative integer) values.
#' @param size target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution). Must be strictly positive, need not be integer.
#' @param prob probability of success in each trial. 0 < prob <= 1.
#' @param mu alternative parametrization via mean: see ‘Details’.
#' @param pi vector of (real lying in \[0, 1\]) zero-inflation parameters.
#' @param log logical; if TRUE, probabilities, p, are given as log(p).
#'
#' @return The (log) probability mass at x, given lambda and pi.
#' @export
dzinb <- function(x, size, prob, mu, pi, log = FALSE) {
  if(any(pi < 0 | pi > 1)) {
    stop(glue::glue("argument pi contains values that lie outside the  interval [0, 1]"))
  }

  probabilityMass = pi * (x == 0) +
                    (1 - pi) * stats::dnbinom(x = x,
                                              size = size,
                                              prob = prob,
                                              mu = mu)

  if (log) {
    return(log(probabilityMass))
  } else {
    return(probabilityMass)
  }
}

#' Zero-inflated Negative Binomial Quantile Function
#'
#' @param p vector of quantiles.
#' @param size target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution). Must be strictly positive, need not be integer.
#' @param prob probability of success in each trial. 0 < prob <= 1.
#' @param mu alternative parametrization via mean: see ‘Details’.
#' @param pi vector of (in \eqn{[0, 1]}) zero-inflation parameters.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X <= x]}, otherwise, \eqn{P[X > x]}.
#' @param log.p logical; if TRUE, probabilities p. This doesn't affect pi.
#'
#' @return A vector of quantiles, each of which coincide with the respective probability in p.
#' @export
qzinb <- function(p, size, prob, mu, pi, lower.tail = TRUE, log.p = FALSE) {
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

  q <- ifelse(p_lower < pi, 0, stats::qnbinom(p_lower - pi, size, prob, mu, lower.tail = TRUE, log.p = FALSE))

  q
}

#' Zero-inflated Negative-binomial Log Probability Mass Function Stan Code
#'
#' @return A string containing Stan source-code
#' @export
#'
#' @examples
#' cat(zi_neg_binomial_2_lpmf_stan[["source_code"]])
zi_neg_binomial_2_lpmf_stan <- get_stan_function("zi_neg_binomial_2_lpmf")
