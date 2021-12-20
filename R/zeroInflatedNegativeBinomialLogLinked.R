#' Zero-inflated Negative Binomial Log-linked Probability Mass
#'
#' @param x vector of (non-negative integer) values.
#' @param mu mean of the distribution.
#' @param phi dispersion parameter.
#' @param gamma0 The zero-inflation to mean link intercept.
#' @param gamma1 The zero-inflation to mean link gradient.
#' @param log logical; if TRUE, probabilities, p, are given as log(p).
#'
#' @return The (log) probability mass at x, given lambda and pi.
#' @export
dzinbll_2 <- function(x, mu, phi, gamma0, gamma1, log = FALSE) {

  pi = stats::plogis(gamma0 + gamma1 * log(mu))

  dzinb_2(x, mu, phi, pi, log = log)
}

#' Zero-inflated Negative Log-linked Binomial Quantile Function
#'
#' @param p vector of quantiles.
#' @param mu mean of the distribution.
#' @param phi dispersion parameter.
#' @param gamma0 The zero-inflation to mean link intercept.
#' @param gamma1 The zero-inflation to mean link gradient.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X <= x]}, otherwise, \eqn{P[X > x]}.
#' @param log.p logical; if TRUE, probabilities p. This doesn't affect pi.
#'
#' @return A vector of quantiles, each of which coincide with the respective probability in p.
#' @export
qzinbll_2 <- function(p, mu, phi, gamma0, gamma1, lower.tail = TRUE, log.p = FALSE) {
  pi = stats::plogis(gamma0 + gamma1 * log(mu))

  qzinb_2(p, mu, phi, pi, lower.tail = lower.tail, log.p = log.p)
}

#' Zero-inflated Negative-binomial Log-linked Log Probability Mass Function Stan Code
#'
#' Stan code for the zero-inflated negative-binomial log probability mass function,
#' where the amount of zero-inflation is a function of log(mean).
#'
#' @usage NULL
#' @format NULL
#' @export
#'
#' @examples
#' cat(zi_neg_binomial_log_linked_2_lpmf_stan[["source_code"]])
zi_neg_binomial_log_linked_2_lpmf_stan <- get_stan_function("zi_neg_binomial_log_linked_2_lpmf")
