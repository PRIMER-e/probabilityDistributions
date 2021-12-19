#' Zero-inflated Negative Binomial Linked Probability Mass
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
dzinbl_2 <- function(x, mu, phi, gamma0, gamma1, log = FALSE) {

  pi = stats::qlogis(gamma0 + gamma1 * mu)

  dzinb_2(x, mu, phi, pi, log = log)
}

#' Zero-inflated Negative Linked Binomial Quantile Function
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
qzinbl_2 <- function(p, mu, phi, gamma0, gamma1, lower.tail = TRUE, log.p = FALSE) {
  pi = stats::qlogis(gamma0 + gamma1 * mu)

  qzinb_2(p, mu, phi, pi, lower.tail = lower.tail, log.p = log.p)
}

#' Zero-inflated Negative-binomial Linked Log Probability Mass Function Stan Code
#'
#' @return A string containing Stan source-code
#' @export
#'
#' @examples
#' cat(zi_neg_binomial_2_linked_lpmf_stan[["source_code"]])
zi_neg_binomial_2_linked_lpmf_stan <- get_stan_function("zi_neg_binomial_2_linked_lpmf")
