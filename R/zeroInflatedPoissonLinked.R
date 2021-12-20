#' Zero-inflated Poisson Linked Probability Mass Function
#'
#' @param x vector of (non-negative integer) values.
#' @param lambda vector of (non-negative) means.
#' @param gamma0 The zero-inflation to mean link intercept.
#' @param gamma1 The zero-inflation to mean link gradient.
#' @param log logical; if TRUE, probabilities, p, are given as log(p).
#'
#' @return The (log) probability mass at x, given lambda and pi.
#' @export
dzipl <- function(x, lambda, gamma0, gamma1, log = FALSE) {
  pi <- stats::plogis(gamma0 + gamma1 * lambda)

  dzip(x, lambda, pi, log = log)
}

#' Zero-inflated Poisson Linked Quantile Function
#'
#' @param p vector of quantiles.
#' @param lambda vector of (non-negative) means.
#' @param gamma0 The zero-inflation to mean link intercept.
#' @param gamma1 The zero-inflation to mean link gradient.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X <= x]}, otherwise, \eqn{P[X > x]}.
#' @param log.p logical; if TRUE, probabilities p. This doesn't affect pi.
#'
#' @return A vector of quantiles, each of which coincide with the respective probability in p.
#' @export
qzipl <- function(p, lambda, gamma0, gamma1, lower.tail = TRUE, log.p = FALSE) {
  pi <- stats::plogis(gamma0 + gamma1 * lambda)

  qzip(p, lambda, pi, lower.tail = lower.tail, log.p = log.p)
}

#' Zero-inflated Poisson Linked Log Probability Mass Function Stan Code
#'
#' Stan code for the zero-inflated Poisson distribution's log probability mass function,
#' where the amount of zero-inflation is a function of the mean.
#'
#' @usage NULL
#' @format NULL
#' @export
#'
#' @examples
#' cat(zi_poisson_linked_lpmf_stan[["source_code"]])
zi_poisson_linked_lpmf_stan <- get_stan_function("zi_poisson_linked_lpmf")

