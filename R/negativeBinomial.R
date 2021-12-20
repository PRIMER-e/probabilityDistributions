#' Negative Binomial Log Probability Mass Function Stan Code
#'
#' @description There are two negative binomial density functions built into
#' Stan:
#' \preformatted{
#' real neg_binomial_lpmf(ints n | reals alpha, reals beta)
#' real neg_binomial_2_lpmf(ints y | reals mu, reals phi)
#' }
#'
#' The second one uses the mean and over-dispersion parameterisation familiar
#' to ecologists.
#'
#' See \url{https://mc-stan.org/docs/2_19/functions-reference/negative-binomial-distribution.html}
#' and \url{https://mc-stan.org/docs/2_19/functions-reference/nbalt.html} for
#' more details.
#'
#' @usage NULL
#' @format NULL
#' @export
#' @examples
#' cat(neg_binomial_2_lpmf_stan[["source_code"]])
neg_binomial_2_lpmf_stan <- list(source_code =
"// There are two negative binomial density functions built into Stan:
//
// real neg_binomial_lpmf(ints n | reals alpha, reals beta)
// real neg_binomial_2_lpmf(ints y | reals mu, reals phi)
//
// The second one uses the mean and over-dispersion parameterisation familiar
// to ecologists.
//
// See <https://mc-stan.org/docs/2_19/functions-reference/negative-binomial-distribution.html>
// and <https://mc-stan.org/docs/2_19/functions-reference/nbalt.html> for
// more details.")


#' The Negative Binomial Probability Mass Function (alternative parameterisation)
#'
#' @param x vector of (non-negative integer) quantiles.
#' @param mu mean.
#' @param phi dispersion.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#'
#' @return the (log) probability mass at x, given mu and phi.
#' @export
dneg_binomial_2 <- function(x, mu, phi, log = FALSE) {
  if (length(log) != 1) {
    stop("log should be length 1.")
  }

  stats::dnbinom(x = x, mu = mu, size = phi, log = log)
}

#' The Negative Binomial Quantile Function (alternative parameterisation)
#'
#' @param p vector of probabilities.
#' @param mu mean.
#' @param phi dispersion.
#' @param lower.tail logical; if TRUE (default), probablities are \eqn{P[X <= x]}, otherwise, \eqn{[X > x]}.
#' @param log.p logical; if TRUE, probabilities are given as log(p).
#'
#' @return A vector of quantiles, each of which correspond to a probability in p.
#' @export
qneg_binomial_2 <- function(p, mu, phi, lower.tail = TRUE, log.p = FALSE) {
  if (length(lower.tail) != 1) {
    stop("lower.tail should be length 1.")
  }

  if (length(log.p) != 1) {
    stop("log.p should be length 1.")
  }

  stats::qnbinom(p = p,
          size = phi,
          mu = mu,
          lower.tail = lower.tail,
          log.p = log.p)
}
