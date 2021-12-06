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

#' Poisson Probability Mass Function
#'
#' This is available in the `stats` package.
#' See \link[stats]{dnbinom} for more details
#' @name dnbinom
NULL

#' Poisson Quantile Function
#'
#' This is available in the `stats` package.
#' See \link[stats]{qnbinom} for more details.
#' @name qnbinom
NULL
