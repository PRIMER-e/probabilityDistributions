#' Negative Binomial Log Probability Mass Stan Code
#'
#' Stan has built-in functions for calculating the log probability density
#' of the negative binomial distribution:
#' \preformatted{
#' real neg_binomial_lpmf(ints n | reals alpha, reals beta)
#' real neg_binomial_2_lpmf(ints y | reals mu, reals phi)
#' real neg_binomial_2_log_lpmf(ints y | reals eta, reals phi)
#' }
#'
#' @return A text string containing Stan code.
#' @export
dnbinom_stan <- function() {
  "// Stan has built-in functions for calculating the log probability density
// of the negative binomial distribution:
// real neg_binomial_lpmf(ints n | reals alpha, reals beta)
// real neg_binomial_2_lpmf(ints y | reals mu, reals phi)
// real neg_binomial_2_log_lpmf(ints y | reals eta, reals phi)
"
}
