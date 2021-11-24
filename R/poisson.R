
#' Stan code for the Poisson Log Probability Density
#'
#' Stan has a built-in function which calculates the log probability density
#' of the the Poisson distribution:
#' \code{  real poisson_lpmf(ints n | reals lambda)}
#'
#' @return A text string containing Stan code.
#' @export
dpois_stan <- function() {
  "// Stan has a built-in function which calculates the log probability density
// of  the Poisson distribution:
//   real poisson_lpmf(ints n | reals lambda)
"
}
