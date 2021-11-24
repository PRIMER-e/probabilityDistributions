
#' Stan code for the Poisson Log Probability Density
#'
#' Stan already has built-in support for the Poisson distribution via the
#' poisson_lpmf function.
#' Therefore, this function just returns some comments directing people
#' to use that.
#'
#' @return A text string containing Stan code.
#' @export
dpois_stan <- function() {
  "// The Poisson log probability density is built-in to Stan.
// It's available via the function poisson_lpmf( n | lambda).
"
}
