#' Zero-inflated Poisson Probability Mass
#'
#' @param x vector of (non-negative integer) values.
#' @param lambda vector of (non-negative) means.
#' @param pi vector of (real lying in \[0, 1\]) zero-inflation parameters.
#' @param log logical; if TRUE, probabilities, p, are given as log(p).
#'
#' @return The (log) probability mass at x, given lambda and pi.
#' @export
dzip <- function(x, lambda, pi, log = FALSE) {
  if(any(pi < 0 | pi > 1)) {
    stop(glue::glue("argument pi contains values that lie outside the  interval [0, 1]"))
  }

  probabilityMass = pi * (x == 0) + (1 - pi) * stats::dpois(x, lambda)

  if (log) {
    return(log(probabilityMass))
  } else {
    return(probabilityMass)
  }
}

#' Zero-inflated Poisson Log Probability Mass Stan Code
#'
#' This function returns a string of Stan code that defines the function
#' \code{real zi_poisson_lmpf(int n, real lambda, real pi)}. When called
#' the Stan function will return the probability mass at n, for a
#' zero-inflated Poisson distribution with mean lambda and zero-inflation
#' parameter pi.
#'
#' In order to use it the code should be placed with the functions { ... }
#' block of a Stan model specification and then referenced within
#' model { ... } block.
#'
#' @return A string containing Stan code.
#' @export
dzip_stan <- function() {
"// The log probability mass at x, given lambda and pi.
// lambda is the (non-negative) mean of the poisson distribution.
// pi is the amount of zero-inflation, which should lie within the range [0, 1].
real zi_poisson_lpmf(int n, real lambda, real pi) {
  return log(pi * (n == 0) +
               (1 - pi) * exp(poisson_lpmf(n | lambda))
  );
}"
}
