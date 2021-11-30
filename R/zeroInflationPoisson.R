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
