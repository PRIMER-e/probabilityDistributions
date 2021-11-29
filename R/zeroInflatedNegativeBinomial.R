#' Zero-inflated Negative Binomial Probability Mass
#'
#' @param x vector of (non-negative integer) values.
#' @param size target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution). Must be strictly positive, need not be integer.
#' @param prob probability of success in each trial. 0 < prob <= 1.
#' @param mu alternative parametrization via mean: see ‘Details’.
#' @param pi vector of (real lying in \[0, 1\]) zero-inflation parameters.
#' @param log logical; if TRUE, probabilities, p, are given as log(p).
#'
#' @return The (log) probability mass at x, given lambda and pi.
#' @export
dzinbinom <- function(x, size, prob, mu, pi, log = FALSE) {
  if(any(pi < 0 | pi > 1)) {
    stop(glue::glue("argument pi contains values that lie outside the  interval [0, 1]"))
  }

  probabilityMass = pi * (x == 0) +
                    (1 - pi) * stats::dnbinom(x = x,
                                              size = size,
                                              prob = prob,
                                              mu = mu)

  if (log) {
    return(log(probabilityMass))
  } else {
    return(probabilityMass)
  }
}
