dzip <- function(x, lambda, pi, log = FALSE) {
  if(pi < 0 || pi > 1) {
    stop(glue::glue("argument pi = {pi} is not in the interval [0, 1]"))
  }

  probabilityMass = pi * (x == 0) + (1 - pi) * dpois(x, lambda)

  if (log) {
    return(log(probabilityMass))
  } else {
    return(probabilityMass)
  }
}
