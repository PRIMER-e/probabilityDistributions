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

#' @export
dzip_stan <- function() {
"real zi_neg_binomial_2_lpmf(int y, real mu, real phi, real omega) {
  return log(omega * (y == 0) +
               (1 - omega) * exp(neg_binomial_2_lpmf(y | mu, phi))
  );
}"
}
