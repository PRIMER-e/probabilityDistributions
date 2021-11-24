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

dzip_stan <- function() {
"real zi_poisson_lpmf(int n, real lambda, real pi) {
  return log(pi * (n == 0) +
               (1 - pi) * exp(poisson_lpmf(n | lambda))
  );
}"
}
