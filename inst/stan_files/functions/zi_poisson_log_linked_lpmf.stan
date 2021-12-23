real zi_poisson_log_linked_lpmf(int n, real lambda, real gamma0, real gamma1) {

  real pi = inv_logit(gamma0 + gamma1 * log(lambda));

  // TODO: Would this be more efficient if it used log_sum_exp?
  return log(pi * (n == 0) + (1 - pi) * exp(poisson_lpmf(n | lambda)));
}
