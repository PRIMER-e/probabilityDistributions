real zi_neg_binomial_log_linked_2_lpmf(int n, real mu, real phi, real gamma0, real gamma1) {

  real pi = inv_logit(gamma0 + gamma1 * log(mu));

  // TODO: Would this be more efficient if it used log_sum_exp?
  return log(pi * (n == 0) + (1 - pi) * exp(neg_binomial_2_lpmf(n | mu, phi)));
}
