real zi_poisson_linked_lpmf(int[] n, vector lambda, vector gamma0, vector gamma1) {

  real probability_mass = 0;

  for (i in 1:size(n)) {
    real pi = logit(gamma0 + gamma1 * lambda[i])

    // TODO: Would this be more efficient if it used log_sum_exp?
    probability_mass += log(pi * (n[i] == 0) + (1 - pi) * exp(poisson_lpmf(n[i] | lambda[i])));
  }

  return probability_mass;
}
