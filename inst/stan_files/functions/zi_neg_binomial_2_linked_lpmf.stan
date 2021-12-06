real zi_neg_binomial_2_linked_lpmf(int[] n, vector mu, vector phi, vector gamma0, vector gamma1) {

  real probability_mass = 0;

  for (i in 1:size(n)) {
    real pi = logit(gamma0[i] + gamma1[i] * lambda[i])

    // TODO: Would this be more efficient if it used log_sum_exp?
    probability_mass += log(pi * (n[i] == 0) + (1 - pi) * exp(neg_binomial_2_lpmf(n[i] | mu[i], phi[i])));
  }

  return probability_mass;
}
