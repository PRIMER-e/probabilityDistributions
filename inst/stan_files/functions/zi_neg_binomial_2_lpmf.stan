real zi_neg_binomial_2_lpmf(int[] n, vector mu, vector phi, vector pi) {

  real probability_mass = 0;

  for (i in 1:size(n)) {
    if (pi[i] < 0 || pi[i] > 1) {
      reject("pi should be in [0, 1]; pi=", pi[i]);
    }

    // TODO: Would this be more efficient if it used log_sum_exp?
    probability_mass += log(pi[i] * (n[i] == 0) + (1 - pi[i]) * exp(neg_binomial_2_lpmf(n[i] | mu[i], phi[i])));
  }

  return probability_mass;
}
