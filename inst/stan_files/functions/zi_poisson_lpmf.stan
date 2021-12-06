// The log probability mass at x, given lambda and pi.
// lambda is the (non-negative) mean of the poisson distribution.
// pi is the amount of zero-inflation, which should lie within the range [0, 1].
real zi_poisson_lpmf(int[] n, vector lambda, vector pi) {

  real probability_mass = 0;

  for (i in 1:size(n)) {
    if (pi[i] < 0 || pi[i] > 1) {
      reject("pi should be in [0, 1]; pi=", pi[i]);
    }

    // TODO: Would this be more efficient if it used log_sum_exp?
    probability_mass += log(pi[i] * (n[i] == 0) + (1 - pi[i]) * exp(poisson_lpmf(n[i] | lambda[i])));
  }

  return probability_mass;
}
