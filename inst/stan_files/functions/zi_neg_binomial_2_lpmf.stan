real zi_neg_binomial_2_lpmf(int n, real mu, real phi, real pi) {

  if (pi < 0 || pi > 1) {
    reject("pi should be in [0, 1]; pi=", pi);
  }

    // TODO: Would this be more efficient if it used log_sum_exp?
   return log(pi * (n == 0) + (1 - pi) * exp(neg_binomial_2_lpmf(n | mu, phi)));
}
