// The log probability mass at x, given lambda and pi.
// lambda is the (non-negative) mean of the poisson distribution.
// pi is the amount of zero-inflation, which should lie within the range [0, 1].
real zi_poisson_lpmf(int n, real lambda, real pi) {

    if (pi < 0 || pi > 1) {
      reject("pi should be in [0, 1]; pi=", pi);
    }

    // TODO: Would this be more efficient if it used log_sum_exp?
    return log(pi * (n == 0) + (1 - pi) * exp(poisson_lpmf(n | lambda)));
}
