## code to prepare `available_stan_probability_distributions` dataset goes here

# TODO: Automatically generate this by parsing Doxygen commented .stan files.
available_stan_probability_distributions <- data.frame(distibution = character(0),
                                                       function_type = character(0),
                                                       log = logical(0),
                                                       builtin = logical(0),
                                                       function_name = character(0))

available_stan_probability_distributions[nrow(available_stan_probability_distributions) + 1, ] <-
  list(distribution = "zero-inflated Poisson",
       function_type = "PMF",
       log = TRUE,
       builtin = FALSE,
       function_name = "zi_poisson_lpmf")


usethis::use_data(available_stan_probability_distributions, overwrite = TRUE)
