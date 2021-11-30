available_stan_probability_distributions_data <- data.frame(distibution = character(0),
                                                       function_type = character(0),
                                                       function_name = character(0))

available_stan_probability_distributions_data[nrow(available_stan_probability_distributions_data) + 1, ] <-
  list(distribution = "zero-inflated Poisson",
       function_type = "PMF",
       function_name = "zi_poisson_lpmf")


#' Available Stan Function Definitions
#'
#' @return A dataframe containing all available Stan function defintions.
#' @export
#'
#' @examples
#' available_stan_probability_distributions()
available_stan_probability_distributions <- function() {
  available_stan_probability_distributions_data
}

#' Find a Stan Function Definition
#'
#' @param function_name Name of the function.
#'
#' @return A string containing the Stan function definition.
#' @export
#'
#' @examples
#' x <- stan_probability_distribution("zi_poisson_lpmf")
#' cat(x)
stan_probability_distribution <- function(function_name) {
  filename = paste0(function_name, ".stan")

  full_filename <- system.file("stan_files",
                      "functions",
                      filename,
                      package = getPackageName())

  if (full_filename == "") {
    stop(paste0("Definition for ", function_name, " could not be found."))
  }

  readr::read_file(full_filename)
}
