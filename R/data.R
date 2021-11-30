#' Available Stan Probability Distributions
#'
#' A data.frame listing all the Stan probability distribution functions
#' available within this package.
#'
#' The source-code for a function can be loaded using the
#' `stan_probability_distribution` function
#'
#' @format A data frame with 1 row and 5 variables:
#' \describe{
#'   \item{function_name}{Name of the function}
#'   \item{builtin}{Whether the function is built-in to Stan}
#'   \item{distibution}{Which distribution the function relates to}
#'   \item{function_type}{Type of function, e.g. PDF or Quantile}
#'   \item{log}{Whether the function returns log values}
#' }
#'
#' @examples
#' available_stan_probability_distributions
"available_stan_probability_distributions"
