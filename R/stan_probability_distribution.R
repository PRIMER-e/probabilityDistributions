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
