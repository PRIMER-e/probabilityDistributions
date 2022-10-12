#' compare arguments lengths, probably smarter way of doing this than combn
#' @param ... objects to compare lengths of
check_lengths <- function(...) {
  check_length <- function(pair) {
    a <- length(pair[[1]]); b <- length(pair[[2]])
    if (a > 1 & b > 1 & a != b) {
      stop("vector recycling is discouraged, please use the same length",
           " arguments, or combinations of  single values and equal length",
           " vectors (iteratively if combinations are required)",
           call. = FALSE)
    }
  }
  # this is cheeky to do both in same function
  args <- list(...)
  apply(utils::combn(args, 2), 2, check_length)
  max(sapply(args, length))
}
#' recycle length one vectors to max output length N
#' @param arg object to pad if necessary
#' @param N length to pad to
pad_arg <- function(arg, N) {
  if (length(arg) == 1) {
    rep_len(arg, N)
  } else {
    arg
  }
}
