% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zeroInflatedPoisson.R
\name{dzip}
\alias{dzip}
\title{Zero-inflated Poisson Probability Mass Function}
\usage{
dzip(x, lambda, pi, log = FALSE)
}
\arguments{
\item{x}{vector of (non-negative integer) values.}

\item{lambda}{vector of (non-negative) means.}

\item{pi}{vector of (real lying in [0, 1]) zero-inflation parameters.}

\item{log}{logical; if TRUE, probabilities, p, are given as log(p).}
}
\value{
The (log) probability mass at x, given lambda and pi.
}
\description{
Zero-inflated Poisson Probability Mass Function
}
