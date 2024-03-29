% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zeroInflatedPoissonLogLinked.R
\name{dzipll}
\alias{dzipll}
\title{Zero-inflated Poisson Linked Probability Mass Function}
\usage{
dzipll(x, lambda, gamma0, gamma1, log = FALSE)
}
\arguments{
\item{x}{vector of (non-negative integer) values.}

\item{lambda}{vector of (non-negative) means.}

\item{gamma0}{The zero-inflation to mean link intercept.}

\item{gamma1}{The zero-inflation to mean link gradient.}

\item{log}{logical; if TRUE, probabilities, p, are given as log(p).}
}
\value{
The (log) probability mass at x, given lambda and pi.
}
\description{
Zero-inflated Poisson Linked Probability Mass Function
}
