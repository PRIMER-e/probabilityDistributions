% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zeroAugmentedGamma.R
\name{qzag}
\alias{qzag}
\title{Zero-augmented Gamma Quantile Function}
\usage{
qzag(p, mu, phi, pi, lower.tail = TRUE, log.p = FALSE)
}
\arguments{
\item{p}{vector of quantiles.}

\item{mu}{mean/s (non-negative) for the gamma distribution.}

\item{phi}{dispersion parameter for the gamma distribution.}

\item{pi}{vector of (in \eqn{[0, 1]}) zero-inflation parameters.}

\item{lower.tail}{logical; if TRUE (default), probabilities are \eqn{P[X <=
x]}, otherwise, \eqn{P[X > x]}.}

\item{log.p}{logical; if TRUE, probabilities p. This doesn't affect pi.}
}
\value{
A vector of quantiles, each of which coincide with the respective
probability in p.
}
\description{
Zero-augmented Gamma Quantile Function
}
