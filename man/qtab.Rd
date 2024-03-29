% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tailAdjustedBeta.R
\name{qtab}
\alias{qtab}
\title{Tail-adjusted Beta Quantile Function}
\usage{
qtab(p, mu, phi, delta, lower.tail = TRUE, log.p = FALSE)
}
\arguments{
\item{p}{vector of quantiles.}

\item{mu}{mean/s in (0, 1) for the beta distribution.}

\item{phi}{dispersion parameter (positive) for the beta distribution.}

\item{delta}{rounding parameter in (0, 0.5) for the tail adjustment.}

\item{lower.tail}{logical; if TRUE (default), probabilities are \eqn{P[X <=
x]}, otherwise, \eqn{P[X > x]}.}

\item{log.p}{logical; if TRUE, probabilities p. This doesn't affect pi.}
}
\value{
A vector of quantiles, each of which coincide with the respective
probability in p.
}
\description{
Tail-adjusted Beta Quantile Function
}
