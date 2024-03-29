% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tailAdjustedBeta.R
\name{beta_mu_to_shape}
\alias{beta_mu_to_shape}
\alias{beta_shape_to_mu}
\title{Convert Beta parameters between mean-dispersion and shape-shape}
\usage{
beta_mu_to_shape(mu, phi)

beta_shape_to_mu(shape1, shape2)
}
\arguments{
\item{mu}{mean/s (non-negative).}

\item{phi}{dispersion parameter.}

\item{shape1}{first shape parameter (a.k.a. alpha).}

\item{shape2}{second shape parameter (a.k.a. beta).

See \link[stats]{dbeta} for more details.}
}
\value{
a list with the converted Beta parameters
}
\description{
Convert Beta parameters between mean-dispersion and shape-shape
}
