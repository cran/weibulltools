% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence_intervals.R
\encoding{UTF-8}
\name{delta_method}
\alias{delta_method}
\title{Delta Method for Parametric Lifetime Distributions}
\usage{
delta_method(
  p,
  dist_params,
  dist_varcov,
  distribution = c("weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "weibull3", "lognormal3", "loglogistic3"),
  direction = c("y", "x")
)
}
\arguments{
\item{p}{A numeric vector of probabilities or quantiles. If the standard errors
of quantiles should be determined the corresponding probabilities have to be
specified, and if the standard errors of standardized quantiles (z-values)
should be computed corresponding quantiles are required.}

\item{dist_params}{A (named) numeric vector of (log-)location-scale parameters
returned from \code{\link{ml_estimation}}.}

\item{dist_varcov}{A (named) numeric matrix of estimated variances and
covariances returned from \code{\link{ml_estimation}}.}

\item{distribution}{Supposed distribution of the random variable. Has to be in
line with the specification made in \code{\link{ml_estimation}}.}

\item{direction}{A character string specifying for which quantity the standard
errors are calculated. One of \code{"y"} (if \code{p} are quantiles) or
\code{"x"} (if \code{p} are probabilities).}
}
\value{
A numeric vector of estimated standard errors for quantiles or
  standardized quantiles (\emph{z-values}).
}
\description{
This function applies the delta method for two- or three-parametric lifetime
distributions that belong to the (log-)location-scale family.
}
\details{
The delta method estimates the standard errors for quantities that can be
written as non-linear functions of ML estimators. Hence, (log-)location-scale
parameters as well as the variance-covariance matrix of these have to be estimated
with \link[=ml_estimation]{maximum likelihood}.

The estimated standard errors are needed to calculate Fisher's (normal
approximation) confidence intervals. For confidence bounds on the probability,
standard errors of the standardized quantiles (\code{direction = "y"})
have to be computed (\emph{z-procedure}) and for bounds on quantiles, standard errors
of quantiles (\code{direction = "x"}) are required. For more information see
\code{\link{confint_fisher}}.
}
\examples{
# Reliability data preparation:
data <- reliability_data(
  shock,
  x = distance,
  status = status
)

# Parameter estimation using maximum likelihood:
mle <- ml_estimation(
  data,
  distribution = "weibull",
  conf_level = 0.95
)

# Example 1 - Standard errors of standardized quantiles:
delta_y <- delta_method(
  p = shock$distance,
  dist_params = mle$coefficients,
  dist_varcov = mle$varcov,
  distribution = "weibull",
  direction = "y"
)

# Example 2 - Standard errors of quantiles:
delta_x <- delta_method(
  p = seq(0.01, 0.99, 0.01),
  dist_params = mle$coefficients,
  dist_varcov = mle$varcov,
  distribution = "weibull",
  direction = "x"
)

}
\references{
Meeker, William Q; Escobar, Luis A., Statistical methods for
  reliability data, New York: Wiley series in probability and statistics, 1998
}
