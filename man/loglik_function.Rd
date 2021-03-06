% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml_estimation.R
\encoding{UTF-8}
\name{loglik_function}
\alias{loglik_function}
\title{Log-Likelihood Function for Parametric Lifetime Distributions}
\usage{
loglik_function(
  x,
  status,
  wts = rep(1, length(x)),
  dist_params,
  distribution = c("weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "weibull3", "lognormal3", "loglogistic3")
)
}
\arguments{
\item{x}{A numeric vector which consists of lifetime data. Lifetime
data could be every characteristic influencing the reliability of a product,
e.g. operating time (days/months in service), mileage (km, miles), load
cycles.}

\item{status}{A vector of binary data (0 or 1) indicating whether unit \emph{i}
is a right censored observation (= 0) or a failure (= 1).}

\item{wts}{Optional vector of case weights. The length of \code{wts} must be the
same as the number of observations in \code{x}.}

\item{dist_params}{A (named) numeric vector of (log-)location-scale parameters
in the order of location (\eqn{\mu}) and scale (\eqn{\sigma}). If a
three-parametric model is selected, the threshold parameter (\eqn{\gamma})
has to be the third element.}

\item{distribution}{Supposed distribution of the random variable.}
}
\value{
Returns the log-likelihood value for the data with respect to the parameters
given in \code{dist_params}.
}
\description{
This function computes the log-likelihood value with respect to a given set
of parameters. For two-parametric models the location and scale parameters
are required. If a three-parametric lifetime distribution is needed an
additional threshold parameter has to be provided. In terms of
\emph{Maximum Likelihood Estimation} this function can be optimized (\code{\link{optim}})
to estimate the parameters and variance-covariance matrix of the parameters.
}
\examples{
# Vectors:
cycles <- alloy$cycles
status <- alloy$status

# Example 1 - Evaluating Log-Likelihood function of two-parametric weibull:
loglik_weib <- loglik_function(
  x = cycles,
  status = status,
  dist_params = c(5.29, 0.33),
  distribution = "weibull"
)

# Example 2 - Evaluating Log-Likelihood function of three-parametric weibull:
loglik_weib3 <- loglik_function(
  x = cycles,
  status = status,
  dist_params = c(4.54, 0.76, 92.99),
  distribution = "weibull3"
)

}
\references{
Meeker, William Q; Escobar, Luis A., Statistical methods for
  reliability data, New York: Wiley series in probability and statistics, 1998
}
