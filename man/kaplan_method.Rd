% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probability_estimators.R
\name{kaplan_method}
\alias{kaplan_method}
\title{Estimation of Failure Probabilities using the Kaplan-Meier Estimator}
\usage{
kaplan_method(x, status, id = NULL)
}
\arguments{
\item{x}{A numeric vector which consists of lifetime data. Lifetime
data could be every characteristic influencing the reliability of a product,
e.g. operating time (days/months in service), mileage (km, miles), load
cycles.}

\item{status}{A vector of binary data (0 or 1) indicating whether a unit is
a right censored observation (= 0) or a failure (= 1).}

\item{id}{A vector for the identification of every unit. Default is \code{NULL}.}
}
\value{
A tibble containing the following columns:
\itemize{
\item \code{id} : Identification for every unit.
\item \code{x} : Lifetime characteristic.
\item \code{status} : Binary data (0 or 1) indicating whether a unit is a right
censored observation (= 0) or a failure (= 1).
\item \code{rank} : Filled with \code{NA}.
\item \code{prob} : Estimated failure probabilities, \code{NA} if \code{status = 0}.
\item \code{cdf_estimation_method} : Specified method for the estimation of failure
probabilities (always 'kaplan').
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#soft-deprecated}{\figure{lifecycle-soft-deprecated.svg}{options: alt='[Soft-deprecated]'}}}{\strong{[Soft-deprecated]}}

\code{kaplan_method()} is no longer under active development, switching to
\link{estimate_cdf} is recommended.
}
\details{
Whereas the non-parametric Kaplan-Meier estimator is used to estimate the
survival function \emph{S(t)} in terms of (multiple) right censored data, the
complement is an estimate of the cumulative distribution function \emph{F(t)}.
One modification is made in contrast to the original Kaplan-Meier estimator
(see 'References').
}
\examples{
# Vectors:
obs   <- seq(10000, 100000, 10000)
state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
state_2 <- c(0, 1, 1, 0, 0, 0, 1, 0, 0, 1)
uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
           "fl29", "AX23","Uy12", "kl1a")

# Example 1 - Observation with highest characteristic is an intact unit:
tbl_kap <- kaplan_method(
  x = obs,
  status = state,
  id = uic
)

# Example 2 - Observation with highest characteristic is a defective unit:
tbl_kap_2 <- kaplan_method(
  x = obs,
  status = state_2
)

}
\references{
\emph{NIST/SEMATECH e-Handbook of Statistical Methods},
\emph{8.2.1.5. Empirical model fitting - distribution free (Kaplan-Meier) approach},
\href{https://www.itl.nist.gov/div898/handbook/apr/section2/apr215.htm}{NIST SEMATECH},
December 3, 2020
}
