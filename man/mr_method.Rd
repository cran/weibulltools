% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probability_estimators.R
\name{mr_method}
\alias{mr_method}
\title{Estimation of Failure Probabilities using Median Ranks}
\usage{
mr_method(
  x,
  status = rep(1, length(x)),
  id = NULL,
  method = c("benard", "invbeta"),
  ties.method = c("max", "min", "average")
)
}
\arguments{
\item{x}{A numeric vector which consists of lifetime data. Lifetime
data could be every characteristic influencing the reliability of a product,
e.g. operating time (days/months in service), mileage (km, miles), load
cycles.}

\item{status}{A vector of ones indicating that every unit \emph{i} has failed.}

\item{id}{A vector for the identification of every unit. Default is \code{NULL}.}

\item{method}{Method for the estimation of the cdf. Can be "benard" (default)
or "invbeta".}

\item{ties.method}{A character string specifying how ties are treated, default is "max".}
}
\value{
A tibble with failed units containing the following columns:
\itemize{
\item \code{id} : Identification for every unit.
\item \code{x} : Lifetime characteristic.
\item \code{status} : Status of failed units (always 1).
\item \code{rank} : The assigned ranks.
\item \code{prob} : Estimated failure probabilities.
\item \code{method} : Specified method for the estimation of failure
probabilities (always 'mr').
}
}
\description{
\ifelse{html}{\figure{lifecycle-soft-deprecated.svg}{options: alt='Soft-deprecated lifecycle'}}{\strong{Soft-deprecated}}

\code{mr_method()} is no longer under active development, switching
to \code{\link{estimate_cdf}} is recommended.
}
\details{
This non-parametric approach (\emph{Median Ranks}) is used to estimate the
failure probabilities in terms of complete data. Two methods are available to
estimate the cumulative distribution function \emph{F(t)}:
\itemize{
\item "benard" : Benard's approximation for Median Ranks.
\item "invbeta" : Exact Median Ranks using the inverse beta distribution.
}
}
\examples{
# Vectors:
obs   <- seq(10000, 100000, 10000)
state <- rep(1, length(obs))
uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
           "fl29", "AX23", "Uy12", "kl1a")

# Example 1 - Benard's approximation:
tbl_mr <- mr_method(
  x = obs,
  status = state,
  id = uic,
  method = "benard"
)

# Example 2 - Inverse beta distribution:
tbl_mr_invbeta <- mr_method(
  x = obs,
  status = state,
  method = "invbeta"
)

}
