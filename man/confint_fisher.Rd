% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence_intervals.R
\encoding{UTF-8}
\name{confint_fisher}
\alias{confint_fisher}
\alias{confint_fisher.wt_model}
\title{Fisher's Confidence Bounds for Quantiles and Probabilities}
\usage{
confint_fisher(x, ...)

\method{confint_fisher}{wt_model}(
  x,
  b_lives = c(0.01, 0.1, 0.5),
  bounds = c("two_sided", "lower", "upper"),
  conf_level = 0.95,
  direction = c("y", "x"),
  ...
)
}
\arguments{
\item{x}{A list with classes \code{wt_model} and \code{wt_ml_estimation} returned by
\link{ml_estimation}.}

\item{...}{Further arguments passed to or from other methods. Currently not used.}

\item{b_lives}{A numeric vector indicating the probabilities \eqn{p} of the
\eqn{B_p}-lives (quantiles) to be considered.}

\item{bounds}{A character string specifying the bound(s) to be computed.}

\item{conf_level}{Confidence level of the interval.}

\item{direction}{A character string specifying the direction of the confidence
interval. \code{"y"} for failure probabilities or \code{"x"} for quantiles.}
}
\value{
A tibble with class \code{wt_confint} containing the following columns:
\itemize{
\item \code{x} : An ordered sequence of the lifetime characteristic regarding the
failed units, starting at \code{min(x)} and ending up at \code{max(x)}. With
\code{b_lives = c(0.01, 0.1, 0.5)} the 1\%, 10\% and 50\% quantiles are additionally
included in \code{x}, but only if the specified probabilities are in the range of
the estimated probabilities.
\item \code{prob} : An ordered sequence of probabilities with specified \code{b_lives}
included.
\item \code{std_err} : Estimated standard errors with respect to \code{direction}.
\item \code{lower_bound} : Provided, if \code{bounds} is one of \code{"two_sided"} or \code{"lower"}.
Lower confidence limits with respect to \code{direction}, i.e. limits for
quantiles or probabilities.
\item \code{upper_bound} : Provided, if \code{bounds} is one of \code{"two_sided"} or \code{"upper"}.
Upper confidence limits with respect to \code{direction}, i.e. limits for
quantiles or probabilities.
\item \code{cdf_estimation_method} : A character that is always \code{NA_character}. Only
needed for internal use.
}

Further information is stored in the attributes of this tibble:
\itemize{
\item \code{distribution} : Distribution which was specified in \link{ml_estimation}.
\item \code{bounds} : Specified bound(s).
\item \code{direction} : Specified direction.
\item \code{model_estimation} : Input list with classes \code{wt_model} and \code{wt_ml_estimation}.
}
}
\description{
This function computes normal-approximation confidence intervals for quantiles
and failure probabilities.
}
\details{
The basis for the calculation of these confidence bounds are the standard errors
obtained by the \link[=delta_method]{delta method}.

The bounds on the probability are determined by the \emph{z-procedure}. See
'References' for more information on this approach.
}
\examples{
# Reliability data preparation:
## Data for two-parametric model:
data_2p <- reliability_data(
  shock,
  x = distance,
  status = status
)

## Data for three-parametric model:
data_3p <- reliability_data(
  alloy,
  x = cycles,
  status = status
)

# Model estimation with ml_estimation():
ml_2p <- ml_estimation(
  data_2p,
  distribution = "weibull"
)

ml_3p <- ml_estimation(
  data_3p,
  distribution = "lognormal3",
  conf_level = 0.90
)


# Example 1 - Two-sided 95\% confidence interval for probabilities ('y'):
conf_fisher_1 <- confint_fisher(
  x = ml_2p,
  bounds = "two_sided",
  conf_level = 0.95,
  direction = "y"
)

# Example 2 - One-sided lower/upper 90\% confidence interval for quantiles ('x'):
conf_fisher_2_1 <- confint_fisher(
  x = ml_2p,
  bounds = "lower",
  conf_level = 0.90,
  direction = "x"
)

conf_fisher_2_2 <- confint_fisher(
  x = ml_2p,
  bounds = "upper",
  conf_level = 0.90,
  direction = "x"
)

# Example 3 - Two-sided 90\% confidence intervals for both directions using
# a three-parametric model:

conf_fisher_3_1 <- confint_fisher(
  x = ml_3p,
  bounds = "two_sided",
  conf_level = 0.90,
  direction = "y"
)

conf_fisher_3_2 <- confint_fisher(
  x = ml_3p,
  bounds = "two_sided",
  conf_level = 0.90,
  direction = "x"
)

}
\references{
Meeker, William Q; Escobar, Luis A., Statistical methods for
reliability data, New York: Wiley series in probability and statistics, 1998
}
