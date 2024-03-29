% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\encoding{UTF-8}
\name{plot_mod_mix}
\alias{plot_mod_mix}
\title{Add Estimated Population Lines of a Separated Mixture Model to a
Probability Plot}
\usage{
plot_mod_mix(
  p_obj,
  x,
  status,
  mix_output,
  distribution = c("weibull", "lognormal", "loglogistic"),
  title_trace = "Fit",
  ...
)
}
\arguments{
\item{p_obj}{A plot object returned by \link{plot_prob_mix}.}

\item{x}{A numeric vector containing the x-coordinates of the respective
regression line.}

\item{status}{A vector of binary data (0 or 1) indicating whether a unit is a
right censored observation (= 0) or a failure (= 1).}

\item{mix_output}{A list returned by \link{mixmod_regression} or \link{mixmod_em},
which consists of elements necessary to visualize the regression lines.}

\item{distribution}{Supposed distribution of the random variable.}

\item{title_trace}{A character string which is assigned to the legend trace.}

\item{...}{Further arguments passed to or from other methods. Currently not used.}
}
\value{
A plot object containing the probability plot with plotting positions
and estimated regression line(s).
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#soft-deprecated}{\figure{lifecycle-soft-deprecated.svg}{options: alt='[Soft-deprecated]'}}}{\strong{[Soft-deprecated]}}

\code{plot_mod_mix()} is no longer under active development, switching to
\link{plot_mod} is recommended.
}
\details{
This function adds one or multiple estimated regression lines to an existing
probability plot \link{plot_prob}). Depending on the output of the function
\link{mixmod_regression} or \link{mixmod_em} one or multiple lines are plotted.

The name of the legend entry is a combination of the \code{title_trace} and the
number of determined subgroups. If \code{title_trace = "Line"} and the data has
been split in two groups, the legend entries would be \code{"Line: 1"} and
\code{"Line: 2"}.
}
\examples{
# Vectors:
hours <- voltage$hours
status <- voltage$status

# Example 1 - Using result of mixmod_em in mix_output:
mix_mod_em <- mixmod_em(
  x = hours,
  status = status,
  distribution = "weibull",
  conf_level = 0.95,
  k = 2,
  method = "EM",
  n_iter = 150
)

plot_weibull_em <- plot_prob_mix(
  x = hours,
  status = status,
  id = id,
  distribution = "weibull",
  mix_output = mix_mod_em
)

plot_weibull_emlines <- plot_mod_mix(
  p_obj = plot_weibull_em,
  x = hours,
  status = status,
  mix_output = mix_mod_em,
  distribution = "weibull"
)

# Example 2 - Using result of mixmod_regression in mix_output:
john <- johnson_method(x = hours, status = status)
mix_mod_reg <- mixmod_regression(
  x = john$x,
  y = john$prob,
  status = john$status,
  distribution = "weibull"
)

plot_weibull_reg <- plot_prob_mix(
  x = john$x,
  status = john$status,
  id = john$id,
  distribution = "weibull",
  mix_output = mix_mod_reg,
)

plot_weibull_reglines <- plot_mod_mix(
  p_obj = plot_weibull_reg,
  x = john$x,
  status = john$status,
  mix_output = mix_mod_reg,
  distribution = "weibull"
)

}
\references{
Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
Failure Mode, Quality Progress, 35(6), 47-52, 2002
}
