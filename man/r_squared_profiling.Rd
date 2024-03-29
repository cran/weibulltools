% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_squared_function.R
\encoding{UTF-8}
\name{r_squared_profiling}
\alias{r_squared_profiling}
\alias{r_squared_profiling.wt_cdf_estimation}
\title{R-Squared-Profile Function for Parametric Lifetime Distributions with Threshold}
\usage{
r_squared_profiling(x, ...)

\method{r_squared_profiling}{wt_cdf_estimation}(
  x,
  thres,
  distribution = c("weibull3", "lognormal3", "loglogistic3", "exponential2"),
  direction = c("x_on_y", "y_on_x"),
  ...
)
}
\arguments{
\item{x}{A \code{tibble} with class \code{wt_cdf_estimation} returned by \link{estimate_cdf}.}

\item{...}{Further arguments passed to or from other methods. Currently not used.}

\item{thres}{A numeric value for the threshold parameter.}

\item{distribution}{Supposed parametric distribution of the random variable.}

\item{direction}{Direction of the dependence in the regression model.}
}
\value{
Returns the coefficient of determination with respect to the threshold
parameter \code{thres}.
}
\description{
This function evaluates the coefficient of determination with respect to a
given threshold parameter of a parametric lifetime distribution. In terms of
\emph{Rank Regression} this function can be optimized (\link[stats:optim]{optim}) to
estimate the threshold parameter.
}
\examples{
# Data:
data <- reliability_data(
  alloy,
  x = cycles,
  status = status
)

# Probability estimation:
prob_tbl <- estimate_cdf(
  data,
  methods = "johnson"
)

# Determining the optimal coefficient of determination:
## Range of threshold parameter must be smaller than the first failure:
threshold <- seq(
  0,
  min(
    dplyr::pull(
      dplyr::filter(
        prob_tbl,
        status == 1,
        x == min(x)
      ),
      x
    ) - 0.1
  ),
  length.out = 100
)

## Coefficient of determination with respect to threshold values:
profile_r2 <- r_squared_profiling(
  x = dplyr::filter(
    prob_tbl,
    status == 1
  ),
  thres = threshold,
  distribution = "weibull3"
)

## Threshold value (among the candidates) that maximizes the coefficient of determination:
threshold[which.max(profile_r2)]

## plot:
plot(
  threshold,
  profile_r2,
  type = "l"
)
abline(
  v = threshold[which.max(profile_r2)],
  h = max(profile_r2),
  col = "red"
)

}
