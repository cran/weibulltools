## ----setup, echo=FALSE, message=FALSE-----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  screenshot.force = FALSE,
  comment = "#>"
)
library(weibulltools)

## ----dataset_shock, message = FALSE-------------------------------------------
# Data:
shock_tbl <- reliability_data(data = shock, x = distance, status = status)
shock_tbl

## ---- Parameter estimation procedures-----------------------------------------
# Estimation of failure probabilities:
shock_cdf <- estimate_cdf(shock_tbl, methods = "johnson")

# Rank Regression:
rr_weibull <- rank_regression(shock_cdf, distribution = "weibull")

# Maximum Likelihood Estimation: 
ml_weibull <- ml_estimation(shock_tbl, distribution = "weibull")

## ---- Confidence intervals for model parameters-------------------------------
# Confidence intervals based on Rank Regression: 
rr_weibull$confint

# Confidence intervals based on Maximum Likelihood Estimation:
ml_weibull$confint

## ---- Confidence level--------------------------------------------------------
# Confidence intervals based on another confidence level: 
ml_weibull_99 <- ml_estimation(shock_tbl, distribution = "weibull", conf_level = 0.99)
ml_weibull_99$confint

## ---- Confidence intervals for probabilities----------------------------------
# Beta-Binomial confidence bounds:
conf_bb <- confint_betabinom(
  x = rr_weibull, 
  b_lives = c(0.01, 0.1, 0.5), 
  bounds = "two_sided", 
  conf_level = 0.95, 
  direction = "y"
)
conf_bb

# Fisher's normal approximation confidence intervals:
conf_fisher <- confint_fisher(x = ml_weibull)
conf_fisher

## ---- Preparation for visualization-------------------------------------------
# Probability plot
weibull_grid <- plot_prob(
  shock_cdf,
  distribution = "weibull", 
  title_main = "Weibull Probability Plot", 
  title_x = "Mileage in km", 
  title_y = "Probability of Failure in %",
  title_trace = "Defectives",
  plot_method = "ggplot2"
)

## ---- BBB on failure probabilities, fig.cap = "Figure 1: Beta-Binomial confidence bounds for failure probabilities.", message = FALSE----
# Beta-Binomial confidence intervals: 
weibull_conf_bb <- plot_conf(
  weibull_grid, 
  conf_bb, 
  title_trace_mod = "Rank Regression", 
  title_trace_conf = "Beta-Binomial Bounds"
)
weibull_conf_bb

## ---- FI on failure probabilities, fig.cap = "Figure 2: Fisher's normal approximation confidence intervals for failure probabilities.", message = FALSE----
# Fisher's normal approximation confidence intervals:
weibull_conf_fisher <- plot_conf(
  weibull_grid, 
  conf_fisher, 
  title_trace_mod = "Maximum Likelihood", 
  title_trace_conf = "Fisher's Confidence Intervals"
  )
weibull_conf_fisher

## ---- Confidence intervals for quantiles--------------------------------------
# Computation of confidence intervals for quantiles: 
## Beta-Binomial confidence intervals: 
conf_bb_x <- confint_betabinom(
  x = rr_weibull, 
  bounds = "upper", 
  conf_level = 0.95, 
  direction = "x"
)
conf_bb_x

## Fisher's normal approximation confidence intervals:
conf_fisher_x <- confint_fisher(x = ml_weibull, bounds = "lower", direction = "x")
conf_fisher_x

## ---- BBB on quantiles, fig.cap = "Figure 3: One-sided (upper) Beta-Binomial confidence bound for quantiles.", message = FALSE----
# Visualization: 
## Beta-Binomial confidence intervals: 
weibull_conf_bb_x <- plot_conf(
  weibull_grid,
  conf_bb_x, 
  title_trace_mod = "Rank Regression", 
  title_trace_conf = "Beta-Binomial Bounds"
)
weibull_conf_bb_x

## ---- FI on quantiles, fig.cap = "Figure 4: One-sided (lower) normal approximation confidence interval for quantiles.", message = FALSE----
## Fisher's normal approximation confidence intervals:
weibull_conf_fisher_x <- plot_conf(
  weibull_grid, 
  conf_fisher_x, 
  title_trace_mod = "Maximum Likelihood", 
  title_trace_conf = "Fisher's Confidence Intervals"
)
weibull_conf_fisher_x

