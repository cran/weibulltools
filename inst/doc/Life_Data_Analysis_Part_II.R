## ----setup, echo=FALSE, message=FALSE-----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  screenshot.force = FALSE,
  comment = "#>"
)
library(weibulltools)

## ----dataset_shock, message = FALSE-------------------------------------------
shock_tbl <- reliability_data(data = shock, x = distance, status = status)
shock_tbl

## ---- data_alloy--------------------------------------------------------------
# Data: 
alloy_tbl <- reliability_data(data = alloy, x = cycles, status = status)
alloy_tbl

## ----RR_weibull, fig.cap = "Figure 1: RR for a two-parametric Weibull distribution.", message = FALSE----
# rank_regression needs estimated failure probabilities: 
shock_cdf <- estimate_cdf(shock_tbl, methods = "johnson")

# Estimating two-parameter Weibull: 
rr_weibull <- rank_regression(shock_cdf, distribution = "weibull")
rr_weibull 

# Probability plot: 
weibull_grid <- plot_prob(
  shock_cdf,
  distribution = "weibull", 
  title_main = "Weibull Probability Plot", 
  title_x = "Mileage in km", 
  title_y = "Probability of Failure in %",
  title_trace = "Defectives",
  plot_method = "ggplot2"
)

# Add regression line: 
weibull_plot <- plot_mod(
  weibull_grid,
  x = rr_weibull,
  title_trace = "Rank Regression"
)

weibull_plot

## ----ML_weibull, fig.cap = "Figure 2: ML for a two-parametric Weibull distribution.", message = FALSE----
# Again estimating Weibull: 
ml_weibull <- ml_estimation(
  shock_tbl, 
  distribution = "weibull"
)

ml_weibull 

# Add ML estimation to weibull_grid: 
weibull_plot2 <- plot_mod(
  weibull_grid, 
  x = ml_weibull, 
  title_trace = "Maximum Likelihood"
)

weibull_plot2

## ----ML_estimation_log-normal, message = FALSE--------------------------------
# Two-parameter log-normal:  
ml_lognormal <- ml_estimation(
  alloy_tbl,
  distribution = "lognormal"
)

ml_lognormal

# Three-parameter Log-normal:  
ml_lognormal3 <- ml_estimation(
  alloy_tbl,
  distribution = "lognormal3"
)

ml_lognormal3

## ----ML_visualization_I, fig.cap = "Figure 3: ML for a two-parametric log-normal distribution.", message = FALSE----
# Constructing probability plot: 
tbl_cdf_john <- estimate_cdf(alloy_tbl, "johnson")

lognormal_grid <- plot_prob(
  tbl_cdf_john,
  distribution = "lognormal", 
  title_main = "Log-normal Probability Plot", 
  title_x = "Cycles", 
  title_y = "Probability of Failure in %",
  title_trace = "Failed units",
  plot_method = "ggplot2"
)

# Add two-parametric model to grid: 
lognormal_plot <- plot_mod(
  lognormal_grid,
  x = ml_lognormal,
  title_trace = "Two-parametric log-normal"
)

lognormal_plot

## ----ML_visualization_II, fig.cap = "Figure 4: ML for a three-parametric log-normal distribution.", message = FALSE----
# Add three-parametric model to lognormal_plot:
lognormal3_plot <- plot_mod(
  lognormal_grid, 
  x = ml_lognormal3, 
  title_trace = "Three-parametric log-normal"
)

lognormal3_plot

