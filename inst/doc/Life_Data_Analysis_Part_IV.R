## ----setup, echo=FALSE, message=FALSE-----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  screenshot.force = FALSE,
  comment = "#>"
)
library(weibulltools)

## ----dataset_voltage, message = FALSE-----------------------------------------
voltage_tbl <- reliability_data(data = voltage, x = hours, status = status)
voltage_tbl

## ----probability_plot_weibull, fig.cap = "Figure 1: Plotting positions in Weibull grid.", message = FALSE----
# Estimating failure probabilities: 
voltage_cdf <- estimate_cdf(voltage_tbl, "johnson")

# Probability plot: 
weibull_plot <- plot_prob(
  voltage_cdf,
  distribution = "weibull", 
  title_main = "Weibull Probability Plot", 
  title_x = "Time in Hours", 
  title_y = "Probability of Failure in %",
  title_trace = "Defectives",
  plot_method = "ggplot2"
)

weibull_plot

## ----segmented_weibull_I, fig.cap = "Figure 2: Subgroup-specific plotting positions using segmented regression.", message = FALSE----
# Applying mixmod_regression(): 
mixreg_weib <- mixmod_regression(
  x = voltage_cdf, 
  distribution = "weibull", 
  k = 2
)

mixreg_weib

# Using plot_prob_mix(). 
mix_reg_plot <- plot_prob(
  x = mixreg_weib, 
  title_main = "Weibull Mixture Regression", 
  title_x = "Time in Hours", 
  title_y = "Probability of Failure", 
  title_trace = "Subgroup",
  plot_method = "ggplot2"
)

mix_reg_plot

## ----segmented_weibull_II, fig.cap = "Figure 3: Subgroup-specific regression lines using segmented regression.", message = FALSE----
# Using plot_mod() to visualize regression lines of subgroups: 
mix_reg_lines <- plot_mod(
  mix_reg_plot, 
  x = mixreg_weib, 
  title_trace = "Fitted Line"
)

mix_reg_lines

## ----em_weibull_I, fig.cap = "Figure 4: Subgroup-specific plotting positions using EM algorithm.", message = FALSE----
# Applying mixmod_regression(): 
mix_em_weib <- mixmod_em(
  x = voltage_tbl, 
  distribution = "weibull",
  k = 2
)

mix_em_weib

# Using plot_prob(): 
mix_em_plot <- plot_prob(
  x = mix_em_weib,
  title_main = "Weibull Mixture EM", 
  title_x = "Time in Hours", 
  title_y = "Probability of Failure", 
  title_trace = "Subgroup",
  plot_method = "ggplot2"
)

mix_em_plot

## ----em_weibull_II, fig.cap = "Figure 5: Subgroup-specific regression lines using EM algorithm.", message = FALSE----

# Using plot_mod() to visualize regression lines of subgroups: 
mix_em_lines <- plot_mod(
  mix_em_plot, 
  x = mix_em_weib, 
  title_trace = "Fitted Line"
)

mix_em_lines

