## ----setup, echo=FALSE, message=FALSE-----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  screenshot.force = FALSE,
  comment = "#>"
)
library(weibulltools)

## ----rank_densities, fig.cap = "Figure 1: Densities for different ranks i in samples of size n = 10.", message = FALSE, warning = FALSE----
library(dplyr) # data manipulation 
library(ggplot2) # visualization

x <- seq(0, 1, length.out = 100) # CDF
n <- 10 # sample size
i <- c(1, 3, 5, 7, 9) # ranks
r <- n - i + 1 # inverse ranking

df_dens <- expand.grid(cdf = x, i = i) %>% 
  mutate(n = n, r = n - i + 1, pdf = dbeta(x = x, shape1 = i, shape2 = r))

densplot <- ggplot(data = df_dens, aes(x = cdf, y = pdf, colour = as.factor(i))) + 
  geom_line() + 
  scale_colour_discrete(guide = guide_legend(title = "i")) + 
  theme_bw() + 
  labs(x = "Failure Probability", y = "Density")
densplot

## ----dataset_shock, message = FALSE-------------------------------------------
shock_tbl <- reliability_data(data = shock, x = distance, status = status)
shock_tbl

## ----failure_probabilities----------------------------------------------------
# Estimate CDF with both methods: 
cdf_tbl <- estimate_cdf(shock_tbl, methods = c("mr", "johnson"))

# First case where only failed units are taken into account:
cdf_tbl_mr <- cdf_tbl %>% filter(cdf_estimation_method == "mr")
cdf_tbl_mr

# Second case where both, survived and failed units are considered:
cdf_tbl_john <- cdf_tbl %>% filter(cdf_estimation_method == "johnson") 
cdf_tbl_john

## ----probability_plot_weibull, fig.cap = "Figure 3: Plotting positions in Weibull grid.", message = FALSE----
# Weibull grid for estimated probabilities: 
weibull_grid <- plot_prob(
  cdf_tbl,
  distribution = "weibull", 
  title_main = "Weibull Probability Plot", 
  title_x = "Mileage in km", 
  title_y = "Probability of Failure in %",
  title_trace = "Method",
  plot_method = "ggplot2"
)

weibull_grid

## ----probability_plot_log-normal, fig.cap = "Figure 4: Plotting positions in log-normal grid.", message = FALSE----
# Log-normal grid for estimated probabilities: 
lognorm_grid <- plot_prob(
  cdf_tbl,
  distribution = "lognormal",
  title_main = "Log-normal Probability Plot",
  title_x = "Mileage in km",
  title_y = "Probability of Failure in %",
  title_trace = "Method",
  plot_method = "ggplot2"
)

lognorm_grid

