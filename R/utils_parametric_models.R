# Function that checks if distribution has a threshold parameter:
has_thres <- function(distribution) {
  distribution %in% c("weibull3", "lognormal3", "loglogistic3", "exponential2")
}



# Function that removes the trailing number. Used for threshold distributions:
std_parametric <- function(distribution) {
  sub("[[:digit:]]$", "", distribution)
}



# Function that checks for standard variant of distribution (without threshold):
is_std_parametric <- function(distribution, dist_params) {
  n_par <- length(dist_params)

  if (distribution == "exponential") {
    n_par == 1L
  } else {
    n_par == 2L
  }
}



# Function that checks the correctness between input distribution and parameters:
check_dist_params <- function(dist_params, distribution) {
  three_parametric <- distribution %in%
    c("weibull3", "lognormal3", "loglogistic3")

  one_parametric <- distribution == "exponential"

  name_arg <- as.character(substitute(dist_params))

  if (three_parametric && length(dist_params) != 3) {
    stop(
      "A three-parametric distribution needs three parameters but ",
      sQuote(name_arg), " has length ", length(dist_params), ".",
      call. = FALSE
    )
  }

  if (!three_parametric && !one_parametric && length(dist_params) != 2) {
    stop(
      "A two-parametric distribution needs two parameters but ",
      sQuote(name_arg), " has length ", length(dist_params), ".",
      call. = FALSE
    )
  }

  if (one_parametric && length(dist_params) != 1) {
    stop(
      "A one-parametric distribution needs one parameter but ",
      sQuote(name_arg), " has length ", length(dist_params), ".",
      call. = FALSE
    )
  }
}



# Function that checks compatibility of plotting grid and model
check_compatible_distributions <- function(p_obj_dist, model_dist) {
  if (p_obj_dist != std_parametric(model_dist)) {
    msg <- paste0(
      "Incompatible distributions! Probability plot has distribution '",
      p_obj_dist,
      "' whereas model has distribution '",
      model_dist,
      "'."
    )

    stop(
      errorCondition(
        message = msg,
        class = "incompatible_distributions"
      )
    )
  }
}



# Function that converts Weibull loc-sc parameters to shape and scale:
to_shape_scale_params <- function(loc_sc_params) {

  # Coefficients:
  wb_params <- c(
    eta = exp(loc_sc_params[[1]]),
    beta = 1 / loc_sc_params[[2]],
    gamma = if (length(loc_sc_params) == 3L) loc_sc_params[[3]] else NULL
  )

  wb_params
}



# Function that converts Weibull location-scale confint to shape-scale confint:
to_shape_scale_confint <- function(loc_sc_confint) {

  # Confidence intervals:
  wb_confint <- loc_sc_confint
  wb_confint[1, ] <- exp(wb_confint[1, ])
  wb_confint[2, ] <- rev(1 / wb_confint[2, ])

  # Names:
  rownames(wb_confint)[1:2] <- c("eta", "beta")

  wb_confint
}



# Function that converts Weibull shape-scale parameters to location and scale:
to_location_scale_params <- function(shape_scale_params) {

  # Coefficients:
  wb_params <- c(
    mu = log(shape_scale_params[[1]]),
    sigma = 1 / shape_scale_params[[2]],
    gamma = if (length(shape_scale_params) == 3L) shape_scale_params[[3]] else NULL
  )

  wb_params
}



# Function that converts Weibull shape-scale confint to location and scale confint:
to_location_scale_confint <- function(shape_scale_confint) {

  # Confidence intervals:
  wb_confint <- shape_scale_confint
  wb_confint[1, ] <- log(wb_confint[1, ])
  wb_confint[2, ] <- rev(1 / wb_confint[2, ])

  # Names:
  rownames(wb_confint)[1:2] <- c("mu", "sigma")

  wb_confint
}



# Function that standardizes lifetime characteristic depending on distribution:
standardize <- function(x, dist_params, distribution) {

  # Length dist_params:
  n <- length(dist_params)

  # Threshold model:
  if (has_thres(distribution)) {
    x <- x - dist_params[[n]]

    n <- n - 1
  }

  # Standard-parametric model with q or q - threshold:
  distribution <- std_parametric(distribution)

  # (log-)location-scale:
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    x <- log(x)
  }

  # Standardize:
  z <- if (distribution != "exponential") {
    ## Location-scale:
    (x - dist_params[[1]]) / dist_params[[n]]
  } else {
    ## Scale:
    z <- x / dist_params[[1]]
  }

  z
}



# Quantile, probability and density functions of the smallest extreme value dist:
## Quantile function:
qsev <- function(p) {
  p <- ifelse(p >= 0.9999999999999999, 0.9999999999999999, p)
  p <- ifelse(p <= 1 - 0.9999999999999999, 1 - 0.9999999999999999, p)

  log(-log(1 - p))
}



## Probability function (cdf):
psev <- function(q) {
  1 - exp(-exp(q))
}



## Density function (pdf):
dsev <- function(x) {
  exp(x - exp(x))
}



# Function that simulates a sample of a 'Dirichlet' distribution:
rdirichlet <- function(n, par) {
  k <- length(par)
  z <- matrix(0, nrow = n, ncol = k)
  s <- matrix(0, nrow = n)
  for (i in 1:k) {
    z[, i] <- stats::rgamma(n, shape = par[i])
    s <- s + z[, i]
  }
  for (i in 1:k) {
    z[, i] <- z[, i]/s
  }
  return(z)
}



# Standard quantile function:
q_std <- function(p, distribution) {
  switch(
    distribution,
    "weibull" =,
    "sev" = qsev(p),
    "lognormal" =,
    "normal" = stats::qnorm(p),
    "loglogistic" =,
    "logistic" = stats::qlogis(p),
    "exponential" = stats::qexp(p)
  )
}



# Standard probability function:
p_std <- function(q, distribution) {
  switch(
    distribution,
    "weibull" =,
    "sev" = psev(q),
    "lognormal" =,
    "normal" = stats::pnorm(q),
    "loglogistic" =,
    "logistic" = stats::plogis(q),
    "exponential" = stats::pexp(q)
  )
}
