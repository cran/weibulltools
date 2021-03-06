test_that("confint_betabinom fails for methods kaplan and nelson", {
  data <- reliability_data(shock, x = distance, status = status)
  cdf_kaplan <- estimate_cdf(data, "k")
  cdf_nelson <- estimate_cdf(data, "n")
  rr_kaplan <- rank_regression(cdf_kaplan)
  rr_nelson <- rank_regression(cdf_nelson)

  expect_error(confint_betabinom(rr_kaplan), "The beta binomial.*")
  expect_error(confint_betabinom(rr_nelson), "The beta binomial.*")
})

test_that("confint_betabinom remains stable", {
  obs   <- seq(10000, 100000, 10000)
  state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
  tbl <- reliability_data(x = obs, status = state)

  tbl_john <- estimate_cdf(tbl, "johnson")

  rr <- rank_regression(
    tbl_john,
    distribution = "weibull",
    conf_level = .95
  )

  conf_betabin <- confint_betabinom(
    x = tbl_john$x,
    status = tbl_john$status,
    dist_params = rr$coefficients,
    distribution = "weibull",
    bounds = "two_sided",
    conf_level = 0.95,
    direction = "y"
  )

  expect_snapshot_output(conf_betabin)

  cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
                224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
                180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
                159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
                139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
                112, 108, 104, 99, 99, 96, 94)

  state <- c(rep(0, 5), rep(1, 67))
  tbl_2 <- reliability_data(x = cycles, status = state)

  tbl_john_2 <- estimate_cdf(tbl_2, "johnson")

  rr_weib3 <- rank_regression(
    x = tbl_john_2$x,
    y = tbl_john_2$prob,
    status = tbl_john_2$status,
    distribution = "weibull3",
    conf_level = .95
  )

  conf_betabin_weib3 <- confint_betabinom(
    x = tbl_john_2$x,
    status = tbl_john_2$status,
    dist_params = rr_weib3$coefficients,
    distribution = "weibull3",
    bounds = "two_sided",
    conf_level = 0.95,
    direction = "y"
  )

  expect_snapshot_output(conf_betabin_weib3)

  conf_betabin_x <- confint_betabinom(
    x = tbl_john$x,
    status = tbl_john$status,
    dist_params = rr$coefficients,
    distribution = "weibull",
    bounds = "two_sided",
    conf_level = 0.95,
    direction = "x"
  )

  expect_snapshot_output(conf_betabin_x)
})

test_that("confint_fisher remains stable", {
  obs   <- seq(10000, 100000, 10000)
  state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
  tbl <- reliability_data(x = obs, status = state)

  tbl_john <- estimate_cdf(tbl, "johnson")

  mle <- ml_estimation(tbl, distribution = "weibull", conf_level = 0.95)

  conf_fish <- confint_fisher(
    x = mle,
    bounds = "two_sided",
    conf_level = 0.95,
    direction = "y"
  )

  expect_snapshot_output(conf_fish)

  conf_fish_x <- confint_fisher(
    x = mle,
    bounds = "two_sided",
    conf_level = 0.95,
    direction = "x"
  )

  expect_snapshot_output(conf_fish_x)
})

test_that("delta_method remains stable", {
  obs   <- seq(10000, 100000, 10000)
  status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
  data <- reliability_data(x = obs, status = status)

  mle <- ml_estimation(
    data,
    distribution = "weibull",
    conf_level = 0.95
  )

  delta_prob <- sapply(obs, delta_method,
                       dist_params = mle$coefficients,
                       dist_varcov = mle$varcov,
                       distribution = "weibull",
                       direction = "y"
  )

  expect_snapshot_output(delta_prob)
})

test_that("predict_quantile remains stable", {
  quants <- predict_quantile(
    p = c(0.01, 0.1, 0.5),
    dist_params = c(5, 0.5),
    distribution = "weibull"
  )

  expect_snapshot_output(quants)

  quants_weib3 <- predict_quantile(
    p = c(0.01, 0.1, 0.5),
    dist_params = c(5, 0.5, 10),
    distribution = "weibull3"
  )

  expect_snapshot_output(quants_weib3)
})

test_that("predict_prob remains stable", {
  probs <- predict_prob(
    q = c(15, 48, 124),
    dist_params = c(5, 0.5),
    distribution = "weibull"
  )

  expect_snapshot_output(probs)

  probs_weib3 <- predict_prob(
    q = c(25, 58, 134),
    dist_params = c(5, 0.5, 10),
    distribution = "weibull3"
  )

  expect_snapshot_output(probs_weib3)
})

