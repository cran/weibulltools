#' Estimation of Failure Probabilities
#'
#' @description
#' This function applies a non-parametric method to estimate the failure
#' probabilities of complete data taking (multiple) right-censored observations
#' into account.
#'
#' @template details-estimate-cdf
#' @templateVar header One or multiple techniques can be used for the `methods` argument:
#'
#' @param x A tibble with class `wt_reliability_data` returned by [reliability_data].
#' @param methods One or multiple methods of `"mr"`, `"johnson"`, `"kaplan"` or
#' `"nelson"` used for the estimation of failure probabilities. See 'Details'.
#' @param options A list of named options. See 'Options'.
#' @template dots
#'
#' @return A tibble with class `wt_cdf_estimation` containing the following columns:
#'
#' * `id` : Identification for every unit.
#' * `x` : Lifetime characteristic.
#' * `status` : Binary data (0 or 1) indicating whether a unit is a right
#'   censored observation (= 0) or a failure (= 1).
#' * `rank` : The (computed) ranks. Determined for methods `"mr"` and `"johnson"`,
#'   filled with `NA` for other methods or if `status = 0`.
#' * `prob` : Estimated failure probabilities, `NA` if `status = 0`.
#' * `cdf_estimation_method` : Specified method for the estimation of failure
#'   probabilities.
#'
#' @section Options:
#' Argument `options` is a named list of options:
#'
#' | Method    | Name             | Value                                     |
#' | --------- | ---------------- | ----------------------------------------- |
#' | `mr`      | `mr_method`      | `"benard"` (default) or `"invbeta"`       |
#' | `mr`      | `mr_ties.method` | `"max"` (default), `"min"` or `"average"` |
#' | `johnson` | `johnson_method` | `"benard"` (default) or `"invbeta"`       |
#'
#' @references *NIST/SEMATECH e-Handbook of Statistical Methods*,
#' *8.2.1.5. Empirical model fitting - distribution free (Kaplan-Meier) approach*,
#' [NIST SEMATECH](https://www.itl.nist.gov/div898/handbook/apr/section2/apr215.htm),
#' December 3, 2020
#'
#' @examples
#' # Reliability data:
#' data <- reliability_data(
#'   alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Example 1 - Johnson method:
#' prob_tbl <- estimate_cdf(
#'   x = data,
#'   methods = "johnson"
#' )
#'
#' # Example 2 - Multiple methods:
#' prob_tbl_2 <- estimate_cdf(
#'   x = data,
#'   methods = c("johnson", "kaplan", "nelson")
#' )
#'
#' # Example 3 - Method 'mr' with options:
#' prob_tbl_3 <- estimate_cdf(
#'   x = data,
#'   methods = "mr",
#'   options = list(
#'     mr_method = "invbeta",
#'     mr_ties.method = "average"
#'   )
#' )
#'
#' # Example 4 - Multiple methods and options:
#' prob_tbl_4 <- estimate_cdf(
#'   x = data,
#'   methods = c("mr", "johnson"),
#'   options = list(
#'     mr_ties.method = "max",
#'     johnson_method = "invbeta"
#'   )
#' )
#'
#' @md
#'
#' @export
estimate_cdf <- function(x, ...) {
  UseMethod("estimate_cdf")
}



#' @rdname estimate_cdf
#'
#' @export
estimate_cdf.wt_reliability_data <- function(x,
                                             methods = c(
                                               "mr", "johnson", "kaplan",
                                               "nelson"
                                             ),
                                             options = list(),
                                             ...
) {

  methods <- if (missing(methods)) {
    "mr"
  } else {
    unique(match.arg(methods, several.ok = TRUE))
  }

  tbl_out <- purrr::map_dfr(methods, function(method) {
    if (method == "mr") {
      mr_method_(
        data = x,
        method = options$mr_method %||% "benard",
        ties.method = options$mr_ties.method %||% "max"
      )
    } else if (method == "johnson") {
      johnson_method_(
        data = x,
        method = options$johnson_method %||% "benard"
      )
    } else {
      switch(
        method,
        "kaplan" = kaplan_method_(data = x),
        "nelson" = nelson_method_(data = x)
      )
    }
  })

  class(tbl_out) <- c("wt_cdf_estimation", class(tbl_out))

  return(tbl_out)
}



#' Estimation of Failure Probabilities
#'
#' @inherit estimate_cdf description return references
#'
#' @template details-estimate-cdf
#' @templateVar header The following techniques can be used for the `method` argument:
#'
#' @inheritParams estimate_cdf
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param status A vector of binary data (0 or 1) indicating whether unit *i*
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param id A vector for the identification of every unit. Default is `NULL`.
#' @param method Method used for the estimation of failure probabilities. See
#'   'Details'.
#'
#' @inheritSection estimate_cdf Options
#'
#' @seealso [estimate_cdf]
#'
#' @examples
#' # Vectors:
#' cycles <- alloy$cycles
#' status <- alloy$status
#'
#' # Example 1 - Johnson method:
#' prob_tbl <- estimate_cdf(
#'   x = cycles,
#'   status = status,
#'   method = "johnson"
#' )
#'
#'
#' # Example 2 - Method 'mr' with options:
#' prob_tbl_2 <- estimate_cdf(
#'   x = cycles,
#'   status = status,
#'   method = "mr",
#'   options = list(
#'     mr_method = "invbeta",
#'     mr_ties.method = "average"
#'   )
#' )
#'
#' @md
#'
#' @export
estimate_cdf.default <- function(x,
                                 status,
                                 id = NULL,
                                 method = c(
                                   "mr", "johnson", "kaplan", "nelson"
                                 ),
                                 options = list(),
                                 ...
) {
  # Fail early, if user tries to call estimate_cdf.wt_reliability_data with a
  # tibble which is not of class reliability data. Otherwise failure would occur
  # in reliability_data, which is counterintuitive
  status

  data <- reliability_data(x = x, status = status, id = id)

  method <- match.arg(method)

  estimate_cdf.wt_reliability_data(
    x = data,
    methods = method,
    options = options
  )
}



#' @export
print.wt_cdf_estimation <- function(x, ...) {
  n_methods <- length(unique(x$cdf_estimation_method))
  if (n_methods == 1) {
    cat(
      "CDF estimation for method '", x$cdf_estimation_method[1], "':\n",
      sep = ""
    )
  } else {
    cat(
      "CDF estimation for methods ",
      paste0("'", unique(x$cdf_estimation_method), "'", collapse = ", "),
      ":\n",
      sep = ""
    )
  }
  NextMethod()
}



#' Estimation of Failure Probabilities using Median Ranks
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `mr_method()` is no longer under active development, switching to [estimate_cdf]
#' is recommended.
#'
#' @details
#' This non-parametric approach (*Median Ranks*) is used to estimate the
#' failure probabilities in terms of complete data. Two methods are available to
#' estimate the cumulative distribution function *F(t)*:
#'
#' * `"benard"` : Benard's approximation for Median Ranks.
#' * `"invbeta"` : Exact Median Ranks using the inverse beta distribution.
#'
#' @inheritParams estimate_cdf.default
#' @param status A vector of ones indicating that every unit has failed.
#' @param method Method for the estimation of the cdf. Can be `"benard"` (default)
#' or `"invbeta"`.
#' @param ties.method A character string specifying how ties are treated,
#' default is `"max"`.
#'
#' @return A tibble with only failed units containing the following columns:
#'
#' * `id` : Identification for every unit.
#' * `x` : Lifetime characteristic.
#' * `status` : Status of failed units (always 1).
#' * `rank` : Assigned ranks.
#' * `prob` : Estimated failure probabilities.
#' * `cdf_estimation_method` : Specified method for the estimation of failure
#'    probabilities (always 'mr').
#'
#' @examples
#' # Vectors:
#' obs   <- seq(10000, 100000, 10000)
#' state <- rep(1, length(obs))
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23", "Uy12", "kl1a")
#'
#' # Example 1 - Benard's approximation:
#' tbl_mr <- mr_method(
#'   x = obs,
#'   status = state,
#'   id = uic,
#'   method = "benard"
#' )
#'
#' # Example 2 - Inverse beta distribution:
#' tbl_mr_invbeta <- mr_method(
#'   x = obs,
#'   status = state,
#'   method = "invbeta"
#' )
#'
#' @md
#'
#' @export
mr_method <- function(x,
                      status = rep(1, length(x)),
                      id = NULL,
                      method = c("benard", "invbeta"),
                      ties.method = c("max", "min", "average")
) {
  deprecate_soft("2.0.0", "mr_method()", "estimate_cdf()")

  method <- match.arg(method)
  ties.method <- match.arg(ties.method)

  if (!purrr::is_null(id)) {
    if (!((length(x) == length(status)) && (length(x) == length(id)))) {
      stop("'x', 'status' and 'id' must be of same length!", call. = FALSE)
    }
  } else {
    if (length(x) != length(status)) {
      stop("'x' and 'status' must be of same length!", call. = FALSE)
    }
  }

  data <- reliability_data(x = x, status = status, id = id)

  mr_method_(data, method, ties.method)
}



mr_method_ <- function(data,
                       method = "benard",
                       ties.method = "max"
) {

  if (!all(data$status == 1)) {
    message("The 'mr' method only considers failed units (status == 1) and does",
            " not retain intact units (status == 0).")
  }

  tbl_in <- data %>%
    # Remove additional classes
    tibble::as_tibble()

  tbl_calc <- tbl_in %>%
    dplyr::filter(.data$status == 1) %>%
    dplyr::arrange(.data$x) %>%
    dplyr::mutate(rank = rank(.data$x, ties.method = ties.method))

  if (method == "benard") {
    tbl_calc <- dplyr::mutate(
      tbl_calc,
      prob = (.data$rank - .3) / (length(.data$x) + .4)
    )
  } else {
    tbl_calc <- dplyr::mutate(
      tbl_calc,
      prob = stats::qbeta(.5, .data$rank, length(.data$x) - .data$rank + 1)
    )
  }

  tbl_out <- tbl_calc %>%
    dplyr::mutate(cdf_estimation_method = "mr") %>%
    dplyr::relocate(
      "id", "x", "status", "rank", "prob", "cdf_estimation_method"
    )

  tbl_out
}



#' Estimation of Failure Probabilities using Johnson's Method
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `johnson_method()` is no longer under active development, switching to
#' [estimate_cdf] is recommended.
#'
#' @details
#' This non-parametric approach is used to estimate the failure probabilities in
#' terms of uncensored or (multiple) right censored data. Compared to complete
#' data the correction is done by calculating adjusted ranks which takes
#' non-defective units into account.
#'
#' @inheritParams mr_method
#' @param status A vector of binary data (0 or 1) indicating whether a unit is
#' a right censored observation (= 0) or a failure (= 1).
#'
#' @return A tibble containing the following columns:
#'
#' * `id` : Identification for every unit.
#' * `x` : Lifetime characteristic.
#' * `status` : Binary data (0 or 1) indicating whether a unit is a right
#'   censored observation (= 0) or a failure (= 1).
#' * `rank` : Adjusted ranks, `NA` if `status = 0`.
#' * `prob` : Estimated failure probabilities, `NA` if `status = 0`.
#' * `cdf_estimation_method` : Specified method for the estimation of failure
#'   probabilities (always 'johnson').
#'
#' @examples
#' # Vectors:
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23", "Uy12", "kl1a")
#'
#' # Example 1 - Johnson method for intact and failed units:
#' tbl_john <- johnson_method(
#'   x = obs,
#'   status = state,
#'   id = uic
#' )
#'
#' # Example 2 - Johnson's method works also if only defective units are considered:
#' tbl_john_2 <- johnson_method(
#'   x = obs,
#'   status = rep(1, length(obs))
#' )
#'
#' @md
#'
#' @export
johnson_method <- function(x,
                           status,
                           id = NULL,
                           method = c("benard", "invbeta")
) {
  deprecate_soft("2.0.0", "johnson_method()", "estimate_cdf()")

  method <- match.arg(method)

  if (!purrr::is_null(id)) {
    if (!((length(x) == length(status)) && (length(x) == length(id)))) {
      stop("'x', 'status' and 'id' must be of same length!", call. = FALSE)
    }
  } else {
    if (length(x) != length(status)) {
      stop("'x' and 'status' must be of same length!", call. = FALSE)
    }
  }

  data <- reliability_data(x = x, status = status, id = id)

  johnson_method_(data, method)
}



johnson_method_ <- function(data, method = "benard") {

  tbl_in <- data %>%
    # Remove additional classes
    tibble::as_tibble()

  tbl_calc <- tbl_in %>%
    dplyr::group_by(.data$x) %>%
    dplyr::mutate(
      failure = sum(.data$status == 1),
      survivor = sum(.data$status == 0)
    ) %>%
    dplyr::distinct(.data$x, .keep_all = TRUE) %>%
    dplyr::arrange(.data$x) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_i = .data$failure + .data$survivor,
      n_out = dplyr::lag(cumsum(.data$n_i), n = 1L, default = 0)
    ) %>%
    dplyr::mutate(
      rank = calculate_ranks(
        f = .data$failure,
        n_out = .data$n_out,
        n = sum(.data$n_i)
      )
    )

  if (method == "benard") {
    tbl_calc <- dplyr::mutate(
      tbl_calc,
      prob = (.data$rank - .3) / (sum(.data$n_i) + .4)
    )
  } else {
    tbl_calc <- dplyr::mutate(
      tbl_calc,
      prob = stats::qbeta(.5, .data$rank, sum(.data$n_i) - .data$rank + 1)
    )
  }

  tbl_out <- tbl_in %>%
    dplyr::arrange(.data$x) %>%
    dplyr::mutate(
      rank = ifelse(
        .data$status == 1,
        tbl_calc$rank[match(.data$x[order(.data$x)], tbl_calc$x)],
        NA_real_
      ),
      prob = ifelse(
        .data$status == 1,
        tbl_calc$prob[match(.data$x[order(.data$x)], tbl_calc$x)],
        NA_real_
      )
    ) %>%
    dplyr::mutate(cdf_estimation_method = "johnson") %>%
    dplyr::relocate(
      "id", "x", "status", "rank", "prob", "cdf_estimation_method"
    )

  tbl_out
}



#' Estimation of Failure Probabilities using the Kaplan-Meier Estimator
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `kaplan_method()` is no longer under active development, switching to
#' [estimate_cdf] is recommended.
#'
#' @details
#' Whereas the non-parametric Kaplan-Meier estimator is used to estimate the
#' survival function *S(t)* in terms of (multiple) right censored data, the
#' complement is an estimate of the cumulative distribution function *F(t)*.
#' One modification is made in contrast to the original Kaplan-Meier estimator
#' (see 'References').
#'
#' @inheritParams johnson_method
#'
#' @return A tibble containing the following columns:
#'
#' * `id` : Identification for every unit.
#' * `x` : Lifetime characteristic.
#' * `status` : Binary data (0 or 1) indicating whether a unit is a right
#'   censored observation (= 0) or a failure (= 1).
#' * `rank` : Filled with `NA`.
#' * `prob` : Estimated failure probabilities, `NA` if `status = 0`.
#' * `cdf_estimation_method` : Specified method for the estimation of failure
#'   probabilities (always 'kaplan').
#'
#' @references *NIST/SEMATECH e-Handbook of Statistical Methods*,
#' *8.2.1.5. Empirical model fitting - distribution free (Kaplan-Meier) approach*,
#' [NIST SEMATECH](https://www.itl.nist.gov/div898/handbook/apr/section2/apr215.htm),
#' December 3, 2020
#'
#' @examples
#' # Vectors:
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' state_2 <- c(0, 1, 1, 0, 0, 0, 1, 0, 0, 1)
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23","Uy12", "kl1a")
#'
#' # Example 1 - Observation with highest characteristic is an intact unit:
#' tbl_kap <- kaplan_method(
#'   x = obs,
#'   status = state,
#'   id = uic
#' )
#'
#' # Example 2 - Observation with highest characteristic is a defective unit:
#' tbl_kap_2 <- kaplan_method(
#'   x = obs,
#'   status = state_2
#' )
#'
#' @md
#'
#' @export
kaplan_method <- function(x,
                          status,
                          id = NULL
) {
  deprecate_soft("2.0.0", "kaplan_method()", "estimate_cdf()")

  if (!purrr::is_null(id)) {
    if (!((length(x) == length(status)) && (length(x) == length(id)))) {
      stop("'x', 'status' and 'id' must be of same length!", call. = FALSE)
    }
  } else {
    if (length(x) != length(status)) {
      stop("'x' and 'status' must be of same length!", call. = FALSE)
    }
  }

  data <- reliability_data(x = x, status = status, id = id)

  kaplan_method_(data)
}



kaplan_method_ <- function(data) {

  if (all(data$status == 1)) {
    warning(
      'Use methods = "mr" since there is no censored data problem!',
      call. = FALSE
    )
  }

  tbl_in <- data %>%
    # Remove additional classes
    tibble::as_tibble()

  tbl_calc <- tbl_in %>%
    dplyr::group_by(.data$x) %>%
    dplyr::mutate(
      failure = sum(.data$status == 1),
      survivor = sum(.data$status == 0)
    ) %>%
    dplyr::distinct(.data$x, .keep_all = TRUE) %>%
    dplyr::arrange(.data$x) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_i = .data$failure + .data$survivor,
      n_out = dplyr::lag(cumsum(.data$n_i), n = 1L, default = 0),
      n_in = sum(.data$n_i) - .data$n_out
    )

  if (tbl_in$status[which.max(tbl_in$x)] == 0) {
    tbl_calc <- tbl_calc %>%
      dplyr::mutate(
        prob = 1 - cumprod((.data$n_in - .data$failure) / .data$n_in)
      )
  } else {
    tbl_calc <- tbl_calc %>%
      dplyr::mutate(
        prob = 1 - (((.data$n_in + .7) / (.data$n_in + .4)) *
                      cumprod(
                        ((.data$n_in + .7) - .data$failure) / (.data$n_in + 1.7)
                      )
                    )
      )
  }

  tbl_out <- tbl_in %>%
    dplyr::arrange(.data$x) %>%
    dplyr::mutate(
      rank = NA_real_,
      prob = ifelse(
        .data$status == 1,
        tbl_calc$prob[match(.data$x[order(.data$x)], tbl_calc$x)],
        NA_real_
      ),
      cdf_estimation_method = "kaplan"
    ) %>%
    dplyr::relocate(
      "id", "x", "status", "rank", "prob", "cdf_estimation_method"
    )

  tbl_out
}



#' Estimation of Failure Probabilities using the Nelson-Aalen Estimator
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `nelson_method()` is no longer under active development, switching to
#' [estimate_cdf] is recommended.
#'
#' @details
#' This non-parametric approach estimates the cumulative hazard rate in
#' terms of (multiple) right censored data. By equating the definition of the
#' hazard rate with the hazard rate according to Nelson-Aalen one can calculate
#' the failure probabilities.
#'
#' @inheritParams johnson_method
#'
#' @return A tibble containing the following columns:
#'
#' * `id` : Identification for every unit.
#' * `x` : Lifetime characteristic.
#' * `status` : Binary data (0 or 1) indicating whether a unit is a right
#'   censored observation (= 0) or a failure (= 1).
#' * `rank` : Filled with `NA`.
#' * `prob` : Estimated failure probabilities, `NA` if `status = 0`.
#' * `cdf_estimation_method` : Specified method for the estimation of failure
#'   probabilities (always 'nelson').
#'
#' @examples
#' # Vectors:
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23","Uy12", "kl1a")
#'
#' # Example - Nelson-Aalen estimator applied to intact and failed units:
#' tbl_nel <- nelson_method(
#'   x = obs,
#'   status = state,
#'   id = uic
#' )
#'
#' @md
#'
#' @export
nelson_method <- function(x,
                          status,
                          id = NULL
) {
  deprecate_soft("2.0.0", "nelson_method()", "estimate_cdf()")

  if (!purrr::is_null(id)) {
    if (!((length(x) == length(status)) && (length(x) == length(id)))) {
      stop("'x', 'status' and 'id' must be of same length!", call. = FALSE)
    }
  } else {
    if (length(x) != length(status)) {
      stop("'x' and 'status' must be of same length!", call. = FALSE)
    }
  }

  data <- reliability_data(x = x, status = status, id = id)

  nelson_method_(data)
}



nelson_method_ <- function(data) {

  if (all(data$status == 1)) {
    warning(
      'Use methods = "mr" since there is no censored data problem!',
      call. = FALSE
    )
  }

  tbl_in <- data %>%
    # Remove additional classes
    tibble::as_tibble()

  tbl_calc <- tbl_in %>%
    dplyr::group_by(.data$x) %>%
    dplyr::mutate(
      failure = sum(.data$status == 1),
      survivor = sum(.data$status == 0)
    ) %>%
  dplyr::distinct(.data$x, .keep_all = TRUE) %>%
  dplyr::arrange(.data$x) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    n_out = .data$failure + .data$survivor,
    n_in = nrow(data) - dplyr::lag(cumsum(.data$n_out), n = 1L, default = 0),
    lam_nel = ifelse(.data$status == 1, .data$failure / .data$n_in, 0),
    H_nel = cumsum(.data$lam_nel),
    prob = 1 - exp(-.data$H_nel)
  )

  tbl_out <- tbl_in %>%
    dplyr::arrange(.data$x) %>%
    dplyr::mutate(
      rank = NA_real_,
      prob = ifelse(
        .data$status == 1,
        tbl_calc$prob[match(.data$x[order(.data$x)], tbl_calc$x)],
        NA_real_
      ),
      cdf_estimation_method = "nelson"
    ) %>%
    dplyr::relocate(
      "id", "x", "status", "rank", "prob", "cdf_estimation_method"
    )

  tbl_out
}
