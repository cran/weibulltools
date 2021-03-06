#' @export
plot_layout_vis.ggplot <- function(
  # An empty ggplot object
  p_obj,
  x,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability"
) {

  distribution <- match.arg(distribution)

  layout_helper <- plot_layout_helper(x, distribution, "ggplot2")

  p_obj <- if (distribution %in% c("sev", "normal", "logistic")) {
    p_obj +
      ggplot2::scale_x_continuous(
        breaks = layout_helper$x_ticks,
        minor_breaks = NULL,
        labels = layout_helper$x_labels
      )
  } else {
    p_obj +
      ggplot2::scale_x_log10(
        breaks = layout_helper$x_ticks,
        minor_breaks = NULL,
        labels = layout_helper$x_labels
      )
  }

  p_obj <- p_obj +
    ggplot2::scale_y_continuous(
      breaks = layout_helper$y_ticks,
      minor_breaks = NULL,
      labels = layout_helper$y_labels,
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # Rotate x axis labels
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      # Center titel
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(title = title_main, x = title_x, y = title_y)

  return(p_obj)
}

#' @export
plot_prob_vis.ggplot <- function(
  p_obj, tbl_prob,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability",
  title_trace = "Sample"
) {

  distribution <- match.arg(distribution)

  n_method <- length(unique(tbl_prob$cdf_estimation_method))
  n_group <- length(unique(tbl_prob[["group"]]))

  if (n_method == 1) tbl_prob$cdf_estimation_method <- ""
  if (n_group <= 1) tbl_prob$group <- ""

  mapping <- if (n_group <= 1) {
    if (n_method == 1) {
      ggplot2::aes(x = .data$x, y = .data$q, color = I("#3C8DBC"))
    } else {
      ggplot2::aes(x = .data$x, y = .data$q, color = .data$cdf_estimation_method)
    }
  } else {
    if (n_method == 1) {
      ggplot2::aes(
        x = .data$x, y = .data$q, color = .data$group
      )
    } else {
      ggplot2::aes(
        x = .data$x, y = .data$q, color = .data$cdf_estimation_method,
        shape = .data$group
      )
    }
  }

  labs <- if (n_group <= 1 || n_method == 1) {
    ggplot2::labs(color = title_trace)
  } else {
    ggplot2::labs(color = title_trace, shape = "Subgroups")
  }

  p_prob <- p_obj +
    ggplot2::geom_point(
      data = tbl_prob, mapping = mapping
    ) +
    labs

  return(p_prob)
}

#' @export
plot_mod_vis.ggplot <- function(
  p_obj, tbl_pred, title_trace = "Fit"
) {

  n_method <- length(unique(tbl_pred$cdf_estimation_method))
  n_group <- length(unique(tbl_pred$group))

  if (n_method == 1) tbl_pred$cdf_estimation_method <- ""

  mapping <- if (n_group == 1) {
    if (n_method == 1) {
      ggplot2::aes(
        x = .data$x_p, y = .data$q, color = I("#CC2222")
      )
    } else {
      ggplot2::aes(
        x = .data$x_p, y = .data$q, color = .data$cdf_estimation_method
      )
    }
  } else {
    # group aesthetic must be paste of method and group to create distinct
    # groups
    if (n_method == 1) {
      ggplot2::aes(
        x = .data$x_p,
        y = .data$q,
        color = .data$group
      )
    } else {
      ggplot2::aes(
        x = .data$x_p,
        y = .data$q,
        color = .data$cdf_estimation_method,
        group = paste(.data$cdf_estimation_method, .data$group)
      )
    }
  }

  p_mod <- p_obj +
    ggplot2::geom_line(
      data = tbl_pred, mapping = mapping
    ) +
    ggplot2::labs(
      color = paste(p_obj$labels$colour, "+\n", title_trace)
    )

  return(p_mod)
}

#' @export
plot_conf_vis.ggplot <- function(p_obj, tbl_p, title_trace) {
  mapping <- if (all(is.na(tbl_p$cdf_estimation_method))) {
    ggplot2::aes(
      x = .data$x, y = .data$q, group = .data$bound, color = I("#CC2222")
    )
  } else {
    ggplot2::aes(
      x = .data$x,
      y = .data$q,
      group = paste(.data$bound, .data$cdf_estimation_method),
      color = .data$cdf_estimation_method
    )
  }

  p_conf <- p_obj +
    ggplot2::geom_line(
      data = tbl_p,
      mapping = mapping,
      linetype = "CC"
    ) +
    ggplot2::labs(
      color = paste(p_obj$labels$colour, "+\n", title_trace)
    )

  return(p_conf)
}

#' @export
plot_pop_vis.ggplot <- function(
  p_obj, tbl_pop, title_trace
) {
  p_pop <- p_obj +
    ggplot2::geom_line(
      data = tbl_pop,
      mapping = ggplot2::aes(x = .data$x_s, y = .data$q, color = .data$group)
    ) +
    ggplot2::labs(color = title_trace)

  return(p_pop)
}
