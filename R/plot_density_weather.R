#' Detection density per dawn and dusk session
#'
#' @description
#' Collapses an hourly [detection_density()] table into one row per site, date
#' and session, where hours before noon are `"dawn"` and the rest `"dusk"`.
#' Suits recorders scheduled around sunrise and sunset; it is not meaningful
#' for continuous recording.
#'
#' @param dens Output of `detection_density(..., unit = "hour")`.
#' @return A tibble: `Site`, `date`, `session`, `hours`, `detections`, `rate`.
#' @export
density_by_session <- function(dens) {
  if (!inherits(dens$time, "POSIXct")) {
    stop("density_by_session() needs detection_density(..., unit = \"hour\").",
         call. = FALSE)
  }
  dens |>
    dplyr::mutate(date = as.Date(.data$time),
                  session = factor(ifelse(as.integer(format(.data$time, "%H")) < 12,
                                          "dawn", "dusk"), levels = c("dawn", "dusk"))) |>
    dplyr::group_by(.data$Site, .data$date, .data$session) |>
    dplyr::summarise(hours = sum(.data$hours), detections = sum(.data$detections),
                     .groups = "drop") |>
    dplyr::mutate(rate = .data$detections / .data$hours)
}

weather_labels <- c(
  temperature_2m = "temperature (\u00b0C)",
  wind_speed_10m = "wind (km/h)",
  wind_gusts_10m = "max gust (km/h)",
  precipitation = "rain in window (mm)",
  rain_day = "daily rain (mm)",
  surface_pressure = "pressure (hPa)",
  pressure_change_24h = "24-h pressure change (hPa)",
  cloud_cover = "cloud cover (%)",
  moon_illumination = "moon illumination"
)
weather_label <- function(v) ifelse(v %in% names(weather_labels), weather_labels[v], v)

#' Detection heatmap with weather strips beneath
#'
#' @description
#' The [plot_density_heatmap()] view on top, and one strip per weather variable
#' underneath on the same date axis, so days of high activity can be read
#' against the conditions on those days. Session variables are drawn as a dawn
#' line and a dusk line; `rain_day` and `moon_illumination` once per day.
#'
#' @param dens Output of [detection_density()].
#' @param weather Output of [weather_by_session()].
#' @param vars Weather columns to show, top to bottom.
#' @param heights Relative height of the heatmap to each strip. Default 3.
#' @inheritParams plot_density
#'
#' @return A `patchwork` object (print or `ggsave()` it like a ggplot).
#' @export
plot_density_weather <- function(dens, weather, sites = NULL, date_range = NULL,
                                 vars = c("wind_speed_10m", "temperature_2m",
                                          "rain_day", "pressure_change_24h",
                                          "moon_illumination"),
                                 marks = NULL, mark_label = "marked day",
                                 confidence = NULL, title = NULL, subtitle = NULL,
                                 heights = 3) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("plot_density_weather() needs the 'patchwork' package.", call. = FALSE)
  }
  g <- density_daily_grid(dens, sites, date_range)
  xlim <- range(g$time) + c(-0.5, 0.5)
  top <- plot_density_heatmap(dens, sites = sites, date_range = date_range,
                              marks = marks, mark_label = mark_label,
                              confidence = confidence, title = title,
                              subtitle = subtitle) +
    ggplot2::scale_x_date(limits = xlim, expand = c(0, 0)) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())

  w <- weather[weather$date >= xlim[1] & weather$date <= xlim[2], ]
  once_daily <- c("rain_day", "moon_illumination")
  session_cols <- stats::setNames(unname(birdnet_palette(2)), c("dawn", "dusk"))
  strips <- lapply(seq_along(vars), function(i) {
    v <- vars[i]
    last <- i == length(vars)
    if (v %in% once_daily) {
      d <- w[w$session == "dawn", ]
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$date, .data[[v]]))
      p <- p + if (v == "rain_day") {
        ggplot2::geom_col(fill = birdnet_ink$secondary, width = 0.8)
      } else {
        ggplot2::geom_line(colour = birdnet_ink$secondary, linewidth = 0.5)
      }
    } else {
      p <- ggplot2::ggplot(w, ggplot2::aes(.data$date, .data[[v]], colour = .data$session)) +
        ggplot2::geom_line(linewidth = 0.5) +
        ggplot2::scale_colour_manual(values = session_cols, name = NULL)
    }
    if (v == "pressure_change_24h") {
      p <- p + ggplot2::geom_hline(yintercept = 0, colour = birdnet_ink$axis, linewidth = 0.3)
    }
    p +
      ggplot2::scale_x_date(limits = xlim, expand = c(0, 0)) +
      ggplot2::labs(x = NULL, y = NULL, title = NULL) +
      ggplot2::annotate("text", x = xlim[1] + 0.8, y = Inf, label = weather_label(v),
                        hjust = 0, vjust = 1.3, size = 2.8, colour = birdnet_ink$secondary) +
      birdnet_theme(base_size = 9) +
      ggplot2::theme(
        legend.position = if (i == 1) "right" else "none",
        axis.text.x = if (last) ggplot2::element_text() else ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(1, 5, 1, 5)
      )
  })
  patchwork::wrap_plots(c(list(top), strips), ncol = 1,
                        heights = c(heights, rep(1, length(vars))))
}

#' Activity against weather, session by session
#'
#' @description
#' One panel per weather variable: each point is one site's dawn or dusk
#' session, detections per recorded hour against the conditions in that
#' session, with a smooth per session. A first look for weather-driven
#' calling. It cannot separate a change in calling from a change in
#' detectability: wind and rain also mask calls and alter false positives.
#'
#' @param sessions Output of [density_by_session()].
#' @param weather Output of [weather_by_session()].
#' @param sites Sites to include. Default: all in `sessions`.
#' @param vars Weather columns to plot against.
#' @param date_range Optional two dates to restrict to (e.g. a breeding period).
#' @param log_rate Plot `log10(rate + 1)`? Default `TRUE`, as activity is
#'   heavily skewed.
#' @inheritParams plot_density
#'
#' @return A `ggplot` object.
#' @export
plot_activity_vs_weather <- function(sessions, weather, sites = NULL,
                                     vars = c("wind_speed_10m", "temperature_2m",
                                              "rain_day", "pressure_change_24h",
                                              "moon_illumination", "cloud_cover"),
                                     date_range = NULL, log_rate = TRUE,
                                     confidence = NULL, title = NULL, subtitle = NULL) {
  d <- sessions
  if (!is.null(sites)) d <- d[d$Site %in% sites, ]
  if (!is.null(date_range)) {
    date_range <- as.Date(date_range)
    d <- d[d$date >= date_range[1] & d$date <= date_range[2], ]
  }
  long <- d |>
    dplyr::inner_join(weather, by = c("date", "session")) |>
    tidyr::pivot_longer(dplyr::all_of(vars), names_to = "variable", values_to = "value") |>
    dplyr::mutate(variable = factor(weather_label(.data$variable),
                                    levels = weather_label(vars)),
                  y = if (log_rate) log10(.data$rate + 1) else .data$rate)
  session_cols <- stats::setNames(unname(birdnet_palette(2)), c("dawn", "dusk"))
  ylab <- density_y_label(confidence)
  ggplot2::ggplot(long, ggplot2::aes(.data$value, .data$y, colour = .data$session)) +
    ggplot2::geom_point(size = 1.4, alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                         linewidth = 0.8, span = 1) +
    ggplot2::scale_colour_manual(values = session_cols, name = NULL) +
    ggplot2::facet_wrap(~variable, scales = "free_x") +
    ggplot2::labs(x = NULL, y = if (log_rate) paste0("log10(", ylab, " + 1)") else ylab,
                  title = title, subtitle = subtitle) +
    birdnet_theme(base_size = 10) +
    # One session needs no legend; the title names it.
    ggplot2::theme(legend.position = if (length(sessions) > 1) "top" else "none",
                   legend.justification = "left")
}

#' Activity against one covariate, one panel per site
#'
#' @description
#' Each point is one site's session: detections per recorded hour against a
#' covariate such as a temperature anomaly. Coloured by session, with a fitted
#' curve per site and session from an overdispersed (quasi-Poisson) log-linear
#' model of counts with recorded hours as the offset, i.e. the same shape of
#' relationship a count model assumes. The y-axis is on a `log(1 + x)` scale so
#' sessions with no detections stay on the plot.
#'
#' @param data Sessions joined to the covariate: needs `Site`, `session`,
#'   `detections`, `hours`, `rate` and the column named by `x`. Typically
#'   [density_by_session()] joined to [session_anomaly()] output.
#' @param x Name of the covariate column.
#' @param x_label Axis label for the covariate.
#' @param sites Sites to show, in panel order. Default: all in `data`.
#' @param free_y Own y-scale per site? Default `TRUE`.
#' @param xref Draw a reference line at this x (e.g. 0 for an anomaly), or
#'   `NULL` for none. Default 0.
#' @param curves Optional data frame of fitted lines to draw instead of the
#'   per-site fits, with columns `Site`, `session`, `x` and `rate` (e.g.
#'   predictions from a mixed model), so the figure shows the model reported.
#' @inheritParams plot_density
#'
#' @return A `ggplot` object.
#' @export
plot_activity_vs_covariate <- function(data, x, x_label = x, sites = NULL,
                                       free_y = TRUE, xref = 0, curves = NULL, confidence = NULL,
                                       title = NULL, subtitle = NULL) {
  d <- data[!is.na(data[[x]]) & data$hours > 0, ]
  if (is.null(sites)) sites <- unique(as.character(d$Site))
  d <- d[d$Site %in% sites, ]
  d$Site <- factor(d$Site, levels = sites)
  d$x <- d[[x]]
  # Per site x session curve; skipped where a session has no detections at all.
  grid <- if (!is.null(curves)) {
    dplyr::mutate(curves[curves$Site %in% sites, ],
                  Site = factor(.data$Site, levels = sites))
  } else d |>
    dplyr::group_by(.data$Site, .data$session) |>
    dplyr::filter(sum(.data$detections) > 0) |>
    dplyr::group_modify(function(g, key) {
      fit <- stats::glm(detections ~ x + offset(log(hours)), family = stats::quasipoisson,
                        data = g)
      xs <- seq(min(g$x), max(g$x), length.out = 50)
      tibble::tibble(x = xs, rate = exp(stats::predict(
        fit, newdata = data.frame(x = xs, hours = 1))))
    }) |>
    dplyr::ungroup()
  sessions <- if (is.factor(d$session)) levels(droplevels(d$session)) else unique(d$session)
  session_cols <- stats::setNames(unname(birdnet_palette(length(sessions))), sessions)
  ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$rate, colour = .data$session)) +
    (if (!is.null(xref)) ggplot2::geom_vline(xintercept = xref, colour = birdnet_ink$axis,
                                             linewidth = 0.3)) +
    ggplot2::geom_point(size = 1.6, alpha = 0.55) +
    ggplot2::geom_line(data = grid, linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = session_cols, name = NULL) +
    ggplot2::scale_y_continuous(transform = "log1p",
                                breaks = c(0, 1, 3, 10, 30, 100, 300)) +
    ggplot2::facet_wrap(~Site, scales = if (free_y) "free_y" else "fixed", nrow = 1) +
    ggplot2::labs(x = x_label, y = density_y_label(confidence), title = title,
                  subtitle = subtitle) +
    birdnet_theme(base_size = 10) +
    # One session needs no legend; the title names it.
    ggplot2::theme(legend.position = if (length(sessions) > 1) "top" else "none",
                   legend.justification = "left")
}
