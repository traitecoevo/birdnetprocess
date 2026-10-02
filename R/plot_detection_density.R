# Views of detection density over time, one per question a reader may bring to a
# multi-site deployment. All take the output of detection_density() and share
# the same conventions, so they can be compared side by side:
#
#   * a recorded bin with no detections is a zero; a bin with no recording is
#     drawn as "no recording" and never as zero;
#   * sites are drawn in the order given by `sites` (default: as they appear);
#   * optional `marks` (Site, time) flag particular site-days, e.g. days that
#     were visited or had audio reviewed.
#
# Which view reads best is not known in advance - calling activity can be
# steady, seasonal or boom-and-bust - so they are offered as alternatives.

# Neutral fills for the two kinds of "nothing". Distinct from each other and
# from the lightest step of the sequential ramp.
density_zero_fill <- "#f1f0ec"
density_missing_fill <- "#d4d3cc"

#' Complete a daily density table onto a full site x day grid
#'
#' Internal. Days inside `date_range` with no row are "no recording" (rate NA).
#' Hourly tables are summed to days first.
#'
#' @noRd
density_daily_grid <- function(dens, sites = NULL, date_range = NULL) {
  daily <- dens |>
    dplyr::mutate(time = as.Date(.data$time)) |>
    dplyr::group_by(.data$Site, .data$time) |>
    dplyr::summarise(hours = sum(.data$hours), detections = sum(.data$detections),
                     .groups = "drop") |>
    dplyr::mutate(rate = .data$detections / .data$hours)
  if (is.null(sites)) sites <- unique(daily$Site)
  if (is.null(date_range)) date_range <- range(daily$time)
  date_range <- as.Date(date_range)
  grid <- tidyr::expand_grid(Site = sites,
                             time = seq(date_range[1], date_range[2], by = "day"))
  grid |>
    dplyr::left_join(daily, by = c("Site", "time")) |>
    dplyr::mutate(Site = factor(.data$Site, levels = sites),
                  recorded = !is.na(.data$hours))
}

density_y_label <- function(confidence) {
  if (is.null(confidence)) "detections per recorded hour"
  else paste0("detections ≥ ", confidence, " per recorded hour")
}

#' Detection density over time: four alternative views
#'
#' @description
#' Draw the output of [detection_density()] over time, one row per site.
#'
#' * `plot_density_bars()` - one small panel per site, a bar per day, shared
#'   y-axis. Reads exact values; good for a handful of sites.
#' * `plot_density_heatmap()` - sites as rows, days as columns, colour = density.
#'   The most compact; good for many sites and for seeing which days light up
#'   together.
#' * `plot_density_ridges()` - overlapping silhouettes, one per site, on a
#'   shared scale. Emphasises the shape and timing of bursts.
#' * `plot_density_cumulative()` - cumulative detections per site. Bursts show as
#'   steep steps; the final height ranks sites.
#'
#' For time-of-day patterns use [plot_density_actogram()].
#'
#' @param dens Output of [detection_density()] (`unit = "day"` or `"hour"`;
#'   hourly tables are summed to days). Should hold one species.
#' @param sites Site order, top to bottom. Default: order of appearance. Sites
#'   listed with no rows are drawn as never recording.
#' @param date_range Two dates bounding the x-axis. Days in range with no rows
#'   are drawn as "no recording". Default: the range of the data.
#' @param marks Optional data frame with columns `Site` and `time` (dates)
#'   marking particular site-days.
#' @param mark_label Legend text for `marks`.
#' @param confidence Threshold used in [detection_density()], for the axis label.
#' @param title,subtitle Plot title and subtitle.
#' @param ncol Panels per row for `plot_density_bars()`. Default 1, so every
#'   site shares one time axis.
#' @param overlap Ridge height, as a multiple of the row spacing, for the
#'   densest day. Default 2.5.
#' @param highlight For `plot_density_cumulative()`: sites to colour and label
#'   directly (at most four); the rest are drawn in grey. Default: the four with
#'   the most detections.
#'
#' @return A `ggplot` object.
#' @name plot_density
#' @examples
#' \dontrun{
#' dens <- detection_density(scores, confidence = 0.9)
#' plot_density_heatmap(dens, confidence = 0.9)
#' }
NULL

#' @rdname plot_density
#' @export
plot_density_bars <- function(dens, sites = NULL, date_range = NULL, marks = NULL,
                              mark_label = "marked day", confidence = NULL,
                              title = NULL, subtitle = NULL, ncol = 1) {
  g <- density_daily_grid(dens, sites, date_range)
  ymax <- max(g$rate, na.rm = TRUE)
  p <- ggplot2::ggplot(g, ggplot2::aes(.data$time, .data$rate)) +
    ggplot2::geom_tile(
      data = g[!g$recorded, ],
      ggplot2::aes(y = ymax / 2, height = Inf, fill = "no recording"),
      width = 1, inherit.aes = TRUE
    ) +
    ggplot2::geom_col(data = g[g$recorded, ], width = 0.8,
                      fill = birdnet_palette(1)) +
    ggplot2::scale_fill_manual(values = c("no recording" = density_missing_fill),
                               name = NULL)
  p <- add_density_marks(p, marks, g, y = -ymax * 0.06, mark_label)
  p +
    ggplot2::facet_wrap(~Site, ncol = ncol, strip.position = "left") +
    ggplot2::scale_x_date(expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::labs(x = NULL, y = density_y_label(confidence),
                  title = title, subtitle = subtitle) +
    birdnet_theme(base_size = 10) +
    ggplot2::theme(
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1),
      strip.placement = "outside",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(2, "pt"),
      legend.position = "top", legend.justification = "left"
    )
}

#' @rdname plot_density
#' @export
plot_density_heatmap <- function(dens, sites = NULL, date_range = NULL, marks = NULL,
                                 mark_label = "marked day", confidence = NULL,
                                 title = NULL, subtitle = NULL) {
  g <- density_daily_grid(dens, sites, date_range)
  g$Site <- factor(g$Site, levels = rev(levels(g$Site)))  # first site on top
  pos <- g[g$recorded & g$rate > 0, ]
  nothing <- g[!g$recorded | g$rate == 0, ]
  nothing$state <- ifelse(nothing$recorded, "recorded, none", "no recording")
  p <- ggplot2::ggplot(g, ggplot2::aes(.data$time, .data$Site)) +
    ggplot2::geom_tile(data = nothing, ggplot2::aes(colour = .data$state),
                       fill = NA, width = 0, height = 0) +  # legend carrier only
    ggplot2::geom_tile(
      data = nothing,
      fill = ifelse(nothing$recorded, density_zero_fill, density_missing_fill),
      width = 0.92, height = 0.86
    ) +
    ggplot2::geom_tile(data = pos, ggplot2::aes(fill = .data$rate),
                       width = 0.92, height = 0.86) +
    ggplot2::scale_fill_gradientn(
      colours = birdnet_sequential_colours, trans = "sqrt",
      name = density_y_label(confidence)
    ) +
    ggplot2::scale_colour_manual(
      values = c("recorded, none" = density_zero_fill,
                 "no recording" = density_missing_fill),
      name = NULL,
      guide = ggplot2::guide_legend(override.aes = list(
        fill = c(density_missing_fill, density_zero_fill), width = 1, height = 1,
        colour = birdnet_ink$axis))
    )
  if (!is.null(marks)) {
    m <- marks |>
      dplyr::mutate(time = as.Date(.data$time)) |>
      dplyr::semi_join(g, by = c("Site", "time")) |>
      dplyr::mutate(Site = factor(.data$Site, levels = levels(g$Site)))
    p <- p + ggplot2::geom_point(
      data = m, ggplot2::aes(shape = mark_label), colour = birdnet_ink$primary,
      size = 1.1, inherit.aes = TRUE
    ) + ggplot2::scale_shape_manual(values = stats::setNames(16, mark_label), name = NULL)
  }
  p +
    ggplot2::scale_x_date(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    birdnet_theme(base_size = 10) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   legend.position = "top", legend.justification = "left",
                   legend.key.width = ggplot2::unit(28, "pt"))
}

#' @rdname plot_density
#' @export
plot_density_ridges <- function(dens, sites = NULL, date_range = NULL,
                                confidence = NULL, title = NULL, subtitle = NULL,
                                overlap = 2.5) {
  g <- density_daily_grid(dens, sites, date_range)
  n <- nlevels(g$Site)
  g$base <- n - as.integer(g$Site)  # first site at the top
  scale <- overlap / max(g$rate, na.rm = TRUE)
  g$top <- g$base + dplyr::coalesce(g$rate, 0) * scale
  # Runs of "no recording" drawn as a grey band along the baseline.
  gaps <- g[!g$recorded, ]
  p <- ggplot2::ggplot(g)
  if (nrow(gaps)) {
    p <- p + ggplot2::geom_tile(
      data = gaps, ggplot2::aes(.data$time, .data$base + 0.08),
      height = 0.16, width = 1, fill = density_missing_fill
    )
  }
  # A ridge rises over the rows above it, so lower rows are in front: draw from
  # the top row down, letting each later ridge cover the ones behind it.
  for (lev in levels(g$Site)) {
    d <- g[g$Site == lev, ]
    p <- p +
      ggplot2::geom_ribbon(data = d, ggplot2::aes(.data$time, ymin = .data$base,
                                                  ymax = .data$top),
                           fill = birdnet_ink$surface, colour = NA) +
      ggplot2::geom_ribbon(data = d, ggplot2::aes(.data$time, ymin = .data$base,
                                                  ymax = .data$top),
                           fill = birdnet_palette(1), alpha = 0.35, colour = NA) +
      ggplot2::geom_line(data = d, ggplot2::aes(.data$time, .data$top),
                         colour = birdnet_palette(1), linewidth = 0.5)
  }
  p +
    ggplot2::scale_y_continuous(breaks = seq_len(n) - 1, labels = rev(levels(g$Site)),
                                expand = ggplot2::expansion(add = c(0.2, overlap))) +
    ggplot2::scale_x_date(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, title = title,
                  subtitle = subtitle %||% paste0(
                    "Height: ", density_y_label(confidence),
                    " (tallest = ", signif(max(g$rate, na.rm = TRUE), 2),
                    "). Grey baseline: no recording.")) +
    birdnet_theme(base_size = 10) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' @rdname plot_density
#' @export
plot_density_cumulative <- function(dens, sites = NULL, date_range = NULL,
                                    highlight = NULL, confidence = NULL,
                                    title = NULL, subtitle = NULL) {
  g <- density_daily_grid(dens, sites, date_range) |>
    dplyr::group_by(.data$Site) |>
    dplyr::arrange(.data$time, .by_group = TRUE) |>
    dplyr::mutate(cum = cumsum(dplyr::coalesce(.data$detections, 0L))) |>
    dplyr::ungroup()
  totals <- g |> dplyr::group_by(.data$Site) |>
    dplyr::summarise(total = max(.data$cum), .groups = "drop")
  if (is.null(highlight)) {
    highlight <- as.character(totals$Site[order(-totals$total)][seq_len(min(4, sum(totals$total > 0)))])
  }
  highlight <- utils::head(highlight, 4)
  pal <- stats::setNames(unname(birdnet_palette(length(highlight))), highlight)
  g$hl <- as.character(g$Site) %in% highlight
  ends <- g |> dplyr::filter(.data$hl) |> dplyr::group_by(.data$Site) |>
    dplyr::slice_max(.data$time, n = 1) |> dplyr::ungroup()
  rest <- setdiff(levels(g$Site), highlight)
  ggplot2::ggplot(g, ggplot2::aes(.data$time, .data$cum, group = .data$Site)) +
    ggplot2::geom_step(data = g[!g$hl, ], colour = birdnet_ink$muted,
                       linewidth = 0.4) +
    ggplot2::geom_step(data = g[g$hl, ], ggplot2::aes(colour = .data$Site),
                       linewidth = 0.8) +
    ggplot2::geom_text(data = ends, ggplot2::aes(label = .data$Site),
                       hjust = -0.1, size = 3, colour = birdnet_ink$primary) +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(
      x = NULL,
      y = if (is.null(confidence)) "cumulative detections"
          else paste0("cumulative detections ≥ ", confidence),
      title = title,
      subtitle = subtitle %||% if (length(rest))
        paste0("Grey: ", paste(rest, collapse = ", ")) else NULL
    ) +
    birdnet_theme(base_size = 10) +
    ggplot2::theme(legend.position = "none")
}

#' Time-of-day activity across a season (actogram)
#'
#' @description
#' One panel per site: date along the x-axis, hour of day up the y-axis, colour
#' = detections per recorded hour in that hour. Shows whether calling sits at
#' dawn or dusk and how that shifts through a burst. Hours never recorded are
#' left blank. Only hours that were ever recorded get a row, so a recording
#' schedule such as dawn and dusk only stays compact.
#'
#' @param dens Output of [detection_density()] with `unit = "hour"`.
#' @param sites Sites to draw, in order. Default: every site with a detection.
#' @inheritParams plot_density
#' @param ncol Panels per row. Default 2.
#'
#' @return A `ggplot` object.
#' @export
plot_density_actogram <- function(dens, sites = NULL, confidence = NULL,
                                  title = NULL, subtitle = NULL, ncol = 2) {
  if (!inherits(dens$time, "POSIXct")) {
    stop("plot_density_actogram() needs detection_density(..., unit = \"hour\").",
         call. = FALSE)
  }
  if (is.null(sites)) {
    sites <- unique(dens$Site[dens$detections > 0])
  }
  d <- dens[dens$Site %in% sites, ] |>
    dplyr::mutate(Site = factor(.data$Site, levels = sites),
                  day = as.Date(.data$time),
                  hour = as.integer(format(.data$time, "%H")))
  # Only hours that were ever recorded get a row, so a schedule such as dawn
  # and dusk only does not leave most of each panel empty.
  hours_recorded <- sort(unique(d$hour))
  d$hour <- factor(d$hour, levels = hours_recorded,
                   labels = sprintf("%02d:00", hours_recorded))
  pos <- d[d$detections > 0, ]
  ggplot2::ggplot(d, ggplot2::aes(.data$day, .data$hour)) +
    ggplot2::geom_tile(fill = density_zero_fill, width = 1, height = 1) +
    ggplot2::geom_tile(data = pos, ggplot2::aes(fill = .data$rate),
                       width = 1, height = 1) +
    ggplot2::scale_fill_gradientn(colours = birdnet_sequential_colours,
                                  trans = "sqrt",
                                  name = density_y_label(confidence)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_x_date(expand = c(0, 0)) +
    ggplot2::facet_wrap(~Site, ncol = ncol) +
    ggplot2::labs(x = NULL, y = "hour of day", title = title,
                  subtitle = subtitle %||% "Rows: hours with any recording. Pale: recorded, no detections. Blank: not recorded.") +
    birdnet_theme(base_size = 10) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   legend.position = "top", legend.justification = "left",
                   legend.key.width = ggplot2::unit(28, "pt"))
}

# Small marker row under the bars for flagged site-days.
add_density_marks <- function(p, marks, g, y, mark_label) {
  if (is.null(marks)) return(p)
  m <- marks |>
    dplyr::mutate(time = as.Date(.data$time)) |>
    dplyr::semi_join(g, by = c("Site", "time")) |>
    dplyr::mutate(Site = factor(.data$Site, levels = levels(g$Site)), y = y)
  p +
    ggplot2::geom_point(data = m, ggplot2::aes(.data$time, .data$y, shape = mark_label),
                        colour = birdnet_palette(2)[[2]], size = 1.4,
                        inherit.aes = FALSE) +
    ggplot2::scale_shape_manual(values = stats::setNames(17, mark_label), name = NULL)
}
