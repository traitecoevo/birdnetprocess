#' Map of sites sized by a log-transformed total
#'
#' @description
#' One point per site at its coordinates, sized by `log10(total + 1)` so that
#' sites differing by orders of magnitude stay readable side by side. Sites
#' with a total of zero and sites that did not record are drawn with their own
#' symbols rather than as tiny points, so "none" and "no data" never look like
#' "a little".
#'
#' @param totals Data frame with columns `Site`, `latitude`, `longitude` and the
#'   column named by `value`. An optional `hours` column marks sites with no
#'   recording (`0` or `NA`).
#' @param value Name of the column to size points by. Default `"detections"`.
#' @param value_label Legend title for point size.
#' @param labels Label each site with its name and total? Default `TRUE`.
#'   Uses `ggrepel` when installed so labels avoid each other.
#' @param layers Optional list of ggplot layers drawn beneath the points, e.g.
#'   `geom_sf()` layers of rivers or boundaries (these need the `sf` package).
#' @param scale_bar Add a scale bar? Default `TRUE`; needs `ggspatial`.
#' @param title,subtitle Plot title and subtitle.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' totals <- dplyr::tibble(Site = c("A", "B", "C"), latitude = c(-31, -31.01, -31.02),
#'                         longitude = c(141.8, 141.81, 141.82),
#'                         detections = c(1500, 12, 0))
#' plot_site_map(totals)
#' }
plot_site_map <- function(totals, value = "detections",
                          value_label = "detections",
                          labels = TRUE, layers = NULL, scale_bar = TRUE,
                          title = NULL, subtitle = NULL) {
  missing <- setdiff(c("Site", "latitude", "longitude", value), names(totals))
  if (length(missing)) {
    stop("`totals` is missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  d <- totals
  d$value <- d[[value]]
  recorded <- if ("hours" %in% names(d)) !is.na(d$hours) & d$hours > 0 else TRUE
  d$state <- ifelse(!recorded, "not recording",
                    ifelse(d$value > 0, "detected", "none detected"))
  d$size <- log10(d$value + 1)
  d$label <- ifelse(d$state == "not recording", paste0(d$Site, " (no recording)"),
                    paste0(d$Site, "  ", format(d$value, big.mark = ",", trim = TRUE)))

  pos <- d[d$state == "detected", ]
  zero <- d[d$state != "detected", ]
  top <- max(pos$value, 1)
  breaks <- 10^(0:ceiling(log10(top)))
  breaks <- breaks[breaks <= top * 1.5]

  p <- ggplot2::ggplot()
  for (l in layers) p <- p + l
  p <- p +
    ggplot2::geom_point(
      data = zero, ggplot2::aes(.data$longitude, .data$latitude, shape = .data$state),
      colour = birdnet_ink$muted, size = 2.2, stroke = 0.7
    ) +
    ggplot2::geom_point(
      data = pos, ggplot2::aes(.data$longitude, .data$latitude, size = .data$size),
      fill = birdnet_palette(1), colour = birdnet_ink$surface, shape = 21,
      stroke = 0.6, alpha = 0.9
    ) +
    ggplot2::scale_size_area(
      max_size = 11, breaks = log10(breaks + 1),
      labels = format(breaks, big.mark = ",", trim = TRUE),
      name = paste0(value_label, "\n(log scale)")
    ) +
    ggplot2::scale_shape_manual(
      values = c("none detected" = 1, "not recording" = 4), name = NULL
    )

  if (labels) {
    lab_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
      ggrepel::geom_text_repel(
        data = d, ggplot2::aes(.data$longitude, .data$latitude, label = .data$label),
        size = 3, colour = birdnet_ink$primary, min.segment.length = 0.3,
        segment.colour = birdnet_ink$axis, box.padding = 0.5, seed = 1
      )
    } else {
      ggplot2::geom_text(
        data = d, ggplot2::aes(.data$longitude, .data$latitude, label = .data$label),
        size = 3, colour = birdnet_ink$primary, hjust = -0.15
      )
    }
    p <- p + lab_layer
  }

  # Equal-distance axes. coord_sf when sf is available, so geom_sf context
  # layers line up; otherwise a plain aspect-corrected cartesian map.
  pad <- 0.08 * max(diff(range(d$longitude)), diff(range(d$latitude)), 0.005)
  xlim <- range(d$longitude) + c(-pad, pad)
  ylim <- range(d$latitude) + c(-pad, pad)
  if (requireNamespace("sf", quietly = TRUE)) {
    p <- p + ggplot2::coord_sf(xlim = xlim, ylim = ylim, crs = 4326,
                               default_crs = 4326, expand = FALSE)
    if (scale_bar && requireNamespace("ggspatial", quietly = TRUE)) {
      p <- p + ggspatial::annotation_scale(location = "bl", width_hint = 0.2,
                                           bar_cols = c(birdnet_ink$secondary,
                                                        birdnet_ink$surface),
                                           text_col = birdnet_ink$secondary,
                                           line_col = birdnet_ink$secondary)
    }
  } else {
    p <- p + ggplot2::coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE)
  }

  p +
    ggplot2::labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    birdnet_theme(base_size = 10) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = birdnet_ink$grid,
                                                            linewidth = 0.2),
                   legend.position = "right")
}
