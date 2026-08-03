# One palette for every figure in a deployment report, so the report reads as a
# single system rather than a pile of plots. The hues and their order are not
# cosmetic: the ordering is what keeps adjacent series distinguishable under
# colour-vision deficiency, and it was chosen by validation rather than by eye.
#
# Report figures are rendered as PNGs on a light surface in both page themes,
# because a raster can't restyle itself for a dark page. The palette below is
# therefore the light-surface set, validated against surface #fcfcfb.

# Categorical slots, in order. Safe for series compared side by side
# (bars, stacked segments, lines) - worst adjacent pair clears the CVD floor.
birdnet_series_colours <- c(
  blue = "#2a78d6",
  orange = "#eb6834",
  aqua = "#1baf7a",
  yellow = "#eda100",
  magenta = "#e87ba4",
  green = "#008300",
  violet = "#4a3aa7",
  red = "#e34948"
)

# When every pair may be compared at once - text labels scattered down a plot,
# points in a cloud - the eight above no longer separate. These four do.
birdnet_allpairs_colours <- c(
  blue = "#2a78d6",
  orange = "#eb6834",
  aqua = "#1baf7a",
  violet = "#4a3aa7"
)

# Single-hue ramp for magnitude: light = near zero, dark = peak.
birdnet_sequential_colours <- c(
  "#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b"
)

# Chart chrome. Deliberately recessive: the data carries the colour, the
# furniture stays out of the way.
birdnet_ink <- list(
  surface = "#fcfcfb",
  primary = "#0b0b0b",
  secondary = "#52514e",
  muted = "#898781",
  grid = "#e1e0d9",
  axis = "#c3c2b7",
  residual = "#898781"  # for an "Other"/leftover group, which has no identity
)

#' Categorical colours for plotting
#'
#' @description
#' Returns `n` colours from the package's categorical palette, in a fixed
#' order. The order is the point: it was chosen so that neighbouring series
#' stay distinguishable to readers with colour-vision deficiency, so take the
#' colours from the front rather than picking favourites.
#'
#' @details
#' Two orders are available. The default suits series that are read against
#' their neighbours - bars, stacked areas, lines. `all_pairs = TRUE` returns a
#' smaller set that holds up when *every* pair may be compared at once, such as
#' coloured species labels scattered down a figure; past four such colours, no
#' ordering of this palette separates reliably, so fold the rest into a
#' residual group instead of asking for more.
#'
#' @param n Number of colours. Defaults to the whole palette.
#' @param all_pairs Use the stricter all-pairs-safe set? Default `FALSE`.
#'
#' @return A character vector of hex colours, named by hue.
#' @export
#' @examples
#' birdnet_palette(3)
#' birdnet_palette(4, all_pairs = TRUE)
birdnet_palette <- function(n = NULL, all_pairs = FALSE) {
  pal <- if (all_pairs) birdnet_allpairs_colours else birdnet_series_colours
  if (is.null(n)) {
    return(pal)
  }
  if (n > length(pal)) {
    warning("Asked for ", n, " colours but only ", length(pal),
            " are safe to distinguish; recycling. Consider grouping the ",
            "smaller categories together instead.", call. = FALSE)
    return(rep_len(pal, n))
  }
  pal[seq_len(n)]
}

#' A shared ggplot theme for report figures
#'
#' @description
#' Recessive chart furniture - hairline grid, muted axis text, no panel border -
#' so that every figure in a report looks like it came from the same place.
#'
#' @param base_size Base font size. Default `12`.
#'
#' @return A ggplot2 theme object.
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
#' plot_species_counts(d) + birdnet_theme()
#' }
birdnet_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        colour = birdnet_ink$primary, face = "bold",
        size = ggplot2::rel(1.05), margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = birdnet_ink$secondary, size = ggplot2::rel(0.85),
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        colour = birdnet_ink$muted, size = ggplot2::rel(0.75)
      ),
      axis.title = ggplot2::element_text(colour = birdnet_ink$secondary,
                                         size = ggplot2::rel(0.85)),
      axis.text = ggplot2::element_text(colour = birdnet_ink$muted,
                                        size = ggplot2::rel(0.8)),
      panel.grid.major = ggplot2::element_line(colour = birdnet_ink$grid,
                                               linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(colour = birdnet_ink$secondary,
                                         face = "bold",
                                         size = ggplot2::rel(0.85)),
      legend.title = ggplot2::element_text(colour = birdnet_ink$secondary,
                                           size = ggplot2::rel(0.8)),
      legend.text = ggplot2::element_text(colour = birdnet_ink$secondary,
                                          size = ggplot2::rel(0.8)),
      plot.background = ggplot2::element_rect(fill = birdnet_ink$surface,
                                              colour = NA),
      panel.background = ggplot2::element_rect(fill = birdnet_ink$surface,
                                               colour = NA)
    )
}
