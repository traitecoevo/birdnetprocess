#' Plot daily (diel) activity heatmap
#'
#' @description
#' Folds every recording day onto a single 24-hour clock and draws a heatmap of
#' calling activity by time of day, one row per species and one panel per site.
#' Each species row is scaled to its own peak so the plot shows *when* a species
#' calls rather than how often. This is useful for comparing diel (within-day)
#' vocal patterns across species and sites.
#'
#' @param df A data frame of BirdNET results, ideally from `read_birdnet_folder`
#'   or `read_birdnet_sites`. Must contain `recording_window_time`, `Common Name`
#'   and `Confidence`. If a `Site` column is absent it is derived from the
#'   `file_name` prefix (everything before the `_YYYYMMDD_HHMMSS` stamp).
#' @param site Optional character vector of site names to include. `NULL`
#'   (default) keeps every site; each site becomes its own facet panel.
#' @param species Optional character vector of `Common Name` values to include.
#'   `NULL` (default) keeps every species.
#' @param confidence Numeric. Minimum BirdNET confidence (0-1) to retain.
#'   Defaults to 0.5 (below this is mostly false positives).
#' @param date Controls the time window. `NULL` (default) averages every day in
#'   the recording, folding them onto one 24-hour clock (each species row scaled
#'   to its own peak). Supply a single date (a `Date` or "YYYY-MM-DD" string) to
#'   instead plot just that one 24-hour period.
#' @param min_detections Integer. Drop any species with fewer than this many
#'   detections (after the confidence/site/date filters) so the figure is not
#'   cluttered by one-off detections. Counted per site, so a species is shown
#'   wherever it clears the threshold. Defaults to 1 (keep everything).
#' @param exclude Optional character vector of `Common Name` values to drop
#'   (e.g. environment/noise classes or unreliable species).
#' @param group_col Optional name of a column that classifies each species into
#'   a group (e.g. `"group"` with values like Birds / Insects). When supplied,
#'   the y-axis species labels are coloured by group.
#' @param group_colours Optional named vector mapping group -> colour (e.g.
#'   `c(Birds = "#3b82c4", Insects = "#e9a33b")`). Groups without an entry fall
#'   back to grey. Ignored unless `group_col` is set; defaults to a hue palette.
#' @param lat,lon Optional latitude/longitude. When both are supplied (and the
#'   `suncalc` package is installed) the representative sunrise/sunset for the
#'   deployment is shaded as night bands, and species are ordered into a
#'   nocturnal block then a diurnal block.
#' @param tz Time zone used to convert sunrise/sunset to local time of day.
#'   Defaults to "UTC".
#' @param bin_min Numeric. Time-of-day bin width in minutes. Defaults to 20.
#' @param layout Site panel arrangement: `"rows"` (default) stacks sites
#'   vertically, each panel repeating the species axis; `"columns"` places sites
#'   side-by-side sharing a single species axis (no repetition).
#' @param title,subtitle Optional plot title / subtitle. When `NULL` (default)
#'   sensible text is generated from `date` and whether night shading is drawn.
#' @param fill_palette Optional vector of colours for the activity fill, passed
#'   to [ggplot2::scale_fill_gradientn()], lightest first. Defaults to `NULL`,
#'   meaning the viridis "magma" ramp. Pass [birdnet_palette()]'s sequential
#'   colours to match the rest of a deployment report.
#'
#' @details
#' Times are folded by taking seconds since local midnight from
#' `recording_window_time` and carrying them on an arbitrary reference date so
#' the x-axis is a clean 24-hour time axis. Species are ordered by daylight
#' fraction (when sunrise/sunset are known) and then by the circular mean of
#' their call time, so the midnight wrap-around does not scatter night-active
#' species.
#'
#' @return A `ggplot` object. Save it with [ggplot2::ggsave()]; using
#'   `device = ragg::agg_png` is recommended for crisp text.
#'
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' d <- read_birdnet_sites(c("detections_SiteA", "detections_SiteB"))
#'
#' # One site
#' plot_daily_activity(d, site = "SiteA", lat = -33.4, lon = 151.4,
#'                     tz = "Australia/Sydney")
#'
#' # All sites at once, faceted
#' plot_daily_activity(d)
#'
#' # A single 24h period from a longer deployment, selected species only
#' plot_daily_activity(d, site = "SiteA", date = "2024-01-15",
#'                     species = c("Laughing Kookaburra", "Grey Fantail"))
#' }
plot_daily_activity <- function(df,
                                site = NULL,
                                species = NULL,
                                confidence = 0.5,
                                date = NULL,
                                min_detections = 1,
                                exclude = NULL,
                                group_col = NULL,
                                group_colours = NULL,
                                lat = NULL,
                                lon = NULL,
                                tz = "UTC",
                                bin_min = 20,
                                layout = c("rows", "columns"),
                                title = NULL,
                                subtitle = NULL,
                                fill_palette = NULL) {
  layout <- match.arg(layout)

  if (!"recording_window_time" %in% names(df)) {
    stop("`df` must contain a `recording_window_time` column ",
         "(use read_birdnet_folder() or read_birdnet_sites()).")
  }

  # Derive Site from the filename prefix when not already present.
  if (!"Site" %in% names(df)) {
    df$Site <- sub("_[0-9]{8}_[0-9]{6}.*$", "", df$file_name)
  }

  d <- df |>
    dplyr::filter(Confidence >= confidence, !is.na(recording_window_time))
  if (!is.null(species)) d <- dplyr::filter(d, `Common Name` %in% species)
  if (!is.null(exclude)) d <- dplyr::filter(d, !`Common Name` %in% exclude)
  if (!is.null(site))    d <- dplyr::filter(d, Site %in% site)
  if (!is.null(date)) {
    date <- as.Date(date)
    d <- dplyr::filter(d, as.Date(recording_window_time) == date)
  }
  if (nrow(d) == 0) stop("No detections remain after filtering.")

  # Drop sparse species (per site) so one-off detections don't clutter the plot.
  if (min_detections > 1) {
    d <- d |>
      dplyr::group_by(Site, `Common Name`) |>
      dplyr::filter(dplyr::n() >= min_detections) |>
      dplyr::ungroup()
    if (nrow(d) == 0)
      stop("No species clear min_detections = ", min_detections, ".")
  }

  # Fold every day onto one 24h clock: seconds since midnight carried on an
  # arbitrary reference date so the x-axis is a clean time-of-day axis.
  ref <- as.POSIXct("2000-01-01", tz = "UTC")
  lt <- as.POSIXlt(d$recording_window_time, tz = "UTC")
  d$tod_sec <- lt$hour * 3600 + lt$min * 60 + lt$sec
  d$tod <- ref + (floor(d$tod_sec / (bin_min * 60)) * bin_min * 60)

  # Count per site x species x time-bin, scale each row to its own peak.
  pd <- d |>
    dplyr::count(Site, `Common Name`, tod, name = "n") |>
    dplyr::group_by(Site, `Common Name`) |>
    dplyr::mutate(rel = n / max(n)) |>
    dplyr::ungroup()

  # Representative sunrise/sunset (median over the deployment), if coords given.
  have_sun <- !is.null(lat) && !is.null(lon) &&
    requireNamespace("suncalc", quietly = TRUE)
  if (have_sun) {
    dr <- sort(unique(as.Date(d$recording_window_time)))
    st <- suncalc::getSunlightTimes(date = dr, lat = lat, lon = lon,
                                    keep = c("sunrise", "sunset"))
    tod_of <- function(x) {
      p <- as.POSIXlt(lubridate::with_tz(x, tz))
      p$hour * 3600 + p$min * 60
    }
    sunrise_sec <- stats::median(tod_of(st$sunrise), na.rm = TRUE)
    sunset_sec  <- stats::median(tod_of(st$sunset),  na.rm = TRUE)
  }

  # Order species: nocturnal block then diurnal block (by daylight fraction when
  # sun times are known), and within each block by circular-mean call time.
  ord <- d |>
    dplyr::group_by(`Common Name`) |>
    dplyr::summarise(
      day_frac = if (have_sun) {
        mean(tod_sec >= sunrise_sec & tod_sec <= sunset_sec)
      } else 0,
      circ = atan2(mean(sin(2 * pi * tod_sec / 86400)),
                   mean(cos(2 * pi * tod_sec / 86400))),
      .groups = "drop")
  sp_levels <- ord |>
    dplyr::arrange(day_frac, circ) |>
    dplyr::pull(`Common Name`)
  pd$`Common Name` <- factor(pd$`Common Name`, levels = sp_levels)

  if (is.null(title)) {
    title <- if (is.null(date)) {
      "Daily calling patterns \u2014 all days folded onto a 24h clock"
    } else {
      paste0("Daily calling patterns \u2014 ", format(date, "%d %b %Y"))
    }
  }
  if (is.null(subtitle)) {
    subtitle <- "Each row scaled to its own peak"
    if (have_sun) subtitle <- paste0(subtitle, "; grey band = night")
  }

  p <- ggplot(pd, aes(tod, `Common Name`))

  if (have_sun) {
    night <- data.frame(
      xmin = c(ref, ref + sunset_sec),
      xmax = c(ref + sunrise_sec, ref + 86400))
    p <- p +
      geom_rect(data = night,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey25", alpha = 0.18, inherit.aes = FALSE)
  }

  p <- p + geom_tile(aes(fill = rel), height = 0.85)
  p <- p + if (is.null(fill_palette)) {
    scale_fill_viridis_c(option = "magma", name = "relative\nactivity",
                         begin = 0.05)
  } else {
    scale_fill_gradientn(colours = fill_palette, name = "relative\nactivity")
  }

  if (have_sun) {
    p <- p +
      geom_vline(xintercept = as.numeric(c(ref + sunrise_sec, ref + sunset_sec)),
                 linetype = "dashed", colour = "grey40", linewidth = 0.3)
    # Moon / sun / moon markers above the top row (pre-dawn, midday, late-night).
    nlev <- nlevels(pd$`Common Name`)
    icons <- data.frame(
      x = c(ref + sunrise_sec / 2,
            ref + mean(c(sunrise_sec, sunset_sec)),
            ref + mean(c(sunset_sec, 86400))),
      y = nlev + 0.9,
      lab = c("\U0001F319", "\u2600\ufe0f", "\U0001F319"))
    p <- p +
      geom_text(data = icons, aes(x = x, y = y, label = lab),
                inherit.aes = FALSE, size = 6)
  }

  top_pad <- if (have_sun) 1.5 else 0.6
  # Sites as stacked rows (each panel repeats the species axis) or side-by-side
  # columns (one shared species axis, no repetition).
  if (layout == "columns") {
    facet <- facet_grid(. ~ Site)
    strip_theme <- theme(strip.text.x = element_text(face = "bold"))
  } else {
    facet <- facet_grid(Site ~ ., switch = "y")
    strip_theme <- theme(strip.text.y.left = element_text(angle = 0, face = "bold"),
                         strip.placement = "outside")
  }
  p <- p +
    facet +
    scale_x_datetime(date_labels = "%H", date_breaks = "3 hours",
                     limits = c(ref, ref + 86400), expand = c(0, 0)) +
    scale_y_discrete(expand = expansion(add = c(0.6, top_pad))) +
    coord_cartesian(clip = "off") +
    labs(title = title, subtitle = subtitle, x = "time of day (h)", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    strip_theme

  # Optionally colour the y-axis species labels by a grouping column. Uses
  # `group_colours` (a named group -> colour vector) when supplied, otherwise a
  # default hue palette.
  if (!is.null(group_col) && group_col %in% names(d)) {
    lk <- d[!duplicated(d$`Common Name`), c("Common Name", group_col)]
    grp_lookup <- stats::setNames(lk[[group_col]], lk[["Common Name"]])
    grps <- unique(grp_lookup)
    if (!is.null(group_colours)) {
      pal <- group_colours
      missing <- setdiff(grps, names(pal))
      if (length(missing)) pal[missing] <- "grey50"
    } else {
      pal <- stats::setNames(scales::hue_pal()(length(grps)), grps)
    }
    p <- p + theme(
      axis.text.y = element_text(colour = unname(pal[grp_lookup[sp_levels]])))
  }

  p
}
