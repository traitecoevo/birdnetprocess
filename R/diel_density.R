#' Average diel pattern around sunrise and sunset
#'
#' @description
#' Folds every recording day onto time relative to sunrise and sunset, and
#' measures detections per recorded hour in short bins. Pools all days, so it
#' shows the *average* shape of calling around dawn and dusk, with the drift of
#' sunrise through a season removed. Suits deployments that record windows set
#' relative to the sun (e.g. one hour either side of sunrise and sunset).
#'
#' @details
#' Each window is assigned to whichever of that day's sunrise or sunset is
#' nearer, and its offset from that event is binned. Sun times come from the
#' `suncalc` package using each site's own coordinates.
#'
#' **Clock time zone.** `tz` must be the zone the recorder clocks were set to,
#' which is not always local civil time: a recorder in one zone can be
#' programmed in another. Check it against the recording schedule before
#' trusting the offsets.
#'
#' **Effort.** As in [detection_density()], effort is counted from the windows
#' themselves, so `df` must be an all-window table (every window has a row,
#' whatever its score).
#'
#' @param df Detections from [read_birdnet_folder()] etc., all-window.
#' @param sites Data frame with columns `Site`, `latitude` and `longitude`.
#'   Every site in `df` needs a row.
#' @param tz Time zone of the recorder clocks, e.g. `"Australia/Sydney"`.
#' @param confidence Minimum confidence (0-1) for a detection. Default 0.5.
#' @param bin_min Bin width in minutes. Default 5.
#' @param species Optional `Common Name` values to keep.
#' @param window_s Window length in seconds. Default: taken from the data.
#' @param by_date Keep each date separate instead of pooling days? Default
#'   `FALSE`. With `TRUE` the result has a `date` column, for relating
#'   activity in a sun-relative window to that day's conditions.
#'
#' @return A tibble: `Site`, (`date` if `by_date`), `anchor` (`"sunrise"` or `"sunset"`), `offset_min`
#'   (bin start, minutes relative to the event), `days` (days contributing),
#'   `hours` (recorded), `detections` and `rate` (detections per recorded hour).
#' @export
#' @examples
#' \dontrun{
#' coords <- data.frame(Site = "SiteA", latitude = -33.5, longitude = 151.2)
#' diel <- diel_density(scores, coords, tz = "Australia/Sydney", confidence = 0.9)
#' plot_diel_density(diel, confidence = 0.9)
#' }
diel_density <- function(df, sites, tz, confidence = 0.5, bin_min = 5,
                         species = NULL, window_s = NULL, by_date = FALSE) {
  if (!requireNamespace("suncalc", quietly = TRUE)) {
    stop("diel_density() needs the 'suncalc' package.", call. = FALSE)
  }
  missing <- setdiff(c("Site", "latitude", "longitude"), names(sites))
  if (length(missing)) {
    stop("`sites` is missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!"Site" %in% names(df)) df$Site <- site_from_file(df$file_name)
  if (!is.null(species)) df <- df[df$`Common Name` %in% species, ]
  unknown <- setdiff(unique(df$Site), sites$Site)
  if (length(unknown)) {
    stop("No coordinates for site(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  if (min(df$Confidence, na.rm = TRUE) > 0.05) {
    warning("`df` looks thresholded rather than all-window, so effort will be ",
            "undercounted.", call. = FALSE)
  }
  if (is.null(window_s)) {
    window_s <- if ("end_time_s" %in% names(df)) {
      stats::median(df$end_time_s - df$begin_time_s, na.rm = TRUE)
    } else {
      3
    }
  }

  # Window times are recorder clock times carried as UTC; sun times are put on
  # the same footing (clock time in `tz`, labelled UTC) so they subtract cleanly.
  df$clock <- detection_time(df)
  df$date <- as.Date(df$clock)
  sun <- dplyr::distinct(df, .data$Site, .data$date) |>
    dplyr::left_join(sites[, c("Site", "latitude", "longitude")], by = "Site")
  st <- suncalc::getSunlightTimes(
    data = data.frame(date = sun$date, lat = sun$latitude, lon = sun$longitude),
    keep = c("sunrise", "sunset")
  )
  as_clock <- function(x) lubridate::force_tz(lubridate::with_tz(x, tz), "UTC")
  sun$sunrise <- as_clock(st$sunrise)
  sun$sunset <- as_clock(st$sunset)

  df |>
    dplyr::distinct(.data$Site, .data$date, .data$clock, .data$file_name,
                    .data$begin_time_s, .keep_all = TRUE) |>
    dplyr::left_join(sun[, c("Site", "date", "sunrise", "sunset")],
                     by = c("Site", "date")) |>
    dplyr::mutate(
      to_rise = as.numeric(difftime(.data$clock, .data$sunrise, units = "mins")),
      to_set = as.numeric(difftime(.data$clock, .data$sunset, units = "mins")),
      anchor = ifelse(abs(.data$to_rise) <= abs(.data$to_set), "sunrise", "sunset"),
      offset = ifelse(.data$anchor == "sunrise", .data$to_rise, .data$to_set),
      offset_min = floor(.data$offset / bin_min) * bin_min,
      hit = .data$Confidence >= confidence
    ) |>
    dplyr::group_by(.data$Site, !!!(if (by_date) list(rlang::sym("date")) else list()),
                    .data$anchor, .data$offset_min) |>
    dplyr::summarise(
      days = dplyr::n_distinct(.data$date),
      hours = dplyr::n() * window_s / 3600,
      detections = sum(.data$hit),
      .groups = "drop"
    ) |>
    dplyr::mutate(rate = .data$detections / .data$hours,
                  anchor = factor(.data$anchor, levels = c("sunrise", "sunset")))
}

# Background for the sun-down side of each panel: lighter than the "no
# recording" grey used elsewhere, so it reads as context, not as missing data.
diel_dark_fill <- "#ebeae5"

#' Plot the average diel pattern around sunrise and sunset
#'
#' @description
#' One row per site, a sunrise panel and a sunset panel, bars of detections per
#' recorded hour against minutes from the event. A vertical line marks the
#' event itself.
#'
#' @param diel Output of [diel_density()].
#' @param sites Sites to draw, in order. Default: every site with a detection.
#' @param min_days Drop bins recorded on fewer than this many days, which are
#'   noisy edges where the schedule drifted. Default 5.
#' @param free_y Give each site its own y-scale? Default `TRUE`, because the
#'   point is the shape of each site's pattern; strip labels carry each site's
#'   total so a tall bar on a sparse site is not mistaken for a strong one.
#' @param shade_dark Shade the side of each panel where the sun is down
#'   (before sunrise, after sunset)? Default `TRUE`.
#' @param confidence Threshold used, for the axis label.
#' @param title,subtitle Plot title and subtitle.
#'
#' @return A `ggplot` object.
#' @export
plot_diel_density <- function(diel, sites = NULL, min_days = 5, free_y = TRUE,
                              shade_dark = TRUE, confidence = NULL,
                              title = NULL, subtitle = NULL) {
  if (is.null(sites)) {
    sites <- unique(as.character(diel$Site[diel$detections > 0]))
  }
  d <- diel[diel$Site %in% sites & diel$days >= min_days, ]
  totals <- d |> dplyr::group_by(.data$Site) |>
    dplyr::summarise(n = sum(.data$detections), .groups = "drop")
  lab <- stats::setNames(paste0(totals$Site, "\n", format(totals$n, big.mark = ","),
                                " detections"), totals$Site)
  d$Site <- factor(d$Site, levels = sites, labels = lab[sites])
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$offset_min, .data$rate))
  if (shade_dark) {
    # Sun below the horizon: left of sunrise, right of sunset.
    dark <- data.frame(
      anchor = factor(c("sunrise", "sunset"), levels = levels(d$anchor)),
      xmin = c(-Inf, 0), xmax = c(0, Inf)
    )
    p <- p + ggplot2::geom_rect(
      data = dark, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                ymin = -Inf, ymax = Inf),
      fill = diel_dark_fill, inherit.aes = FALSE
    )
  }
  p +
    ggplot2::geom_col(fill = birdnet_palette(1), width = diff(sort(unique(d$offset_min)))[1] * 0.85,
                      just = 0) +
    ggplot2::geom_vline(xintercept = 0, colour = birdnet_ink$secondary,
                        linewidth = 0.4, linetype = "dashed") +
    ggplot2::facet_grid(Site ~ anchor, scales = if (free_y) "free_y" else "fixed",
                        switch = "y") +
    ggplot2::scale_x_continuous(breaks = seq(-120, 120, 30)) +
    ggplot2::labs(
      x = "minutes from sunrise / sunset", y = density_y_label(confidence),
      title = title,
      subtitle = subtitle %||% paste0(
        "All days pooled. Dashed line: the event itself.",
        if (shade_dark) " Grey: sun below the horizon." else "",
        if (free_y) " Each site has its own y-scale." else "")
    ) +
    birdnet_theme(base_size = 10) +
    ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1),
                   strip.placement = "outside",
                   panel.grid.major.x = ggplot2::element_blank())
}
