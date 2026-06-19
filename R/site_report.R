#' Build a per-site report and plot set from BirdNET detections
#'
#' @description
#' A convenience wrapper that produces a standard set of outputs for a single
#' recording site: optionally range-filters the detections (removing geographic
#' false positives), summarises them, and builds two plots — a species-counts
#' bar chart and a top-species activity plot with day/night shading. When
#' `output_dir` is supplied, the plots and a species-filter report are written
#' to disk.
#'
#' @details
#' The work is delegated to existing package functions:
#' \code{\link{filter_by_range}}, \code{\link{summarise_detections}},
#' \code{\link{plot_species_counts}}, and \code{\link{plot_top_species}}. If
#' `abundance_raster` is `NULL`, no range filtering is performed and all
#' detections are reported.
#'
#' @param data A tibble of BirdNET detections (e.g. from
#'        \code{\link{read_birdnet_folder}}).
#' @param latitude,longitude Numeric. The recorder location (decimal degrees,
#'        WGS84). Used both for range filtering and for day/night shading.
#' @param abundance_raster Optional `terra` `SpatRaster` (or path) of per-species
#'        abundance layers. When supplied, detections are range-filtered via
#'        \code{\link{filter_by_range}}.
#' @param confidence Numeric. Minimum confidence for the summary and plots.
#'        Default `0.5`.
#' @param output_dir Optional directory to write the report CSV and plot PNGs
#'        to. Created if it does not exist. If `NULL`, nothing is written.
#' @param site_name Optional label for the site (used in messages / file names).
#' @param n_top_species Number of top species for the activity plots. Default `10`.
#' @param tz Time zone for day/night shading. Default `"Australia/Sydney"`.
#' @param start_after,end_before Optional deployment-window bounds. Detections
#'        with `recording_window_time` before `start_after` or after
#'        `end_before` are dropped. Useful when the recorder was running before
#'        it was placed at the site (e.g. test recordings made elsewhere). May
#'        be a `POSIXct`, a `Date`, or a string such as `"2026-05-26"` (parsed
#'        in UTC, matching the times produced by \code{\link{read_birdnet_file}}).
#' @param min_abundance Passed to \code{\link{filter_by_range}}. Default `0`.
#' @param name_overrides Passed to \code{\link{filter_by_range}}.
#' @param keep_species Passed to \code{\link{filter_by_range}}: common names to
#'        always keep regardless of modelled range (e.g. nomadic waterbirds).
#'
#' @return Invisibly, a list with elements `report` (per-species filter report,
#'         or `NULL` if no raster), `summary` (the
#'         \code{\link{summarise_detections}} tibble), `plots` (named list of
#'         ggplot objects), `data_filtered` (the filtered detections), and
#'         `output_dir`.
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#' r <- terra::rast("nsw_abundance_stack_3km.tif")
#' d <- read_birdnet_folder("wilddeserts")
#' res <- site_report(
#'   d,
#'   latitude = -29.229286, longitude = 141.286856,
#'   abundance_raster = r, tz = "Australia/Sydney",
#'   output_dir = "wilddeserts_plots/filtered"
#' )
#' res$report
#' }
site_report <- function(data,
                        latitude,
                        longitude,
                        abundance_raster = NULL,
                        confidence = 0.5,
                        output_dir = NULL,
                        site_name = NULL,
                        n_top_species = 10,
                        tz = "Australia/Sydney",
                        start_after = NULL,
                        end_before = NULL,
                        min_abundance = 0,
                        name_overrides = NULL,
                        keep_species = NULL) {
  # 1. Deployment time window (optional): drop detections outside the period the
  # recorder was actually deployed at this site (e.g. recordings made before
  # the unit was moved into place).
  data <- trim_to_window(data, start_after = start_after, end_before = end_before)

  # 2. Range filter (optional)
  if (!is.null(abundance_raster)) {
    data_filtered <- filter_by_range(
      data,
      abundance_raster = abundance_raster,
      latitude = latitude,
      longitude = longitude,
      min_abundance = min_abundance,
      name_overrides = name_overrides,
      keep_species = keep_species
    )
    report <- attr(data_filtered, "range_report")
  } else {
    data_filtered <- data
    report <- NULL
  }

  # 3. Summary
  summary <- summarise_detections(data_filtered, confidence = confidence)

  # 4. Plots (reuse existing plotting functions; skip any that error)
  build <- function(expr) tryCatch(expr, error = function(e) {
    warning("Skipping a plot: ", conditionMessage(e))
    NULL
  })
  plots <- list(
    counts = build(plot_species_counts(data_filtered, confidence = confidence)),
    daynight = build(plot_top_species(
      data_filtered,
      n_top_species = n_top_species,
      confidence_threshold = confidence,
      latitude = latitude, longitude = longitude, tz = tz
    ))
  )
  plots <- plots[!vapply(plots, is.null, logical(1))]

  # 5. Write to disk (optional)
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!is.null(report)) {
      readr::write_csv(report, file.path(output_dir, "species_filter_report.csv"))
    }
    readr::write_csv(summary, file.path(output_dir, "summary.csv"))
    file_names <- c(
      counts = "01_species_counts.png",
      daynight = "02_top_species_daynight.png"
    )
    for (nm in names(plots)) {
      ggplot2::ggsave(
        file.path(output_dir, file_names[[nm]]),
        plots[[nm]],
        width = 10, height = 6, dpi = 150
      )
    }
    message(
      "site_report: wrote ", length(plots), " plots",
      if (!is.null(report)) " + filter report" else "",
      " to ", output_dir
    )
  }

  invisible(list(
    report = report,
    summary = summary,
    plots = plots,
    data_filtered = data_filtered,
    output_dir = output_dir
  ))
}

#' Trim detections to a deployment time window
#'
#' Internal helper. Drops rows whose `recording_window_time` falls outside the
#' `[start_after, end_before]` bounds. `NULL` bounds are ignored. Character /
#' Date bounds are parsed as UTC to match the times from
#' \code{\link{read_birdnet_file}}.
#'
#' @noRd
trim_to_window <- function(data, start_after = NULL, end_before = NULL) {
  if (is.null(start_after) && is.null(end_before)) {
    return(data)
  }
  if (!"recording_window_time" %in% names(data)) {
    warning("No 'recording_window_time' column; skipping time-window trim.")
    return(data)
  }
  as_utc <- function(x) {
    if (is.character(x) || inherits(x, "Date")) {
      lubridate::as_datetime(x, tz = "UTC")
    } else {
      x
    }
  }
  t <- data$recording_window_time
  keep <- !is.na(t)
  if (!is.null(start_after)) keep <- keep & t >= as_utc(start_after)
  if (!is.null(end_before)) keep <- keep & t <= as_utc(end_before)
  n_drop <- sum(!keep)
  if (n_drop > 0) {
    message("site_report: dropped ", n_drop, " detections outside the deployment window.")
  }
  data[keep, , drop = FALSE]
}
