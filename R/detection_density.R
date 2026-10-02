#' Detection density: detections per recorded hour, with zeros kept
#'
#' @description
#' Counts detections at or above a confidence threshold per site and time bin,
#' and divides by how much audio was actually recorded in that bin. Bins that
#' were recorded but had no detections are kept as zeros, which is what
#' separates "quiet" from "not recording" in any time-series plot.
#'
#' @details
#' **Effort.** Recording effort can only be measured from an *all-window*
#' table - one where every analysis window has a row, whatever its score, as
#' written by an inference run with no confidence floor (for example the
#' Parquet score stores read by [read_birdnet_file()]). Effort is then the
#' number of distinct windows per site and bin times the window length.
#'
#' An ordinary thresholded selection table only lists windows that scored
#' highly, so effort cannot be derived from it. Pass an `effort` table instead
#' (columns `Site`, `time` and `hours`), for example from a recording manifest.
#' If no `effort` is given and the lowest score in `df` is above 0.05, the table
#' looks thresholded and the function warns.
#'
#' **Sites** come from a `Site` column, or else from the filename prefix
#' (everything before the `_YYYYMMDD_HHMMSS` stamp).
#'
#' **Time bins.** `unit = "day"` gives one row per site-day (`time` is a
#' `Date`); `unit = "hour"` gives one row per site-hour (`time` is a POSIXct at
#' the start of the hour), which suits time-of-day views. Times are the clock
#' times in the recording filenames.
#'
#' @param df Detections from [read_birdnet_file()], [read_birdnet_folder()] or
#'   [read_birdnet_sites()].
#' @param confidence Minimum confidence (0-1) for a window to count as a
#'   detection. Default 0.5.
#' @param unit Time bin: `"day"` (default) or `"hour"`.
#' @param species Optional `Common Name` values to keep. `NULL` keeps all;
#'   each species gets its own rows.
#' @param effort Optional data frame of recorded effort with columns `Site`,
#'   `time` (matching `unit`) and `hours`. Needed when `df` is not an
#'   all-window table.
#' @param window_s Analysis window length in seconds, used to turn window
#'   counts into hours. Default: taken from the data (`end - begin`), else 3.
#'
#' @importFrom rlang %||% .data
#' @return A tibble with one row per site, time bin and species: `Site`,
#'   `time`, `Common Name`, `hours` (recorded), `detections` and `rate`
#'   (detections per recorded hour).
#' @export
#' @examples
#' \dontrun{
#' d <- read_birdnet_folder("scores", pattern = "\\.parquet$")
#' dens <- detection_density(d, confidence = 0.9)
#' }
detection_density <- function(df,
                              confidence = 0.5,
                              unit = c("day", "hour"),
                              species = NULL,
                              effort = NULL,
                              window_s = NULL) {
  unit <- match.arg(unit)
  for (col in c("Confidence", "Common Name", "begin_time_s")) {
    if (!col %in% names(df)) {
      stop("`df` needs a `", col, "` column (read it with read_birdnet_file()).",
           call. = FALSE)
    }
  }
  if (!"Site" %in% names(df)) df$Site <- site_from_file(df$file_name)
  if (!is.null(species)) df <- df[df$`Common Name` %in% species, ]
  if (nrow(df) == 0) stop("No rows remain after the species filter.", call. = FALSE)

  t <- detection_time(df)
  df$time <- if (unit == "day") as.Date(t) else lubridate::floor_date(t, "hour")

  if (is.null(effort)) {
    if (min(df$Confidence, na.rm = TRUE) > 0.05) {
      warning("The lowest score in `df` is ",
              signif(min(df$Confidence, na.rm = TRUE), 2),
              ", so this looks like a thresholded table rather than an ",
              "all-window one; effort will be undercounted. Supply `effort`.",
              call. = FALSE)
    }
    if (is.null(window_s)) {
      window_s <- if ("end_time_s" %in% names(df)) {
        stats::median(df$end_time_s - df$begin_time_s, na.rm = TRUE)
      } else {
        3
      }
    }
    effort <- df |>
      dplyr::distinct(.data$Site, .data$time, .data$file_name, .data$begin_time_s) |>
      dplyr::count(.data$Site, .data$time, name = "windows") |>
      dplyr::mutate(hours = .data$windows * window_s / 3600) |>
      dplyr::select("Site", "time", "hours")
  } else {
    missing <- setdiff(c("Site", "time", "hours"), names(effort))
    if (length(missing)) {
      stop("`effort` is missing column(s): ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    effort <- effort[, c("Site", "time", "hours")]
  }

  counts <- df |>
    dplyr::filter(.data$Confidence >= confidence) |>
    dplyr::count(.data$Site, .data$time, .data$`Common Name`, name = "detections")

  # Every recorded bin gets a row for every species, so quiet bins read as 0.
  effort |>
    tidyr::crossing(`Common Name` = unique(df$`Common Name`)) |>
    dplyr::left_join(counts, by = c("Site", "time", "Common Name")) |>
    dplyr::mutate(
      detections = dplyr::coalesce(.data$detections, 0L),
      rate = .data$detections / .data$hours
    ) |>
    dplyr::arrange(.data$Site, .data$time, .data$`Common Name`)
}

#' Site name from a recording filename
#'
#' Internal. Everything before the `_YYYYMMDD_HHMMSS` stamp, e.g.
#' `SiteA_20240101_120000.wav` -> `SiteA`.
#'
#' @noRd
site_from_file <- function(file_name) {
  sub("_[0-9]{8}_[0-9]{6}.*$", "", basename(file_name))
}
