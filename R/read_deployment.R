#' Read all detections described by a deployment config
#'
#' @description
#' Reads the detection tables for every site in a deployment, trims each to
#' that site's date window, and returns one tibble with a `Site` column taken
#' from the config.
#'
#' @details
#' This is the config-driven counterpart to \code{\link{read_birdnet_folder}}
#' and \code{\link{read_birdnet_sites}}, and it settles which site a detection
#' belongs to. Elsewhere in the package `Site` is inferred two different ways -
#' \code{\link{read_birdnet_sites}} takes it from the folder basename, while
#' \code{\link{plot_daily_activity}} falls back to the recorder prefix in the
#' file name - and the two disagree whenever the folder is named differently
#' from the recorder. Here the config's site `id` wins, so the label in the
#' report is the one you chose.
#'
#' Each site is trimmed to its own window, which matters when sites were
#' recorded on different days: a single global date range would either keep
#' another site's day or drop your own.
#'
#' Species listed in `analysis$exclude` are dropped - noise classes, and birds
#' someone with local knowledge says are not there. That list may be written
#' either as a plain sequence of common names or as a mapping of name to the
#' reason for dropping it; the reasons are carried through to the report, since
#' removing a species is a judgement a reader should be able to see and argue
#' with. What was dropped, and how much of it, comes back as
#' `attr(x, "excluded")`.
#'
#' The returned tibble also carries the config as an attribute so the report can
#' stamp its provenance. No confidence filtering happens here - the plotting
#' functions each apply `analysis$confidence` themselves.
#'
#' @param cfg A `birdnet_deployment`, from \code{\link{read_deployment}}.
#' @param pattern Regex matching the detection files within each site folder.
#'        Defaults to both arms' output: BirdNET `.txt`/`.csv` tables and a
#'        perch-head `*predictions.csv`.
#' @param recursive Search site folders recursively? Default `FALSE`.
#'
#' @return A tibble of detections with a `Site` column, one row per detection,
#'         with the deployment config attached as `attr(x, "deployment")`.
#' @export
#' @import dplyr
#' @seealso \code{\link{read_deployment}}, \code{\link{assign_groups}}
#' @examples
#' \dontrun{
#' cfg <- read_deployment("deployments/smiths_lake/deployment.yml")
#' d <- read_deployment_detections(cfg)
#' table(d$Site)
#' }
read_deployment_detections <- function(cfg,
                                       pattern = detection_file_pattern(),
                                       recursive = FALSE) {
  if (!inherits(cfg, "birdnet_deployment")) {
    stop("`cfg` must be a birdnet_deployment (see read_deployment()).", call. = FALSE)
  }

  per_site <- lapply(cfg$sites, function(site) {
    if (is.null(site$detections)) {
      warning("Site '", site$id, "' has no 'detections' path; skipping.", call. = FALSE)
      return(NULL)
    }
    if (!dir.exists(site$detections)) {
      warning("No detections folder for site '", site$id, "': ", site$detections,
              ". Has the detector run?", call. = FALSE)
      return(NULL)
    }

    df <- read_birdnet_folder(site$detections, pattern = pattern,
                              recursive = recursive)
    if (nrow(df) == 0) {
      warning("No detections read for site '", site$id, "'.", call. = FALSE)
      return(NULL)
    }

    # The config is the authority on site identity, overriding any Site column
    # a reader may have derived from folder or file names.
    df$Site <- site$id

    n_before <- nrow(df)
    df <- trim_to_window(df,
                         start_after = site$start_after,
                         end_before = site$end_before)
    if (nrow(df) < n_before) {
      message("  ", site$id, ": kept ", nrow(df), " of ", n_before,
              " detections within the site window.")
    } else {
      message("  ", site$id, ": ", nrow(df), " detections.")
    }
    df
  })

  per_site <- per_site[!vapply(per_site, is.null, logical(1))]
  if (length(per_site) == 0) {
    stop("No detections found for any site in '", cfg$name,
         "'. Check the 'detections' paths, or run the detector first.",
         call. = FALSE)
  }

  data <- dplyr::bind_rows(per_site)

  excluded <- exclude_species(cfg)
  if (length(excluded) > 0 && "Common Name" %in% names(data)) {
    hits <- data[["Common Name"]] %in% names(excluded)
    tally <- dplyr::tibble(
      species = names(excluded),
      reason = unname(excluded),
      detections = vapply(names(excluded),
                          function(s) sum(data[["Common Name"]] == s), integer(1))
    )
    data <- data[!hits, , drop = FALSE]
    if (any(hits)) {
      message("Excluded ", sum(hits), " detections of ", sum(tally$detections > 0),
              " listed species.")
    }
    attr(data, "excluded") <- tally[tally$detections > 0, , drop = FALSE]
  }

  attr(data, "deployment") <- cfg
  data
}

#' Species the config says are not there
#'
#' Internal. `analysis$exclude` accepts two YAML shapes, and both are useful:
#' a plain sequence of common names, or a mapping of name to the reason for
#' dropping it. The reason is worth capturing because these are *judgements* -
#' someone decided the bird does not occur here, or that the class is noise -
#' and a report that removes detections silently is one nobody can check.
#'
#' Returns a named character vector, names being species and values the reason
#' (empty string when none was given).
#'
#' @noRd
exclude_species <- function(cfg) {
  ex <- cfg$analysis$exclude
  if (length(ex) == 0) return(character(0))

  nms <- names(ex)
  if (is.null(nms) || !all(nzchar(nms))) {
    # Plain sequence: names are the species, no reasons.
    return(stats::setNames(rep("", length(ex)), as.character(unlist(ex))))
  }
  stats::setNames(vapply(ex, function(r) as.character(r %||% "")[1], character(1)),
                  nms)
}

#' Files that hold detections, whichever arm produced them
#'
#' Internal. BirdNET writes one `BirdNET…txt`/`.csv` table per recording; a
#' perch-head run writes a single `…predictions.csv` per site. A report should
#' not care which backbone ran, so the default matches both.
#'
#' @noRd
detection_file_pattern <- function() {
  "(BirdNET.*\\.(txt|csv)$)|(predictions\\.csv$)"
}

#' Trim detections to a deployment time window
#'
#' Internal helper shared by \code{\link{site_report}} and
#' \code{\link{read_deployment_detections}}. Drops rows whose
#' `recording_window_time` falls outside the `[start_after, end_before]`
#' bounds. `NULL` bounds are ignored. Character / Date bounds are parsed as UTC
#' to match the times from \code{\link{read_birdnet_file}}.
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
  data[keep, , drop = FALSE]
}
