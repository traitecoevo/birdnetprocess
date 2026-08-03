#' Read a deployment configuration file
#'
#' @description
#' Reads a `deployment.yml` describing one field deployment - where the
#' recorders were, which sites and dates they covered, which detector produced
#' the detections, and how the results should be filtered and grouped. The
#' resulting object is the single input to
#' \code{\link{read_deployment_detections}} and \code{\link{birdnet_report}}.
#'
#' @details
#' The same file is intended to be read by the detector-running step (in
#' Python) as well as by this package, so that the site list and target dates
#' cannot drift apart between the two halves of the pipeline.
#'
#' Relative paths in the file are resolved against the directory containing the
#' file itself, not the working directory, so a deployment folder can be moved
#' or checked out anywhere. A leading `~` is expanded.
#'
#' See \code{\link{new_deployment}} to write a commented starter file.
#'
#' The expected structure is:
#'
#' ```yaml
#' name: Smiths Lake phenology
#' location:
#'   latitude: -32.38
#'   longitude: 152.51
#'   tz: Australia/Sydney
#'   place: Smiths Lake, NSW
#' detector:
#'   name: pelican0-15
#'   model: path/to/pelican0-15.tflite
#'   labels: path/to/pelican0-15_Labels.txt
#' sites:
#'   - id: Powerline Strip
#'     detections: path/to/detections/Powerline Strip
#'     date: 2026-02-03
#' analysis:
#'   confidence: 0.5
#' ```
#'
#' @param path Path to the YAML file, or to a directory containing a
#'        `deployment.yml`.
#'
#' @return An object of class `birdnet_deployment`: a list with elements
#'         `name`, `location`, `detector`, `sites`, and `analysis`, plus the
#'         resolved `path` and `dir` of the file it came from. Each element of
#'         `sites` gains `start_after` and `end_before` bounds derived from its
#'         `date` (or `start`/`end`).
#' @export
#' @seealso \code{\link{new_deployment}},
#'          \code{\link{read_deployment_detections}}
#' @examples
#' \dontrun{
#' cfg <- read_deployment("deployments/smiths_lake/deployment.yml")
#' cfg
#' }
read_deployment <- function(path) {
  if (!file.exists(path)) {
    stop("Deployment config not found: ", path, call. = FALSE)
  }
  if (dir.exists(path)) {
    path <- file.path(path, "deployment.yml")
    if (!file.exists(path)) {
      stop("No 'deployment.yml' in directory: ", dirname(path), call. = FALSE)
    }
  }

  path <- normalizePath(path, mustWork = TRUE)
  dir <- dirname(path)
  raw <- yaml::read_yaml(path)

  if (!is.list(raw)) {
    stop("Deployment config did not parse as a YAML mapping: ", path, call. = FALSE)
  }

  cfg <- list(
    name = raw$name,
    location = raw$location,
    detector = raw$detector,
    sites = raw$sites,
    analysis = raw$analysis %||% list(),
    path = path,
    dir = dir
  )

  # Paths in the file are relative to the file, not to getwd().
  cfg$detector$model <- resolve_config_path(cfg$detector$model, dir)
  cfg$detector$labels <- resolve_config_path(cfg$detector$labels, dir)
  cfg$detector$venv <- resolve_config_path(cfg$detector$venv, dir)
  cfg$analysis$range_filter$raster <-
    resolve_config_path(cfg$analysis$range_filter$raster, dir)

  cfg$sites <- lapply(cfg$sites, function(site) {
    site$recordings <- resolve_config_path(site$recordings, dir)
    site$detections <- resolve_config_path(site$detections, dir)
    c(site, site_window(site))
  })

  cfg <- structure(cfg, class = "birdnet_deployment")
  validate_deployment(cfg)
  cfg
}

#' Validate a deployment configuration
#'
#' @description
#' Checks a `birdnet_deployment` for the mistakes that would otherwise surface
#' much later as a confusing plot or an empty report: missing required fields,
#' an out-of-range or transposed latitude/longitude, an unknown time zone, or
#' duplicate site ids.
#'
#' @details
#' Missing *files* are a warning rather than an error, because a config is
#' normally written before the detector has run and so before the detection
#' folders exist.
#'
#' Called automatically by \code{\link{read_deployment}}.
#'
#' @param cfg A `birdnet_deployment`, from \code{\link{read_deployment}}.
#'
#' @return `cfg`, invisibly. Called for its errors and warnings.
#' @export
validate_deployment <- function(cfg) {
  if (!inherits(cfg, "birdnet_deployment")) {
    stop("`cfg` must be a birdnet_deployment (see read_deployment()).", call. = FALSE)
  }
  where <- paste0(" (in ", cfg$path %||% "config", ")")

  require_field(cfg$name, "name", where)
  require_field(cfg$location, "location", where)
  require_field(cfg$location$latitude, "location$latitude", where)
  require_field(cfg$location$longitude, "location$longitude", where)
  require_field(cfg$detector, "detector", where)
  require_field(cfg$detector$name, "detector$name", where)

  lat <- cfg$location$latitude
  lon <- cfg$location$longitude
  if (!is.numeric(lat) || length(lat) != 1 || is.na(lat) || abs(lat) > 90) {
    stop("location$latitude must be a single number between -90 and 90; got ",
         deparse(lat), where, call. = FALSE)
  }
  if (!is.numeric(lon) || length(lon) != 1 || is.na(lon) || abs(lon) > 180) {
    stop("location$longitude must be a single number between -180 and 180; got ",
         deparse(lon), where, call. = FALSE)
  }

  tz <- cfg$location$tz
  if (is.null(tz)) {
    stop("Missing required field 'location$tz'", where,
         ". Day/night shading needs it; e.g. \"Australia/Sydney\".", call. = FALSE)
  }
  if (!tz %in% OlsonNames()) {
    stop("Unknown time zone '", tz, "'", where,
         ". See OlsonNames() for valid values.", call. = FALSE)
  }

  if (length(cfg$sites) == 0) {
    stop("Deployment has no sites", where, ".", call. = FALSE)
  }
  ids <- vapply(cfg$sites, function(s) s$id %||% NA_character_, character(1))
  if (anyNA(ids)) {
    stop("Every entry in 'sites' needs an 'id'", where, ".", call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop("Duplicate site ids", where, ": ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "), call. = FALSE)
  }

  conf <- cfg$analysis$confidence
  if (!is.null(conf) && (!is.numeric(conf) || conf < 0 || conf > 1)) {
    stop("analysis$confidence must be between 0 and 1; got ", deparse(conf),
         where, call. = FALSE)
  }

  # Files that should already exist by the time the config is written.
  check_exists(cfg$detector$labels, "detector$labels")
  check_exists(cfg$analysis$range_filter$raster, "analysis$range_filter$raster")

  # Detection folders are written by the detector, so may legitimately be absent.
  missing_det <- vapply(cfg$sites, function(s) {
    !is.null(s$detections) && !dir.exists(s$detections)
  }, logical(1))
  if (any(missing_det)) {
    message(
      "Detections not found yet for ", sum(missing_det), " of ", length(ids),
      " site(s): ", paste(ids[missing_det], collapse = ", "),
      ". Run the detector before reporting."
    )
  }

  invisible(cfg)
}

#' @export
print.birdnet_deployment <- function(x, ...) {
  cat("<birdnet_deployment>", x$name, "\n")
  place <- x$location$place %||% sprintf("%.4f, %.4f",
                                         x$location$latitude, x$location$longitude)
  cat("  location  ", place, " (", x$location$tz, ")\n", sep = "")
  cat("  detector  ", x$detector$name, format_version(x$detector$version),
      "\n", sep = "")

  ids <- vapply(x$sites, function(s) s$id, character(1))
  cat("  sites     ", length(ids), ": ", paste(ids, collapse = ", "), "\n", sep = "")

  starts <- do.call(c, lapply(x$sites, function(s) s$start_after))
  ends <- do.call(c, lapply(x$sites, function(s) s$end_before))
  if (length(starts) > 0) {
    cat("  dates     ", format(min(starts), "%Y-%m-%d"), " to ",
        format(max(ends), "%Y-%m-%d"), "\n", sep = "")
  } else {
    cat("  dates      all\n")
  }

  cat("  threshold ", x$analysis$confidence %||% 0.5, "\n", sep = "")
  if (!is.null(x$analysis$range_filter$raster)) cat("  range filter on\n")
  invisible(x)
}

#' Create a starter deployment configuration
#'
#' @description
#' Copies the annotated `deployment.yml` template shipped with the package into
#' `dir`, as the starting point for a new deployment. The template documents
#' every field, so it is easier to edit down than to write from scratch.
#'
#' @param dir Directory to write `deployment.yml` into. Created if needed.
#' @param name Optional deployment name to substitute into the template.
#' @param overwrite Overwrite an existing `deployment.yml`? Default `FALSE`.
#'
#' @return The path to the written file, invisibly.
#' @export
#' @examples
#' \dontrun{
#' new_deployment("deployments/smiths_lake", name = "Smiths Lake phenology")
#' }
new_deployment <- function(dir, name = NULL, overwrite = FALSE) {
  template <- pkg_file("templates", "deployment.yml")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(dir, "deployment.yml")
  if (file.exists(target) && !overwrite) {
    stop("'", target, "' already exists. Use overwrite = TRUE to replace it.",
         call. = FALSE)
  }

  lines <- readLines(template, warn = FALSE)
  if (!is.null(name)) {
    lines <- sub("^name:.*$", paste0("name: ", name), lines)
  }
  writeLines(lines, target)
  message("Wrote ", target, " \u2014 edit the paths, then read_deployment() it.")
  invisible(target)
}

# ---------------------------------------------------------------------------
# internals

#' Resolve a config path against the config file's own directory
#'
#' Expands `~` and makes relative paths relative to the YAML file rather than
#' the working directory, so a deployment folder works from anywhere.
#'
#' @noRd
resolve_config_path <- function(x, dir) {
  if (is.null(x) || !nzchar(x)) {
    return(NULL)
  }
  x <- path.expand(x)
  # On Windows an absolute path may start with a drive letter rather than "/".
  is_absolute <- grepl("^(/|[A-Za-z]:)", x)
  if (!is_absolute) {
    x <- file.path(dir, x)
  }
  normalizePath(x, mustWork = FALSE)
}

#' Derive `[start_after, end_before]` bounds for one site
#'
#' A site may give either a single `date` (a whole calendar day) or explicit
#' `start`/`end` bounds. Both are parsed as UTC to match the wall-clock times
#' `read_birdnet_file()` derives from recorder filenames.
#'
#' @noRd
site_window <- function(site) {
  as_utc <- function(x) {
    if (is.null(x)) return(NULL)
    lubridate::as_datetime(x, tz = "UTC")
  }

  if (!is.null(site$date)) {
    if (!is.null(site$start) || !is.null(site$end)) {
      stop("Site '", site$id, "' has both 'date' and 'start'/'end'; use one or the other.",
           call. = FALSE)
    }
    start <- as_utc(site$date)
    if (is.na(start)) {
      stop("Could not parse date '", site$date, "' for site '", site$id,
           "'; expected YYYY-MM-DD.", call. = FALSE)
    }
    return(list(
      start_after = start,
      end_before = start + lubridate::days(1) - lubridate::seconds(1)
    ))
  }

  list(start_after = as_utc(site$start), end_before = as_utc(site$end))
}

#' Render a detector version for display
#'
#' A `v` prefix only makes sense for a number: `version: 2.4` reads as "v2.4",
#' but `version: perch` would read as "vperch". Free-text versions are shown in
#' parentheses instead.
#'
#' @noRd
format_version <- function(version) {
  if (is.null(version) || !nzchar(as.character(version))) {
    return("")
  }
  version <- as.character(version)
  if (grepl("^[0-9]", version)) {
    paste0(" v", version)
  } else {
    paste0(" (", version, ")")
  }
}

#' @noRd
require_field <- function(value, field, where = "") {
  if (is.null(value)) {
    stop("Missing required field '", field, "'", where, ".", call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
check_exists <- function(path, field) {
  if (!is.null(path) && !file.exists(path)) {
    warning("Path in '", field, "' does not exist: ", path, call. = FALSE)
  }
  invisible(TRUE)
}

#' Locate a file shipped in `inst/`
#'
#' `system.file()` returns "" under `devtools::load_all()`, which is how this
#' package is normally used, so fall back to the source tree - the same
#' fallback `tests/testthat/test-read_formats.R` uses for `inst/extdata`.
#'
#' @noRd
pkg_file <- function(...) {
  path <- system.file(..., package = "birdnetprocess")
  if (nzchar(path) && file.exists(path)) {
    return(path)
  }
  # Not installed: look for inst/ relative to the loaded package's source dir.
  dev <- file.path(system.file(package = "birdnetprocess"), "inst", ...)
  if (file.exists(dev)) {
    return(dev)
  }
  stop("Could not find '", file.path(...), "' in the birdnetprocess package.",
       call. = FALSE)
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
