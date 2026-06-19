#' Normalise a species common name to species-distribution-layer style
#'
#' Internal helper. Converts a BirdNET common name (e.g. "Grey Teal",
#' "Bourke's Parrot") into the lower-case, underscore-separated form used as
#' layer names in the abundance raster stack (e.g. `gray_teal`,
#' `bourke_s_parrot`). British "grey" is converted to American "gray" to match
#' the raster naming.
#'
#' @param x A character vector of common names.
#' @return A character vector of normalised names.
#' @noRd
normalize_species_name <- function(x) {
  x <- tolower(x)
  x <- gsub("grey", "gray", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

#' Filter BirdNET detections to species likely present at a location
#'
#' @description
#' Removes detections of species whose modelled range does **not** cover the
#' recorder's location, using a species-distribution-model (SDM) raster stack.
#' This is aimed at the geographic false positives that BirdNET commonly
#' produces (e.g. eastern/coastal species proposed at an arid inland site).
#'
#' @details
#' The abundance raster is expected to be a multi-layer stack with one
#' range-masked layer per species, named in lower-case underscore style
#' (e.g. `zebra_finch`, `noisy_miner`). A cell value of `NA` is interpreted as
#' "outside the modelled range" — the convention used by range-masked SDMs.
#'
#' Each detected species is classified as:
#' \itemize{
#'   \item **kept: within modelled range** — a matching layer exists and the
#'         site has a (non-`NA`) abundance value above `min_abundance`.
#'   \item **removed: outside modelled range** — a matching layer exists but the
#'         site value is `NA`.
#'   \item **removed: below threshold** — a matching layer exists, the value is
#'         non-`NA` but `<= min_abundance` (only possible when `min_abundance > 0`).
#'   \item **unassessed: no range map** — no matching layer (non-birds such as
#'         insects/mammals/anthropogenic labels, and birds absent from the
#'         stack, e.g. arid specialists). These are always **kept**.
#' }
#' Only species in the "removed" categories are dropped, so the function never
#' removes a species it cannot assess.
#'
#' @param data A tibble of BirdNET detections (e.g. from
#'        \code{\link{read_birdnet_folder}}).
#' @param abundance_raster A `terra` `SpatRaster` of per-species abundance
#'        layers, or a file path to one. Requires the `terra` package.
#' @param latitude,longitude Numeric. The recorder location, in decimal degrees
#'        (WGS84 / EPSG:4326).
#' @param min_abundance Numeric. Species with a modelled abundance at or below
#'        this value are also removed. Default `0` (range-only behaviour: keep
#'        anything within the modelled range).
#' @param name_overrides Optional named character vector mapping common names to
#'        layer names, for species the automatic matcher misses, e.g.
#'        `c("Some Bird" = "some_bird_layer")`.
#' @param keep_species Optional character vector of common names to always keep,
#'        regardless of their modelled range. Useful for nomadic or vagrant
#'        species (e.g. waterbirds that irrupt into arid sites after rain) that
#'        the range-masked SDM wrongly excludes. Reported with status
#'        `"kept: manual override"`.
#' @param species_col Name of the column holding common names. Default
#'        `"Common Name"`.
#'
#' @return The filtered `data` tibble. A per-species report is attached as
#'         `attr(result, "range_report")`, a tibble with columns `species`,
#'         `n_detections`, `max_confidence` (the highest detection confidence
#'         for that species), `matched_layer`, `abundance`, and `status`.
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#' r <- terra::rast("nsw_abundance_stack_3km.tif")
#' d <- read_birdnet_folder("wilddeserts")
#' filtered <- filter_by_range(d, r, latitude = -29.23, longitude = 141.29)
#' attr(filtered, "range_report")
#' }
filter_by_range <- function(data,
                            abundance_raster,
                            latitude,
                            longitude,
                            min_abundance = 0,
                            name_overrides = NULL,
                            keep_species = NULL,
                            species_col = "Common Name") {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package \"terra\" is needed for range filtering. ",
      "Please install it with install.packages(\"terra\")."
    )
  }
  if (!species_col %in% names(data)) {
    stop("Column '", species_col, "' not found in data.")
  }

  # Accept either a SpatRaster or a path to one
  if (is.character(abundance_raster)) {
    abundance_raster <- terra::rast(abundance_raster)
  }

  layer_names <- names(abundance_raster)

  # Unique detected species, with detection counts and peak confidence
  # (for the report)
  has_conf <- "Confidence" %in% names(data)
  counts <- data |>
    dplyr::group_by(.data[[species_col]]) |>
    dplyr::summarise(
      n_detections = dplyr::n(),
      max_confidence = if (has_conf) {
        suppressWarnings(max(.data$Confidence, na.rm = TRUE))
      } else {
        NA_real_
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      max_confidence = ifelse(is.finite(.data$max_confidence),
        .data$max_confidence, NA_real_
      )
    ) |>
    dplyr::rename(species = dplyr::all_of(species_col))

  # Match each common name to a raster layer
  overrides_norm <- if (is.null(name_overrides)) {
    character(0)
  } else {
    stats::setNames(unname(name_overrides), names(name_overrides))
  }
  matched <- unname(vapply(counts$species, function(sp) {
    if (sp %in% names(overrides_norm)) {
      return(unname(overrides_norm[[sp]]))
    }
    key <- normalize_species_name(sp)
    if (key %in% layer_names) key else NA_character_
  }, character(1)))

  # Extract abundance at the point (reproject point to raster CRS if needed)
  pt <- terra::vect(
    cbind(longitude, latitude),
    crs = "EPSG:4326"
  )
  if (terra::crs(abundance_raster) != "" &&
    terra::crs(pt) != terra::crs(abundance_raster)) {
    pt <- terra::project(pt, terra::crs(abundance_raster))
  }
  layers_present <- stats::na.omit(unique(matched))
  if (length(layers_present) > 0) {
    vals_row <- terra::extract(
      abundance_raster[[layers_present]], pt, ID = FALSE
    )
    abundance_lookup <- unlist(vals_row[1, , drop = TRUE])
    names(abundance_lookup) <- layers_present
  } else {
    abundance_lookup <- numeric(0)
  }
  abundance <- unname(abundance_lookup[matched])

  # Warn if the point appears to be outside the raster (everything NA)
  if (length(layers_present) > 0 && all(is.na(abundance))) {
    warning(
      "All matched species returned NA at (", latitude, ", ", longitude,
      "); the point may be outside the raster extent."
    )
  }

  # Classify each species
  status <- dplyr::case_when(
    is.na(matched) ~ "unassessed: no range map",
    is.na(abundance) ~ "removed: outside modelled range",
    abundance <= min_abundance ~ "removed: below threshold",
    TRUE ~ "kept: within modelled range"
  )

  # Manual keep overrides: never drop these species, whatever their range says.
  if (!is.null(keep_species)) {
    forced <- counts$species %in% keep_species
    status[forced] <- "kept: manual override"
  }

  report <- counts |>
    dplyr::mutate(
      matched_layer = matched,
      abundance = abundance,
      status = status
    ) |>
    dplyr::arrange(status, dplyr::desc(.data$n_detections))

  removed_species <- report$species[startsWith(report$status, "removed")]
  filtered <- data |>
    dplyr::filter(!.data[[species_col]] %in% removed_species)

  message(
    "filter_by_range: removed ", length(removed_species), " species (",
    nrow(data) - nrow(filtered), " detections); kept ",
    nrow(report) - length(removed_species), " species."
  )

  attr(filtered, "range_report") <- report
  filtered
}
