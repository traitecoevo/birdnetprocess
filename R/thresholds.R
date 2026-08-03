#' Per-species detection thresholds
#'
#' @description
#' Returns the confidence threshold that applies to each of `species`, taking a
#' species' own calibrated cut where the config has one and the deployment-wide
#' `analysis$confidence` otherwise.
#'
#' @details
#' A single global threshold treats every species as equally detectable, which
#' no evaluation ever supports: a distinctive call and a faint, easily-confused
#' one need different cuts to reach the same precision. Where a labelled
#' soundscape has been scored, `analysis$confidence_by_species` carries the
#' per-species operating points; everything else falls back to
#' `analysis$confidence`.
#'
#' Thresholds must be in the same space as the detections' `Confidence` column.
#' Evaluation harnesses often work in **logits** while detectors emit
#' post-sigmoid **probabilities** — a threshold moved between the two without
#' converting looks entirely plausible and silently passes almost everything.
#' Convert before putting values in a config.
#'
#' @param cfg A `birdnet_deployment`, from \code{\link{read_deployment}}.
#' @param species Character vector of common names.
#'
#' @return A numeric vector of thresholds, parallel to `species`.
#' @export
#' @seealso \code{\link{threshold_source}}
#' @examples
#' \dontrun{
#' cfg <- read_deployment("deployments/wild_deserts/deployment.yml")
#' species_thresholds(cfg, c("Brown Songlark", "Budgerigar"))
#' }
species_thresholds <- function(cfg, species) {
  default <- cfg$analysis$confidence %||% 0.5
  by_species <- cfg$analysis$confidence_by_species

  out <- rep(default, length(species))
  if (length(by_species) > 0) {
    named <- unlist(by_species)
    hit <- match(species, names(named))
    out[!is.na(hit)] <- as.numeric(named[hit[!is.na(hit)]])
  }
  out
}

#' Where each species' threshold came from
#'
#' @description
#' Labels each species `"calibrated"`, `"unvalidated"` or `"default"`, so a
#' report can say which detections rest on a measured operating point and which
#' rest on an assumption.
#'
#' @details
#' The distinction that matters is between a species that was *never tested* and
#' one that was *tested and failed*. Both end up on the deployment's default
#' threshold, but they mean opposite things: the first is simply unknown, while
#' the second is a species the evaluation showed the detector could not
#' identify reliably at any cut. Collapsing them into one bucket quietly
#' launders a known-bad species into a plain result, so
#' `analysis$unvalidated_species` names the second group explicitly and the
#' report flags it.
#'
#' @param cfg A `birdnet_deployment`, from \code{\link{read_deployment}}.
#' @param species Character vector of common names.
#'
#' @return A character vector parallel to `species`.
#' @export
#' @seealso \code{\link{species_thresholds}}
threshold_source <- function(cfg, species) {
  calibrated <- names(unlist(cfg$analysis$confidence_by_species))
  unvalidated <- unlist(cfg$analysis$unvalidated_species)

  out <- rep("default", length(species))
  out[species %in% calibrated] <- "calibrated"
  # Tested-and-failed wins over the default label, and over a calibrated one:
  # a species should never be listed as both.
  out[species %in% unvalidated] <- "unvalidated"
  out
}

#' Apply per-species thresholds to a detection table
#'
#' @description
#' Filters detections using each species' own threshold, and records which
#' threshold was applied and where it came from.
#'
#' @param data A tibble of detections with `Common Name` and `Confidence`.
#' @param cfg A `birdnet_deployment`, from \code{\link{read_deployment}}.
#'
#' @return `data`, filtered, with added `threshold` and `threshold_source`
#'         columns.
#' @export
#' @import dplyr
apply_thresholds <- function(data, cfg) {
  if (nrow(data) == 0) {
    return(data)
  }
  species <- data[["Common Name"]]
  data$threshold <- species_thresholds(cfg, species)
  data$threshold_source <- threshold_source(cfg, species)
  data[data$Confidence >= data$threshold, , drop = FALSE]
}
