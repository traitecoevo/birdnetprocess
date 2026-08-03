#' Read a BirdNET labels file
#'
#' @description
#' Parses the label list that ships alongside a BirdNET classifier - one line
#' per class, in the form `Genus species_Common Name` - into a tibble. The
#' scientific name is the only place a custom classifier records what kind of
#' organism a class is, which is what makes it possible to sort detections into
#' taxon groups (see \code{\link{assign_groups}}).
#'
#' @details
#' Lines without an underscore are kept with an empty scientific name, so
#' non-taxonomic classes such as noise categories survive rather than silently
#' vanishing.
#'
#' @param path Path to the labels `.txt` file.
#'
#' @return A tibble with columns `scientific`, `common`, and `genus`.
#' @export
#' @examples
#' \dontrun{
#' labels <- read_detector_labels("recognizers/pelican0-15_Labels.txt")
#' head(labels)
#' }
read_detector_labels <- function(path) {
  if (!file.exists(path)) {
    stop("Labels file not found: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) {
    stop("Labels file is empty: ", path, call. = FALSE)
  }

  has_sep <- grepl("_", lines, fixed = TRUE)
  scientific <- ifelse(has_sep, sub("_.*$", "", lines), "")
  common <- ifelse(has_sep, sub("^[^_]*_", "", lines), lines)

  tibble::tibble(
    scientific = scientific,
    common = common,
    # BirdNET writes "Genus species"; the genus alone is the useful grain for
    # grouping, since a classifier typically has many congeners.
    genus = sub(" .*$", "", scientific)
  )
}

#' Sort detections into taxon groups
#'
#' @description
#' Adds a `group` column to a detection table - `"Birds"`, `"Insects"`,
#' `"Frogs"` and so on - by matching each species' genus, taken from the
#' detector's labels file, against the rules in a deployment config. Use it to
#' colour or facet a plot by broad taxon, as
#' \code{\link{plot_daily_activity}}'s `group_col` argument expects.
#'
#' @details
#' A custom classifier covering more than birds gives no direct signal about
#' what a class *is*; the genus in the labels file is the signal. Rules are
#' written per deployment because the relevant split differs by project -
#' cicadas versus birds in one, native versus introduced in another.
#'
#' Each rule may list `genera` (matched against the genus), `species` (matched
#' against the common name, for one-off cases the genus can't express), or
#' `default: true`. Exactly one group should be the default; everything
#' unmatched lands there. Rules are applied in the order written, so an earlier
#' group wins a tie.
#'
#' @param data A tibble of detections, e.g. from
#'        \code{\link{read_deployment_detections}}.
#' @param labels Path to a labels file, or a tibble from
#'        \code{\link{read_detector_labels}}. If `NULL`, taken from the
#'        deployment config attached to `data`.
#' @param rules A named list of grouping rules. If `NULL`, taken from
#'        `analysis$groups` in the attached config.
#' @param species_col Column holding the common name. Default `"Common Name"`.
#' @param group_col Name of the column to add. Default `"group"`.
#'
#' @return `data` with an added `group_col` column, a factor whose levels
#'         follow the order the groups were declared in.
#' @export
#' @import dplyr
#' @seealso \code{\link{read_detector_labels}}, \code{\link{group_colours}}
#' @examples
#' \dontrun{
#' cfg <- read_deployment("deployments/smiths_lake/deployment.yml")
#' d <- read_deployment_detections(cfg) |> assign_groups()
#' table(d$group)
#' }
assign_groups <- function(data,
                          labels = NULL,
                          rules = NULL,
                          species_col = "Common Name",
                          group_col = "group") {
  cfg <- attr(data, "deployment")
  if (is.null(labels)) labels <- cfg$detector$labels
  if (is.null(rules)) rules <- cfg$analysis$groups

  if (is.null(rules) || length(rules) == 0) {
    stop("No grouping rules given, and none in the deployment config's ",
         "analysis$groups.", call. = FALSE)
  }
  if (is.null(labels)) {
    stop("No labels file given, and none in the deployment config's ",
         "detector$labels. Grouping needs the classifier's scientific names.",
         call. = FALSE)
  }
  if (!species_col %in% names(data)) {
    stop("Column '", species_col, "' not found in the detections.", call. = FALSE)
  }
  if (is.character(labels)) labels <- read_detector_labels(labels)

  defaults <- names(rules)[vapply(rules, function(r) isTRUE(r$default), logical(1))]
  if (length(defaults) > 1) {
    stop("More than one group is marked 'default': ",
         paste(defaults, collapse = ", "), call. = FALSE)
  }
  fallback <- if (length(defaults) == 1) defaults else NA_character_

  species <- unique(data[[species_col]])
  genus <- labels$genus[match(species, labels$common)]

  group <- rep(NA_character_, length(species))
  for (nm in names(rules)) {
    rule <- rules[[nm]]
    hit <- rep(FALSE, length(species))
    if (!is.null(rule$genera)) {
      hit <- hit | (!is.na(genus) & genus %in% unlist(rule$genera))
    }
    if (!is.null(rule$species)) {
      hit <- hit | species %in% unlist(rule$species)
    }
    # First rule to claim a species keeps it.
    group[hit & is.na(group)] <- nm
  }
  group[is.na(group)] <- fallback

  unmatched <- species[is.na(genus)]
  if (length(unmatched) > 0) {
    message(
      length(unmatched), " species not found in the labels file",
      if (!is.na(fallback)) paste0("; assigned to '", fallback, "'") else
        " and left ungrouped",
      ": ", paste(utils::head(unmatched, 5), collapse = ", "),
      if (length(unmatched) > 5) ", ..." else ""
    )
  }

  data[[group_col]] <- factor(
    group[match(data[[species_col]], species)],
    levels = names(rules)
  )
  data
}

#' Colours for taxon groups
#'
#' @description
#' Pulls the per-group `colour` entries out of a set of grouping rules into a
#' named vector suitable for \code{\link{plot_daily_activity}}'s
#' `group_colours` argument. Groups without a declared colour fall back to the
#' package palette, so a config need only name the colours it cares about.
#'
#' @details
#' Group colours are drawn from the stricter all-pairs-safe palette (see
#' \code{\link{birdnet_palette}}), because groups usually appear as species
#' labels scattered down a figure where any two may be compared, not as
#' neighbouring bars.
#'
#' A group named `"Other"` gets the muted residual grey unless the config says
#' otherwise: it is a leftover bin rather than a category anyone is meant to
#' track, and giving it a hue implies more than it means.
#'
#' @param rules A named list of grouping rules, or a `birdnet_deployment`.
#'
#' @return A named character vector of colours, one per group.
#' @export
#' @seealso \code{\link{assign_groups}}, \code{\link{birdnet_palette}}
group_colours <- function(rules) {
  if (inherits(rules, "birdnet_deployment")) rules <- rules$analysis$groups
  if (is.null(rules) || length(rules) == 0) {
    return(character(0))
  }

  declared <- vapply(rules, function(r) r$colour %||% NA_character_, character(1))
  nms <- names(rules)

  # Residual bins take the muted grey and don't consume an identity hue.
  residual <- tolower(nms) %in% c("other", "unknown", "ungrouped")
  hues <- birdnet_palette(all_pairs = TRUE)
  out <- rep(NA_character_, length(nms))
  out[residual] <- birdnet_ink$residual
  n_hued <- sum(!residual)
  if (n_hued > length(hues)) {
    warning(n_hued, " colour-carrying groups, but only ", length(hues),
            " separate reliably. Consider merging some into 'Other'.",
            call. = FALSE)
  }
  out[!residual] <- rep_len(hues, n_hued)

  out <- ifelse(is.na(declared), out, declared)
  stats::setNames(out, nms)
}
