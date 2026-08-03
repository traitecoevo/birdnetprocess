#' Render a deployment report
#'
#' @description
#' Turns a deployment config into a single self-contained HTML report: what was
#' recorded, where, with which detector, and what it heard. Every figure is
#' also written as a standalone PNG for reuse in talks and papers.
#'
#' @details
#' Everything the report needs comes from the config, so the same command
#' regenerates the report after more recordings arrive or the threshold
#' changes. Detections are read via \code{\link{read_deployment_detections}}
#' unless you pass them in - useful if you want to modify them first.
#'
#' Figures are rendered on a light background regardless of the reader's
#' theme, since a PNG cannot restyle itself for a dark page.
#'
#' Requires the `quarto` R package and a Quarto installation.
#'
#' @param cfg A `birdnet_deployment`, from \code{\link{read_deployment}}, or a
#'        path to a `deployment.yml`.
#' @param output_dir Directory for the report. Defaults to a `report/` folder
#'        beside the config file.
#' @param data Optional pre-read detections. If `NULL`, read from the config.
#' @param filename Output file name. Defaults to the deployment name, slugified.
#' @param quiet Suppress Quarto's render output? Default `TRUE`.
#'
#' @return The path to the rendered HTML, invisibly.
#' @export
#' @seealso \code{\link{read_deployment}}, \code{\link{site_report}} for the
#'          simpler single-site PNG output.
#' @examples
#' \dontrun{
#' cfg <- read_deployment("deployments/smiths_lake/deployment.yml")
#' birdnet_report(cfg)
#' }
birdnet_report <- function(cfg,
                           output_dir = NULL,
                           data = NULL,
                           filename = NULL,
                           quiet = TRUE) {
  if (is.character(cfg)) cfg <- read_deployment(cfg)
  if (!inherits(cfg, "birdnet_deployment")) {
    stop("`cfg` must be a birdnet_deployment or a path to a deployment.yml.",
         call. = FALSE)
  }
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The 'quarto' package is required to render reports. ",
         "Install it with install.packages('quarto').", call. = FALSE)
  }

  if (is.null(output_dir)) output_dir <- file.path(cfg$dir, "report")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  if (is.null(filename)) filename <- paste0(slugify(cfg$name), ".html")
  if (!grepl("\\.html$", filename)) filename <- paste0(filename, ".html")

  bundle <- report_bundle(cfg, data = data)

  # The .qmd is a thin renderer over a precomputed bundle: all the R logic
  # stays in the package where it can be tested, and a failed plot can't take
  # the whole render down with it.
  bundle_path <- file.path(tempdir(), "birdnet_report_bundle.rds")
  saveRDS(bundle, bundle_path)

  template <- pkg_file("report", "deployment_report.qmd")
  scss <- pkg_file("report", "report.scss")

  # Quarto renders in place, so work on a copy and keep the package clean.
  work <- file.path(tempdir(), paste0("birdnet_report_", as.integer(Sys.time())))
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  file.copy(c(template, scss), work, overwrite = TRUE)
  qmd <- file.path(work, basename(template))

  # A render failure after a long detector run is expensive, and `quiet = TRUE`
  # hides the reason. Retry once loudly rather than making the user rebuild the
  # bundle to find out what went wrong.
  render <- function(q) {
    quarto::quarto_render(
      input = qmd,
      output_file = filename,
      execute_params = list(bundle = bundle_path),
      quiet = q
    )
  }
  tryCatch(render(quiet), error = function(e) {
    if (!quiet) stop(e)
    message("Report render failed; retrying with output shown.")
    render(FALSE)
  })

  rendered <- file.path(work, filename)
  if (!file.exists(rendered)) {
    stop("Quarto did not produce '", filename, "'. Re-run with quiet = FALSE ",
         "to see why.", call. = FALSE)
  }

  # Top-confidence clips per species, for verification by ear.
  n_clips <- cfg$analysis$export_clips
  if (!is.null(n_clips) && n_clips > 0) {
    clip_dir <- file.path(output_dir, "clips")
    tryCatch({
      export_top_clips(bundle$data, cfg, clip_dir, n_per_species = n_clips,
                       n_species = cfg$analysis$clip_top_species,
                       flag_species = bundle$flagged)
      # One archive to send: the listening sheet only works next to its audio,
      # so the folder has to travel whole.
      zip_clips(clip_dir, file.path(output_dir, paste0(slugify(cfg$name),
                                                       "_clips_for_review.zip")))
    },
      error = function(e) {
        warning("Could not export clips: ", conditionMessage(e), call. = FALSE)
      }
    )
  }
  file.copy(rendered, file.path(output_dir, filename), overwrite = TRUE)

  # Standalone copies of every figure, for slides and manuscripts.
  fig_src <- file.path(work, "figures")
  if (dir.exists(fig_src)) {
    fig_dest <- file.path(output_dir, "figures")
    dir.create(fig_dest, showWarnings = FALSE)
    figs <- list.files(fig_src, pattern = "\\.png$", full.names = TRUE)
    file.copy(figs, fig_dest, overwrite = TRUE)
  }

  out <- file.path(output_dir, filename)
  message("Report written to ", out)
  invisible(out)
}

#' Assemble everything a deployment report needs
#'
#' Internal. Reads and filters the detections, computes the headline numbers,
#' and builds every figure, so the .qmd has no analysis logic of its own.
#' Individual plots are allowed to fail: a report missing one panel is far more
#' useful than no report at all, and the failure is surfaced as a warning and
#' listed in the report's own appendix.
#'
#' @noRd
report_bundle <- function(cfg, data = NULL) {
  conf <- cfg$analysis$confidence %||% 0.5
  lat <- cfg$location$latitude
  lon <- cfg$location$longitude
  tz <- cfg$location$tz

  if (is.null(data)) data <- read_deployment_detections(cfg)
  excluded <- attr(data, "excluded")

  # Optional geographic false-positive filter.
  range_report <- NULL
  raster_path <- cfg$analysis$range_filter$raster
  if (!is.null(raster_path)) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      warning("Range filtering needs the 'terra' package; skipping.", call. = FALSE)
    } else {
      data <- filter_by_range(
        data,
        abundance_raster = raster_path,
        latitude = lat, longitude = lon,
        min_abundance = cfg$analysis$range_filter$min_abundance %||% 0,
        keep_species = cfg$analysis$range_filter$keep_species
      )
      range_report <- attr(data, "range_report")
    }
  }

  # Taxon groups, when the config asks for them and the labels file is there.
  groups <- NULL
  if (!is.null(cfg$analysis$groups) && !is.null(cfg$detector$labels) &&
      file.exists(cfg$detector$labels)) {
    data <- tryCatch(assign_groups(data), error = function(e) {
      warning("Could not assign taxon groups: ", conditionMessage(e), call. = FALSE)
      data
    })
    if ("group" %in% names(data)) groups <- group_colours(cfg)
  }

  # Per-species thresholds where the config has them, the deployment default
  # elsewhere. See species_thresholds().
  above <- apply_thresholds(data, cfg)
  times <- detection_time(above)

  # Species the evaluation tested and could not identify reliably. They are kept
  # so the ecological signal is visible, but the report has to say so — silently
  # mixing them in with validated detections is the one thing that would make
  # the report misleading rather than merely uncertain.
  #
  # Only when something else in the report IS validated, though. Under a single
  # deployment-wide threshold nothing rests on a measured operating point, so
  # flagging a handful of species implies the rest were checked and passed.
  # That is a stronger claim than the report can make, and the wrong few
  # species would carry the doubt.
  per_species <- length(cfg$analysis$confidence_by_species) > 0
  flagged <- if (per_species) {
    sort(unique(above[["Common Name"]][above$threshold_source == "unvalidated"]))
  } else {
    character(0)
  }

  # A site that produced detections but none above threshold disappears from
  # every table and figure without leaving a gap, so the report looks complete
  # while a quarter of the deployment is missing. Usually a failed microphone.
  silent_sites <- setdiff(unique(data$Site), unique(above$Site))
  if (length(silent_sites) > 0) {
    quiet_levels <- vapply(silent_sites, function(s) {
      max(data$Confidence[data$Site == s], na.rm = TRUE)
    }, numeric(1))
    silent_sites <- sprintf("%s (%d detections, none above %.3f; highest %.3f)",
                            silent_sites,
                            vapply(silent_sites,
                                   function(s) sum(data$Site == s), numeric(1)),
                            conf, quiet_levels)
  }

  stats <- list(
    n_sites = length(unique(above$Site)),
    n_detections = nrow(above),
    n_species = length(unique(above[["Common Name"]])),
    n_recordings = dplyr::n_distinct(source_recording(above)),
    first = suppressWarnings(min(times, na.rm = TRUE)),
    last = suppressWarnings(max(times, na.rm = TRUE)),
    confidence = conf,
    n_calibrated = length(unique(
      above[["Common Name"]][above$threshold_source == "calibrated"])),
    per_species = per_species
  )

  per_site <- above |>
    dplyr::mutate(.t = detection_time(above),
                  .rec = source_recording(above)) |>
    dplyr::group_by(Site) |>
    dplyr::summarise(
      detections = dplyr::n(),
      species = dplyr::n_distinct(.data[["Common Name"]]),
      recordings = dplyr::n_distinct(.data$.rec),
      first = min(.data$.t, na.rm = TRUE),
      last = max(.data$.t, na.rm = TRUE),
      .groups = "drop"
    )

  species_table <- above |>
    dplyr::group_by(.data[["Common Name"]]) |>
    dplyr::summarise(
      detections = dplyr::n(),
      sites = dplyr::n_distinct(Site),
      median_confidence = round(stats::median(Confidence), 3),
      max_confidence = round(max(Confidence), 3),
      threshold = round(dplyr::first(threshold), 3),
      basis = dplyr::first(threshold_source),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(detections))

  # With one cut for everything, `threshold` repeats the same number down the
  # page and `basis` says "default" on every row. Both are noise.
  if (!per_species) {
    species_table <- dplyr::select(species_table, -"threshold", -"basis")
  }

  failures <- character(0)
  build <- function(label, expr) {
    tryCatch(expr, error = function(e) {
      msg <- paste0(label, ": ", conditionMessage(e))
      warning("Skipping figure \u2014 ", msg, call. = FALSE)
      failures <<- c(failures, msg)
      NULL
    })
  }

  n_top <- cfg$analysis$n_top_species %||% 10
  min_det <- cfg$analysis$min_detections %||% 1
  sites <- unique(above$Site)

  plots <- list(
    counts = build("species counts", {
      plot_species_ranked(above, n_top = max(n_top, 20),
                          n_total = stats$n_species)
    }),
    trends = build("activity trends", {
      # One line per species means identity is carried by colour alone, and
      # only eight hues stay reliably distinguishable. Past that the chart
      # stops being readable, so cap it rather than inventing more colours.
      n_lines <- min(n_top, length(birdnet_series_colours))
      p <- plot_top_species(
        above, n_top_species = n_lines, confidence_threshold = conf,
        latitude = lat, longitude = lon, tz = tz,
        facet_by = if (length(sites) > 1) "Site" else NULL
      )
      p <- p + ggplot2::scale_colour_manual(
        values = unname(birdnet_palette(n_lines)), name = NULL
      ) + birdnet_theme()
      # plot_top_species() facets with a free y scale. In a report whose point
      # is comparing sites, panels that look alike but are drawn to different
      # maxima invite exactly the wrong reading, so pin them to one scale.
      if (length(sites) > 1) p <- p + ggplot2::facet_wrap(~ Site)
      p
    }),
    activity = build("diel activity", {
      has_groups <- "group" %in% names(above)
      plot_daily_activity(
        above, confidence = conf, min_detections = min_det,
        group_col = if (has_groups) "group" else NULL,
        group_colours = groups,
        lat = lat, lon = lon, tz = tz,
        layout = if (length(sites) > 1) "columns" else "rows",
        fill_palette = birdnet_sequential_colours,
        # The groups are encoded as the colour of the species labels, with no
        # legend box to explain them. Naming each colour in the subtitle is
        # the key, and it keeps working for a reader who can't tell the hues
        # apart.
        subtitle = if (has_groups) group_key(groups, above$group) else NULL
      )
    }),
    confidence = build("confidence by species", {
      plot_confidence_by_species(above, conf)
    })
  )

  list(
    cfg = cfg,
    data = above,
    per_site = per_site,
    species_table = species_table,
    summary = summarise_detections(data, confidence = conf),
    range_report = range_report,
    excluded = excluded,
    stats = stats,
    plots = plots,
    failures = failures,
    flagged = flagged,
    silent_sites = silent_sites,
    n_clips = cfg$analysis$export_clips %||% 0,
    clip_species = min(cfg$analysis$clip_top_species %||% Inf,
                       length(unique(above[["Common Name"]]))),
    clips_zip = paste0(slugify(cfg$name), "_clips_for_review.zip"),
    n_sites_configured = length(cfg$sites),
    threshold_note = cfg$analysis$threshold_note,
    generated = Sys.time()
  )
}

#' Which audio file a detection came from
#'
#' Internal. `file_name` is the name of the *detection table*, which for the
#' BirdNET arm is one per recording and so happens to identify the audio. The
#' perch arm writes a single `perch_predictions.csv` per site, and counting
#' distinct `file_name` there reports a whole deployment as one recording.
#' `Begin Path` carries the real source on every row of both arms.
#'
#' @noRd
source_recording <- function(data) {
  if ("Begin Path" %in% names(data)) {
    return(basename(as.character(data[["Begin Path"]])))
  }
  as.character(data$file_name)
}

#' A text key for group label colours
#'
#' Internal. `plot_daily_activity()` carries taxon groups as the colour of the
#' species labels, which has no legend. Spelling the colours out in words gives
#' the reader a key that survives colour-vision deficiency, greyscale printing
#' and the fact that this is a flat PNG.
#'
#' Only groups actually present in the plotted data are named.
#'
#' @noRd
group_key <- function(colours, present) {
  colour_names <- c(
    "#2a78d6" = "blue", "#eb6834" = "orange", "#1baf7a" = "green",
    "#4a3aa7" = "violet", "#eda100" = "yellow", "#e87ba4" = "pink",
    "#008300" = "green", "#e34948" = "red", "#898781" = "grey"
  )

  used <- names(colours)[names(colours) %in% unique(as.character(present))]
  if (length(used) == 0) return(NULL)

  named <- vapply(used, function(g) {
    hue <- colour_names[[tolower(colours[[g]])]]
    if (is.null(hue)) g else paste0(g, " (", hue, ")")
  }, character(1))

  paste0("Each row scaled to its own peak; grey band = night. ",
         "Species labels: ", paste(named, collapse = ", "), ".")
}

#' Ranked detection counts for the most-detected species
#'
#' Internal. A deployment routinely turns up 100+ classes, most of them with a
#' handful of detections; drawing all of them as vertical bars produces an
#' unreadable picket fence with overlapping labels (which is what
#' \code{\link{plot_species_counts}} does at this scale - it is built for a
#' handful of species). Sorted, one row per species, counted from most to
#' least; the long tail lives in the report's species table.
#'
#' The count axis is logarithmic. A deployment's counts routinely span four
#' orders of magnitude, and on a linear axis the top species flattens every
#' other row against the baseline - true to the numbers, useless for reading
#' the ones below it.
#'
#' That choice rules out bars. A bar encodes magnitude by length from zero, and
#' zero is not on a log axis, so the bars would start at whatever the panel
#' happened to be cut at and their lengths would mean nothing. Position on the
#' axis carries the value instead: a dot for the count, and a hairline back to
#' the axis only so the eye can track across the label gap.
#'
#' @noRd
plot_species_ranked <- function(data, n_top = 20, n_total = NULL) {
  counts <- data |>
    dplyr::count(.data[["Common Name"]], name = "detections", sort = TRUE) |>
    utils::head(n_top)

  if (nrow(counts) == 0) stop("No detections to plot.")

  subtitle <- paste0(
    if (!is.null(n_total) && n_total > nrow(counts)) {
      sprintf("Top %d of %d species; the rest are in the species table below. ",
              nrow(counts), n_total)
    },
    "Note the log scale: each gridline is ten times the last"
  )

  # Ticks at the powers of ten the data actually spans, so the reader can see
  # what the compression is doing rather than having to infer it.
  breaks <- 10^seq(0, ceiling(log10(max(counts$detections))))

  ggplot2::ggplot(counts, ggplot2::aes(
    x = stats::reorder(.data[["Common Name"]], detections),
    y = detections
  )) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = stats::reorder(.data[["Common Name"]], detections)),
      yend = 1, colour = birdnet_ink$grid, linewidth = 0.3
    ) +
    ggplot2::geom_point(colour = birdnet_series_colours[["blue"]], size = 2.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = format(detections, big.mark = ",")),
      hjust = -0.35, size = 3, colour = birdnet_ink$secondary
    ) +
    ggplot2::scale_y_log10(
      breaks = breaks,
      labels = function(x) format(x, big.mark = ",", scientific = FALSE),
      expand = ggplot2::expansion(mult = c(0.02, 0.15))
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Detections (log scale)",
                  title = "Most detected species", subtitle = subtitle) +
    birdnet_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(colour = birdnet_ink$primary,
                                          size = ggplot2::rel(0.85))
    )
}

#' Confidence distribution for the most-detected species
#'
#' Internal. A boxplot per species against the reporting threshold - the
#' quickest read on whether the threshold is in a sensible place for this
#' deployment, and which species are scraping past it.
#'
#' @noRd
plot_confidence_by_species <- function(data, confidence, n_top = 20) {
  top <- data |>
    dplyr::count(.data[["Common Name"]], sort = TRUE) |>
    utils::head(n_top) |>
    dplyr::pull(.data[["Common Name"]])

  d <- data[data[["Common Name"]] %in% top, , drop = FALSE]

  ggplot2::ggplot(d, ggplot2::aes(
    x = stats::reorder(.data[["Common Name"]], Confidence, FUN = stats::median),
    y = Confidence
  )) +
    ggplot2::geom_hline(yintercept = confidence, linetype = "dashed",
                        colour = birdnet_ink$axis) +
    ggplot2::geom_boxplot(outlier.size = 0.4, outlier.colour = birdnet_ink$muted,
                          fill = birdnet_series_colours[["blue"]],
                          colour = birdnet_ink$secondary, linewidth = 0.3,
                          alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL, y = "Confidence",
      title = paste0("Confidence by species (top ", length(top), " by count)"),
      subtitle = "Dashed line marks the reporting threshold"
    ) +
    birdnet_theme()
}

#' @noRd
slugify <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}
