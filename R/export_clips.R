#' Export the highest-confidence audio clips for each species
#'
#' @description
#' Cuts the top-scoring detections of every species out of the source
#' recordings and writes them to a folder, one subfolder per species. This is
#' the fastest way to check whether a detector is right: a species table says a
#' bird was heard 200 times, and five clips say whether it was that bird.
#'
#' @details
#' Verification is the step a detection report cannot do for you, and the
#' friction of finding the audio by hand is usually what stops it happening. A
#' species' best clips are also its most flattering, so treat this as a
#' first-pass sanity check — if the top five are wrong, the species is wrong;
#' if they are right, the marginal ones still need listening to.
#'
#' Clip filenames carry the confidence, site and timestamp, so a folder can be
#' skimmed without opening the manifest.
#'
#' Source recordings are located by file name against each site's `recordings`
#' folder in the config, not by the path recorded in the detections. Detector
#' runs analyse a temporary folder of symlinks that is deleted afterwards, so
#' the path stored with a detection is generally dead by the time anyone wants
#' the audio.
#'
#' Requires `ffmpeg` on the PATH.
#'
#' @param data Detections, e.g. from \code{\link{read_deployment_detections}}.
#'        Needs `Common Name`, `Confidence`, and either `Begin Path` or
#'        `file_name` to identify the source recording.
#' @param cfg A `birdnet_deployment`, used to find the recordings.
#' @param output_dir Folder to write clips into. Created if needed.
#' @param n_per_species Number of clips per species. Default `5`.
#' @param pad Seconds of context before and after the detection. Default `1`.
#' @param species Optional character vector limiting which species to export.
#' @param n_species Export only the `n_species` most-detected species. `NULL`
#'        (the default) exports every species. Ignored if `species` is given.
#' @param flag_species Species to mark as unvalidated on the listening sheet and
#'        sort to the top. Default none.
#'
#' @return A tibble describing every clip written, invisibly. Also written to
#'         `clips.csv` in `output_dir`. The number of species in the report it
#'         was drawn from is attached as `attr(x, "n_species_total")`.
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#' cfg <- read_deployment("deployments/wild_deserts/deployment.yml")
#' d <- read_deployment_detections(cfg)
#' export_top_clips(d, cfg, "report/clips", n_per_species = 5)
#' }
export_top_clips <- function(data, cfg, output_dir,
                             n_per_species = 5, pad = 1, species = NULL,
                             n_species = NULL, flag_species = character(0)) {
  if (!nzchar(Sys.which("ffmpeg"))) {
    stop("ffmpeg not found on the PATH; it is needed to cut clips.", call. = FALSE)
  }
  if (nrow(data) == 0) {
    warning("No detections to export.", call. = FALSE)
    return(invisible(dplyr::tibble()))
  }
  n_total <- dplyr::n_distinct(data[["Common Name"]])

  if (!is.null(species)) {
    data <- data[data[["Common Name"]] %in% species, , drop = FALSE]
  } else if (!is.null(n_species) && n_species < n_total) {
    # A folder of 70 species is a listening job nobody finishes, and the tail
    # is where the detections are thinnest anyway. Cut to the species whose
    # answer moves the report most.
    keep <- data |>
      dplyr::count(.data[["Common Name"]], sort = TRUE) |>
      utils::head(n_species) |>
      dplyr::pull(.data[["Common Name"]])
    data <- data[data[["Common Name"]] %in% keep, , drop = FALSE]
  }

  index <- recording_index(cfg)
  if (length(index) == 0) {
    stop("No source recordings found. Check the sites' 'recordings' paths.",
         call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  top <- data |>
    dplyr::group_by(.data[["Common Name"]]) |>
    dplyr::arrange(dplyr::desc(Confidence), .by_group = TRUE) |>
    dplyr::slice_head(n = n_per_species) |>
    dplyr::ungroup()

  offsets <- clip_offsets(top)
  wav_names <- basename(as.character(
    if ("Begin Path" %in% names(top)) top[["Begin Path"]] else top$file_name
  ))
  sources <- unname(index[wav_names])

  written <- vector("list", nrow(top))
  missing <- character(0)

  for (i in seq_len(nrow(top))) {
    src <- sources[i]
    if (is.na(src)) {
      missing <- c(missing, wav_names[i])
      next
    }
    sp <- top[[i, "Common Name"]]
    folder <- file.path(output_dir, slugify(sp))
    dir.create(folder, showWarnings = FALSE)

    conf <- top$Confidence[i]
    site <- if ("Site" %in% names(top)) top$Site[i] else "site"
    stamp <- sub("\\.wav$", "", wav_names[i], ignore.case = TRUE)
    rank <- sum(top[["Common Name"]][seq_len(i)] == sp)
    out <- file.path(folder, sprintf(
      "%02d_conf%03d_%s_%s_%ds.wav",
      rank, round(conf * 100), slugify(site), stamp, round(offsets$start[i])
    ))

    start <- max(0, offsets$start[i] - pad)
    dur <- (offsets$end[i] - offsets$start[i]) + 2 * pad

    status <- suppressWarnings(system2(
      "ffmpeg",
      c("-v", "error", "-y", "-ss", format(start, scientific = FALSE),
        "-t", format(dur, scientific = FALSE),
        "-i", shQuote(src), shQuote(out)),
      stdout = FALSE, stderr = FALSE
    ))
    if (!identical(status, 0L) || !file.exists(out)) {
      warning("ffmpeg failed for ", basename(out), call. = FALSE)
      next
    }

    written[[i]] <- dplyr::tibble(
      species = sp, rank = rank, confidence = conf, site = site,
      source = src, offset_s = offsets$start[i], clip = out
    )
  }

  written <- dplyr::bind_rows(written)

  if (length(missing) > 0) {
    warning("Could not locate ", length(unique(missing)),
            " source recording(s), e.g. ", missing[1],
            ". Clips for those detections were skipped.", call. = FALSE)
  }
  if (nrow(written) > 0) {
    attr(written, "n_species_total") <- n_total
    prune_stale_clips(output_dir, unique(written$species))
    readr::write_csv(written, file.path(output_dir, "clips.csv"))
    write_verification_sheet(written, data, cfg, output_dir, n_total,
                             flag_species = flag_species)
    message("Wrote ", nrow(written), " clips for ",
            dplyr::n_distinct(written$species), " species",
            if (dplyr::n_distinct(written$species) < n_total) {
              paste0(" (of ", n_total, " in the report)")
            }, " to ", output_dir)
  }
  invisible(written)
}

#' A listening sheet to send with the clips
#'
#' Internal. The clips folder alone asks a lot of whoever opens it: 70-odd
#' folders of wavs with no indication of which ones matter or what to do with
#' an opinion once formed. This writes an `index.html` beside them - one card
#' per species, its clips playable in place, and a verdict to record - so the
#' ask becomes "open this file and click through it".
#'
#' Everything is inline and every path is relative, so the folder can be zipped
#' and emailed and still work offline from `file://`. Answers persist in
#' `localStorage` (a reviewer can stop halfway) and come back out as a CSV that
#' joins to `clips.csv` on species.
#'
#' Species the config could not validate are listed first: they are where a
#' human ear changes the answer most.
#'
#' @noRd
write_verification_sheet <- function(written, data, cfg, output_dir,
                                     n_species_total = NULL,
                                     flag_species = character(0)) {
  per_species <- data |>
    dplyr::group_by(species = .data[["Common Name"]]) |>
    dplyr::summarise(detections = dplyr::n(), .groups = "drop")

  w <- written |>
    dplyr::left_join(per_species, by = "species") |>
    dplyr::mutate(flagged = .data$species %in% flag_species) |>
    dplyr::arrange(dplyr::desc(.data$flagged), dplyr::desc(.data$detections),
                   .data$species, .data$rank)

  cards <- vapply(split(w, factor(w$species, levels = unique(w$species))),
                  species_card, character(1))

  n_flagged <- dplyr::n_distinct(w$species[w$flagged])
  n_shown <- dplyr::n_distinct(w$species)

  # Token substitution rather than sprintf: the template is full of CSS
  # percentages, and every one of them would be a format directive.
  html <- fill_template(
    readLines(pkg_file("report", "verification.html"), warn = FALSE),
    TITLE = escape_html(cfg$name),
    PLACE = escape_html(cfg$location$place %||% ""),
    N_SPECIES = format(n_shown),
    N_CLIPS = format(nrow(w)),
    # The sheet must not read as the whole report when it is the head of it,
    # or "all correct" gets taken for a verdict on species nobody heard.
    SCOPE_NOTE = if (!is.null(n_species_total) && n_species_total > n_shown) {
      sprintf(paste("These are the %d most-detected of the %d species in the",
                    "report; the remaining %d are not covered here."),
              n_shown, n_species_total, n_species_total - n_shown)
    } else {
      "That is every species in the report."
    },
    FLAG_NOTE = if (n_flagged > 0) {
      sprintf(paste("%d of them could not be validated against labelled audio,",
                    "so they are listed first \u2014 those are the ones where a",
                    "human ear changes the answer."), n_flagged)
    } else {
      ""
    },
    SLUG = escape_html(slugify(cfg$name)),
    CARDS = paste(cards, collapse = "\n")
  )

  writeLines(html, file.path(output_dir, "index.html"))
  invisible(file.path(output_dir, "index.html"))
}

#' @noRd
species_card <- function(d) {
  sp <- d$species[1]
  id <- slugify(sp)
  flag <- if (isTRUE(d$flagged[1])) {
    '<span class="flag">not validated</span>'
  } else {
    ""
  }

  clips <- vapply(seq_len(nrow(d)), function(i) {
    sprintf(
      '<li class="clip"><div class="meta"><span class="conf">%.3f</span>
       <span class="site">%s</span></div>
       <audio controls preload="none" src="%s"></audio></li>',
      d$confidence[i], escape_html(as.character(d$site[i])),
      escape_html(relative_clip_path(d$clip[i]))
    )
  }, character(1))

  sprintf(
    '<section class="card" data-species="%s" data-flagged="%s" id="sp-%s">
<header><h2>%s %s</h2>
<p class="count">%s detections in the report &middot; %d clips below, highest confidence first</p></header>
<ul class="clips">%s</ul>
<div class="verdict">
<label><input type="radio" name="v-%s" value="correct"> Correct</label>
<label><input type="radio" name="v-%s" value="wrong"> Wrong</label>
<label><input type="radio" name="v-%s" value="mixed"> Mixed</label>
<label><input type="radio" name="v-%s" value="unsure"> Unsure</label>
<input type="text" class="note" data-species="%s" placeholder="Notes (what it actually is, which clips)">
</div>
</section>',
    escape_html(sp), tolower(as.character(isTRUE(d$flagged[1]))), id,
    escape_html(sp), flag,
    format(d$detections[1], big.mark = ","), nrow(d),
    paste(clips, collapse = ""),
    id, id, id, id, escape_html(sp)
  )
}

#' Drop species folders left behind by an earlier export
#'
#' Internal. The clips folder is rewritten in place on every render, so a
#' species dropped since the last one - excluded in the config, filtered out by
#' range, or pushed under its threshold - leaves its folder sitting there. It
#' then travels in the archive and gets listened to, which is precisely what
#' excluding it was meant to prevent.
#'
#' @noRd
prune_stale_clips <- function(output_dir, species) {
  keep <- vapply(species, slugify, character(1))
  present <- list.dirs(output_dir, full.names = FALSE, recursive = FALSE)
  stale <- setdiff(present, keep)
  if (length(stale) == 0) return(invisible(character(0)))

  unlink(file.path(output_dir, stale), recursive = TRUE)
  message("Removed ", length(stale), " clip folder(s) for species no longer ",
          "in the report.")
  invisible(stale)
}

#' Bundle the clips folder into one archive to send
#'
#' Internal. Zipping is done from the folder's parent with a relative path, so
#' the archive expands to `clips/` rather than to a chain of directories from
#' the root of someone else's machine.
#'
#' A missing `zip` binary is not worth failing a report over - the folder is
#' still there to compress by hand.
#'
#' @noRd
zip_clips <- function(clip_dir, zip_path) {
  if (!dir.exists(clip_dir)) return(invisible(NULL))
  if (!nzchar(Sys.which("zip"))) {
    message("No 'zip' on the PATH; clips left as a folder.")
    return(invisible(NULL))
  }
  if (file.exists(zip_path)) unlink(zip_path)

  wd <- setwd(dirname(clip_dir))
  on.exit(setwd(wd), add = TRUE)
  status <- suppressWarnings(utils::zip(
    normalizePath(zip_path, mustWork = FALSE),
    basename(clip_dir), flags = "-rq"
  ))
  if (!identical(status, 0L)) {
    warning("Could not zip the clips folder.", call. = FALSE)
    return(invisible(NULL))
  }
  message("Clips archived to ", zip_path)
  invisible(zip_path)
}

#' @noRd
fill_template <- function(lines, ...) {
  values <- list(...)
  out <- paste(lines, collapse = "\n")
  for (nm in names(values)) {
    out <- gsub(paste0("{{", nm, "}}"), values[[nm]], out, fixed = TRUE)
  }
  out
}

#' @noRd
relative_clip_path <- function(path) {
  parts <- strsplit(path, .Platform$file.sep, fixed = TRUE)[[1]]
  paste(utils::tail(parts, 2), collapse = "/")
}

#' @noRd
escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

#' Map recording file names to their location on disk
#'
#' Internal. Detector runs read a temporary folder of symlinks which is deleted
#' afterwards, so `Begin Path` in a detection table usually points at something
#' that no longer exists. The config knows where the real recordings live, so
#' resolve by file name instead.
#'
#' @noRd
recording_index <- function(cfg) {
  paths <- unlist(lapply(cfg$sites, function(s) {
    if (is.null(s$recordings) || !dir.exists(s$recordings)) {
      return(character(0))
    }
    list.files(s$recordings, pattern = "\\.(wav|flac|mp3|ogg)$",
               full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  }))
  stats::setNames(paths, basename(paths))
}

#' Start and end of a detection within its source recording
#'
#' Internal. For a combined export (one table spanning many recordings) the
#' within-file offset is `File Offset (s)`; `begin_time_s` is cumulative across
#' the concatenated files and would seek to the wrong place. Falls back to
#' `begin_time_s` for per-recording tables, where the two are the same.
#'
#' @noRd
clip_offsets <- function(data) {
  start <- if ("File Offset (s)" %in% names(data)) {
    as.numeric(data[["File Offset (s)"]])
  } else {
    as.numeric(data$begin_time_s)
  }
  len <- if ("end_time_s" %in% names(data)) {
    as.numeric(data$end_time_s) - as.numeric(data$begin_time_s)
  } else {
    rep(NA_real_, nrow(data))
  }
  len[is.na(len) | len <= 0] <- 3
  list(start = start, end = start + len)
}
