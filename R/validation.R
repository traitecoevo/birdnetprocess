#' Score distribution and high-scoring windows over many score files
#'
#' @description
#' One pass over an all-window score store, keeping two compact summaries:
#' a fine histogram of scores per site and day (enough to place bin edges and
#' count windows per bin for any later grouping of days and sites) and every
#' window scoring at or above `keep_above` (the candidates for review). Files
#' are read one at a time, so stores far larger than memory are fine.
#'
#' @param files Paths to score files readable by [read_birdnet_file()].
#' @param site_fun Function mapping a recording file name to a site name.
#' @param keep_above Keep every window scoring at least this. Default 0.5.
#' @param grid Histogram bin boundaries on the logit scale. Default: 0.05-wide
#'   bins from -15 to 15 (scores outside are clamped into the end bins).
#' @param progress Print a line every this many files (0 for silence).
#'
#' @return A list: `hist` (`Site`, `date`, `bin`, `n`; `bin` indexes `grid`),
#'   `grid`, and `high` (`Site`, `file_name`, `date`, `start_time`,
#'   `begin_time_s`, `end_time_s`, `Confidence`).
#' @export
score_summary_files <- function(files, site_fun = site_from_file, keep_above = 0.5,
                                grid = seq(-15, 15, by = 0.05), progress = 1000) {
  hist <- vector("list", length(files))
  high <- vector("list", length(files))
  logit <- function(p) log(p / (1 - p))
  for (i in seq_along(files)) {
    d <- suppressMessages(read_birdnet_file(files[i]))
    site <- site_fun(basename(files[i]))
    z <- pmin(pmax(logit(pmin(pmax(d$Confidence, 1e-12), 1 - 1e-12)), min(grid)), max(grid) - 1e-9)
    bin <- findInterval(z, grid)
    date <- as.Date(detection_time(d))
    hist[[i]] <- dplyr::count(dplyr::tibble(Site = site, date = date, bin = bin),
                              .data$Site, .data$date, .data$bin, name = "n")
    h <- d[d$Confidence >= keep_above, ]
    if (nrow(h)) {
      high[[i]] <- dplyr::tibble(Site = site, file_name = h$file_name,
                                 date = as.Date(detection_time(h)), start_time = h$start_time,
                                 begin_time_s = h$begin_time_s,
                                 end_time_s = if ("end_time_s" %in% names(h)) h$end_time_s
                                              else h$begin_time_s + 3,
                                 Confidence = h$Confidence)
    }
    if (progress > 0 && i %% progress == 0) message(i, " / ", length(files), " files")
  }
  list(
    hist = dplyr::bind_rows(hist) |>
      dplyr::group_by(.data$Site, .data$date, .data$bin) |>
      dplyr::summarise(n = sum(.data$n), .groups = "drop"),
    grid = grid,
    high = dplyr::bind_rows(high)
  )
}

#' Merge high-scoring windows into calling bouts
#'
#' @description
#' Windows from the same recording whose starts are at most `gap_s` seconds
#' after the previous window's end belong to one bout. A bout is the unit to
#' review: one listen covers all of its windows.
#'
#' @param high Windows, e.g. `score_summary_files()$high`: needs `Site`,
#'   `file_name`, `begin_time_s`, `end_time_s`, `Confidence`.
#' @param gap_s Largest silence, in seconds, allowed inside a bout. Default 6.
#'
#' @return One row per bout: `bout_id`, `Site`, `file_name`, `date`,
#'   `begin_time_s`, `end_time_s`, `n_windows`, `max_score`, `mean_score`.
#' @export
candidate_bouts <- function(high, gap_s = 6) {
  high |>
    dplyr::arrange(.data$file_name, .data$begin_time_s) |>
    dplyr::group_by(.data$file_name) |>
    dplyr::mutate(new = dplyr::coalesce(
      .data$begin_time_s - dplyr::lag(.data$end_time_s) > gap_s, TRUE),
      k = cumsum(.data$new)) |>
    dplyr::group_by(.data$Site, .data$file_name, .data$k) |>
    dplyr::summarise(date = dplyr::first(.data$date),
                     begin_time_s = min(.data$begin_time_s),
                     end_time_s = max(.data$end_time_s),
                     n_windows = dplyr::n(),
                     max_score = max(.data$Confidence),
                     mean_score = mean(.data$Confidence), .groups = "drop") |>
    dplyr::select(-"k") |>
    dplyr::mutate(bout_id = sprintf("B%05d", dplyr::row_number()), .before = 1)
}

#' Logarithmic quantile bin edges from a score histogram
#'
#' @description
#' Navine et al. (2024) bins: bottom 50%, next 25%, next 12.5%, ..., halving
#' until `n_bins` bins, computed from the pooled histogram of
#' [score_summary_files()]. Returned on the score (0-1) scale so the same
#' edges can be applied to every site and period.
#'
#' @param hist,grid `hist` and `grid` from [score_summary_files()] (filter
#'   `hist` first to pool over a subset).
#' @param n_bins Number of bins. Default 10 (top bin = top ~0.2% of windows).
#'
#' @return A tibble: `bin`, `lower`, `upper` (score scale), `share` (nominal
#'   share of pooled windows) and `windows` (pooled windows actually in it).
#' @export
log_quantile_edges <- function(hist, grid, n_bins = 10) {
  pooled <- hist |> dplyr::group_by(.data$bin) |>
    dplyr::summarise(n = sum(.data$n), .groups = "drop") |> dplyr::arrange(.data$bin)
  cum <- cumsum(pooled$n) / sum(pooled$n)
  shares <- c(0.5^(seq_len(n_bins - 1)), 0.5^(n_bins - 1))
  cut_at <- cumsum(shares)[-n_bins]               # 0.5, 0.75, 0.875, ...
  upper_bin <- pooled$bin[vapply(cut_at, function(q) which(cum >= q)[1], integer(1))]
  to_score <- function(z) 1 / (1 + exp(-z))
  edges <- c(0, to_score(grid[upper_bin + 1]), 1)
  out <- dplyr::tibble(bin = seq_len(n_bins), lower = edges[-length(edges)],
                       upper = edges[-1], share = shares)
  lo_bin <- c(0, upper_bin)
  hi_bin <- c(upper_bin, max(pooled$bin))
  out$windows <- vapply(seq_len(n_bins), function(i)
    sum(pooled$n[pooled$bin > lo_bin[i] & pooled$bin <= hi_bin[i]]), numeric(1))
  out
}

#' Stratified random sample of windows by score bin
#'
#' @description
#' Assigns each window to a score bin by `edges` and draws a simple random
#' sample of up to `n_per_bin` windows from every stratum x bin, recording the
#' population size and inclusion probability of each draw, as the Navine et al.
#' (2024) estimator and any weighted calibration fit need.
#'
#' @param windows All windows in the population to sample from: needs
#'   `Confidence` and the `strata` columns.
#' @param edges Bin edges, e.g. from [log_quantile_edges()] (`lower`, `upper`).
#' @param n_per_bin Windows per bin: one number; a named vector by the first
#'   stratum's values (e.g. `c(SiteA = 25, SiteB = 15)`); or an unnamed vector
#'   with one entry per bin (e.g. fewer clips in low bins, more where the
#'   score-to-truth curve changes).
#' @param strata Column(s) to sample within. Default `"Site"`.
#' @param seed Random seed, so the draw can be reproduced.
#'
#' @return The sampled windows with `bin`, `bin_windows` (population in that
#'   stratum x bin), `bin_sampled` and `inclusion_prob`. The full population
#'   table is attached as attribute `population`.
#' @export
sample_windows_by_bin <- function(windows, edges, n_per_bin, strata = "Site", seed = 1) {
  w <- windows
  w$bin <- findInterval(w$Confidence, edges$lower, rightmost.closed = TRUE)
  pop <- dplyr::count(w, dplyr::across(dplyr::all_of(c(strata, "bin"))), name = "bin_windows")
  per_bin <- is.null(names(n_per_bin)) && length(n_per_bin) > 1
  if (per_bin && length(n_per_bin) != nrow(edges)) {
    stop("An unnamed `n_per_bin` must have one entry per bin (", nrow(edges), ").", call. = FALSE)
  }
  quota <- function(key) {
    if (per_bin) return(n_per_bin[[key$bin]])
    if (length(n_per_bin) == 1 && is.null(names(n_per_bin))) return(n_per_bin)
    n_per_bin[[as.character(key[[1]][1])]]
  }
  set.seed(seed)
  picked <- w |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(strata, "bin")))) |>
    dplyr::group_modify(function(g, key) {
      k <- min(nrow(g), quota(key))
      g[sample.int(nrow(g), k), , drop = FALSE]
    }) |>
    dplyr::ungroup() |>
    dplyr::left_join(pop, by = c(strata, "bin")) |>
    dplyr::add_count(dplyr::across(dplyr::all_of(c(strata, "bin"))), name = "bin_sampled") |>
    dplyr::mutate(inclusion_prob = .data$bin_sampled / .data$bin_windows)
  attr(picked, "population") <- pop
  picked
}

#' Cut blinded review clips and write a listening sheet
#'
#' @description
#' Writes one short audio clip per row, centred on the window to be judged, under
#' a random clip ID so the listener sees no site, date, time or score. Rows are
#' shuffled. Two tables come back: a blind listening sheet (clip ID, where the
#' target window sits in the clip, empty columns to fill in) and a key linking
#' each clip ID to its source, which must be kept away from the listener.
#'
#' @param clips Rows to cut: needs `source_path` (the original recording),
#'   `begin_time_s` and `end_time_s` (the target window). Other columns are
#'   carried into the key.
#' @param out_dir Folder for the clips (created if needed).
#' @param pad_s Seconds of context either side of the target window. Default 4.5,
#'   giving 12-s clips for 3-s windows.
#' @param mono Down-mix to one channel? Default `TRUE` (halves the size).
#' @param seed Seed for the shuffle and the clip IDs.
#' @param prefix Clip ID prefix. Default `"C"`.
#'
#' @return A list: `sheet` (blind) and `key` (unblinding), also written as
#'   `listening_sheet.csv` in `out_dir` and returned for the caller to store the
#'   key elsewhere. Needs `ffmpeg` on the PATH.
#' @export
cut_review_clips <- function(clips, out_dir, pad_s = 4.5, mono = TRUE, seed = 1,
                             prefix = "C") {
  if (!nzchar(Sys.which("ffmpeg"))) stop("ffmpeg not found on the PATH.", call. = FALSE)
  dir.create(file.path(out_dir, "clips"), recursive = TRUE, showWarnings = FALSE)
  set.seed(seed)
  key <- clips[sample.int(nrow(clips)), , drop = FALSE]
  key$clip_id <- sprintf("%s%04d", prefix, seq_len(nrow(key)))
  key$clip_start_s <- pmax(0, key$begin_time_s - pad_s)
  key$target_start_in_clip_s <- key$begin_time_s - key$clip_start_s
  key$target_end_in_clip_s <- key$end_time_s - key$clip_start_s
  key$clip_file <- file.path("clips", paste0(key$clip_id, ".wav"))
  key$cut_ok <- FALSE
  for (i in seq_len(nrow(key))) {
    out <- file.path(out_dir, key$clip_file[i])
    if (file.exists(out) && file.size(out) > 0) { key$cut_ok[i] <- TRUE; next }
    dur <- (key$end_time_s[i] - key$begin_time_s[i]) + 2 * pad_s
    status <- suppressWarnings(system2(
      "ffmpeg",
      c("-v", "error", "-y", "-ss", format(key$clip_start_s[i], scientific = FALSE),
        "-t", format(dur, scientific = FALSE), "-i", shQuote(key$source_path[i]),
        if (mono) c("-ac", "1"), shQuote(out)),
      stdout = FALSE, stderr = FALSE))
    key$cut_ok[i] <- identical(status, 0L) && file.exists(out)
    if (!key$cut_ok[i]) warning("ffmpeg failed for ", key$clip_id[i], call. = FALSE)
  }
  sheet <- dplyr::tibble(
    clip_id = key$clip_id, clip_file = key$clip_file,
    target_start_s = round(key$target_start_in_clip_s, 2),
    target_end_s = round(key$target_end_in_clip_s, 2),
    label = "", call_start_s = "", call_end_s = "", n_calls = "", call_type = "",
    certainty = "", notes = ""
  )
  utils::write.csv(sheet, file.path(out_dir, "listening_sheet.csv"), row.names = FALSE)
  list(sheet = sheet, key = key)
}
