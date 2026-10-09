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

#' Score-to-true-positive curve from labelled windows
#'
#' @description
#' Logistic regression of verified label on logit(score): the probability
#' that a window with a given detector score truly contains the target. Within
#' score bins the windows were sampled at random, so the conditional curve
#' needs no weights. Also returns the observed share per bin with Wilson 95%
#' intervals, and the scores at which the curve crosses 0.5 and 0.9.
#'
#' @param labelled Data frame with `Confidence` and a logical `tp` (unsure
#'   labels removed or recoded beforehand). An optional `bin` column gives the
#'   per-bin summary.
#' @return A list: `fit` (glm), `curve` (`score`, `p`, `lo`, `hi`),
#'   `bins` (`bin`, `n`, `tp`, `share`, `lo`, `hi`, `mid_score`) and
#'   `thresholds` (scores where p = 0.5 and 0.9).
#' @export
fit_score_calibration <- function(labelled) {
  d <- labelled
  d$z <- stats::qlogis(pmin(pmax(d$Confidence, 1e-6), 1 - 1e-6))
  fit <- stats::glm(tp ~ z, family = stats::binomial, data = d)
  zs <- seq(min(d$z), max(d$z), length.out = 200)
  pr <- stats::predict(fit, newdata = data.frame(z = zs), se.fit = TRUE)
  curve <- dplyr::tibble(score = stats::plogis(zs), p = stats::plogis(pr$fit),
                         lo = stats::plogis(pr$fit - 1.96 * pr$se.fit),
                         hi = stats::plogis(pr$fit + 1.96 * pr$se.fit))
  wilson <- function(k, n) {
    p <- k / n; z <- 1.96; den <- 1 + z^2 / n
    c((p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / den,
      (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / den)
  }
  bins <- NULL
  if ("bin" %in% names(d)) {
    bins <- d |> dplyr::group_by(.data$bin) |>
      dplyr::summarise(n = dplyr::n(), tp = sum(.data$tp),
                       mid_score = stats::median(.data$Confidence), .groups = "drop") |>
      dplyr::mutate(share = .data$tp / .data$n,
                    lo = mapply(function(k, n) wilson(k, n)[1], .data$tp, .data$n),
                    hi = mapply(function(k, n) wilson(k, n)[2], .data$tp, .data$n))
  }
  b <- stats::coef(fit)
  at <- function(p) stats::plogis((stats::qlogis(p) - b[1]) / b[2])
  list(fit = fit, curve = curve, bins = bins,
       thresholds = c(p50 = unname(at(0.5)), p90 = unname(at(0.9))))
}

#' Plot a score-to-true-positive curve
#'
#' @param cal Output of [fit_score_calibration()].
#' @param title,subtitle Plot title and subtitle.
#' @return A `ggplot` object: the fitted curve with its 95% band, observed bin
#'   shares with Wilson intervals, and the labelled windows as a rug.
#' @export
plot_score_calibration <- function(cal, title = NULL, subtitle = NULL) {
  d <- cal$fit$data
  p <- ggplot2::ggplot(cal$curve, ggplot2::aes(.data$score, .data$p)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                         fill = birdnet_palette(1), alpha = 0.15) +
    ggplot2::geom_line(colour = birdnet_palette(1), linewidth = 0.9) +
    ggplot2::geom_rug(data = d[d$tp, ], ggplot2::aes(.data$Confidence, y = 1), sides = "t",
                      colour = birdnet_ink$secondary, alpha = 0.5, inherit.aes = FALSE) +
    ggplot2::geom_rug(data = d[!d$tp, ], ggplot2::aes(.data$Confidence, y = 0), sides = "b",
                      colour = birdnet_ink$secondary, alpha = 0.5, inherit.aes = FALSE)
  if (!is.null(cal$bins)) {
    p <- p +
      ggplot2::geom_linerange(data = cal$bins, ggplot2::aes(.data$mid_score, ymin = .data$lo, ymax = .data$hi),
                              colour = birdnet_ink$primary, linewidth = 0.4, inherit.aes = FALSE) +
      ggplot2::geom_point(data = cal$bins, ggplot2::aes(.data$mid_score, .data$share),
                          colour = birdnet_ink$primary, size = 2, inherit.aes = FALSE) +
      ggplot2::geom_text(data = cal$bins, ggplot2::aes(.data$mid_score, .data$share,
                                                       label = paste0(.data$tp, "/", .data$n)),
                         vjust = -1, size = 2.6, colour = birdnet_ink$secondary, inherit.aes = FALSE)
  }
  p +
    ggplot2::scale_x_continuous(transform = "logit",
                                breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 0.7, 0.9, 0.99)) +
    ggplot2::scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(x = "detector score (logit scale)", y = "probability the window is a true call",
                  title = title, subtitle = subtitle) +
    birdnet_theme(base_size = 10)
}

#' Calibrated detection density over score files
#'
#' @description
#' Applies a score-to-true-positive curve from [fit_score_calibration()] to
#' every window of every file and sums the probabilities, giving the expected
#' number of true call windows per site and time bin instead of a count above
#' an arbitrary threshold. Only valid where the curve was fitted: the
#' probability that a score is a true call depends on how common calls are,
#' so a curve fitted where calls were dense overstates them where calls are
#' rare. Restrict `files` to the sites and dates the calibration sample
#' covered.
#'
#' @param files Score files readable by [read_birdnet_file()].
#' @param cal Output of [fit_score_calibration()].
#' @param floor Windows scoring below this contribute nothing. Use it to drop
#'   the low-score range where the curve rests on few labels (a lower bound).
#' @param windows_per_call Divide expected true windows by this to get calls
#'   (a call can span two windows). Default 1 (windows).
#' @param unit `"hour"` (default) or `"day"`.
#' @param site_fun Function mapping a file name to a site name.
#'
#' @return A tibble like [detection_density()]: `Site`, `time`, `hours`, and
#'   `detections` holding the expected number of true calls (not an integer),
#'   with `rate` per recorded hour.
#' @export
calibrated_density_files <- function(files, cal, floor = 0, windows_per_call = 1,
                                     unit = c("hour", "day"), site_fun = site_from_file) {
  unit <- match.arg(unit)
  b <- stats::coef(cal$fit)
  out <- vector("list", length(files))
  for (i in seq_along(files)) {
    d <- suppressMessages(read_birdnet_file(files[i]))
    z <- stats::qlogis(pmin(pmax(d$Confidence, 1e-6), 1 - 1e-6))
    p <- ifelse(d$Confidence >= floor, stats::plogis(b[1] + b[2] * z), 0)
    t <- detection_time(d)
    out[[i]] <- dplyr::tibble(
      Site = site_fun(basename(files[i])),
      time = if (unit == "day") as.Date(t) else lubridate::floor_date(t, "hour"),
      p = p, window = 1
    ) |>
      dplyr::group_by(.data$Site, .data$time) |>
      dplyr::summarise(expected = sum(.data$p), windows = sum(.data$window), .groups = "drop")
  }
  window_s <- 3
  dplyr::bind_rows(out) |>
    dplyr::group_by(.data$Site, .data$time) |>
    dplyr::summarise(expected = sum(.data$expected), windows = sum(.data$windows), .groups = "drop") |>
    dplyr::mutate(hours = .data$windows * window_s / 3600,
                  detections = .data$expected / windows_per_call,
                  rate = .data$detections / .data$hours) |>
    dplyr::select("Site", "time", "hours", "detections", "rate")
}
