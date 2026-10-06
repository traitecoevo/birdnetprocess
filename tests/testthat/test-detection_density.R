# An all-window table: every 3-s window has a row, whatever its score.
all_window_table <- function() {
  one_file <- function(file_name, scores) {
    start <- parse_birdnet_filename_datetime(file_name)
    dplyr::tibble(
      file_name = file_name,
      begin_time_s = seq(0, by = 3, length.out = length(scores)),
      end_time_s = begin_time_s + 3,
      `Common Name` = "Laughing Kookaburra",
      Confidence = scores,
      start_time = start,
      recording_window_time = start + begin_time_s
    )
  }
  dplyr::bind_rows(
    one_file("SiteA_20260501_060000.wav", c(rep(0.01, 1196), 0.95, 0.92, 0.6, 0.3)),
    one_file("SiteA_20260502_060000.wav", rep(0.01, 1200)),
    one_file("SiteB_20260501_060000.wav", c(rep(0.02, 600), 0.99))
  )
}

test_that("detection_density divides by recorded hours and keeps quiet days", {
  dens <- detection_density(all_window_table(), confidence = 0.9)

  a1 <- dens[dens$Site == "SiteA" & dens$time == as.Date("2026-05-01"), ]
  expect_equal(a1$hours, 1)
  expect_equal(a1$detections, 2L)
  expect_equal(a1$rate, 2)

  # Recorded but silent: a zero, not a missing row.
  a2 <- dens[dens$Site == "SiteA" & dens$time == as.Date("2026-05-02"), ]
  expect_equal(nrow(a2), 1)
  expect_equal(a2$detections, 0L)

  b1 <- dens[dens$Site == "SiteB", ]
  expect_equal(b1$hours, 601 * 3 / 3600)
  expect_equal(b1$detections, 1L)
})

test_that("detection_density bins by hour", {
  dens <- detection_density(all_window_table(), confidence = 0.5, unit = "hour")
  expect_s3_class(dens$time, "POSIXct")
  expect_equal(sum(dens$detections), 4L)
})

test_that("detection_density warns on a thresholded table without effort", {
  thresholded <- all_window_table() |> dplyr::filter(Confidence >= 0.5)
  expect_warning(detection_density(thresholded), "thresholded")
})

test_that("detection_density uses a supplied effort table", {
  thresholded <- all_window_table() |> dplyr::filter(Confidence >= 0.5)
  effort <- dplyr::tibble(
    Site = c("SiteA", "SiteA", "SiteB"),
    time = as.Date(c("2026-05-01", "2026-05-02", "2026-05-01")),
    hours = c(1, 1, 0.5)
  )
  dens <- detection_density(thresholded, confidence = 0.9, effort = effort)
  expect_equal(nrow(dens), 3)
  expect_equal(dens$rate[dens$Site == "SiteB"], 2)
  expect_equal(dens$detections[dens$time == as.Date("2026-05-02")], 0L)
})

test_that("read_birdnet_file reads a Parquet score store", {
  skip_if_not_installed("nanoparquet")
  path <- file.path(withr::local_tempdir(), "SiteA_20260501_060000.BirdNET.results.parquet")
  nanoparquet::write_parquet(
    data.frame(`Start (s)` = c(0, 3), `End (s)` = c(3, 6),
               `Common name` = "Laughing Kookaburra", Confidence = c(0.1, 0.9),
               check.names = FALSE),
    path
  )
  d <- read_birdnet_file(path)
  expect_equal(d$begin_time_s, c(0, 3))
  expect_true("Common Name" %in% names(d))
  expect_equal(d$recording_window_time[2],
               lubridate::ymd_hms("2026-05-01 06:00:03", tz = "UTC"))
})

test_that("diel_density folds windows onto sunrise and sunset", {
  skip_if_not_installed("suncalc")
  coords <- dplyr::tibble(Site = c("SiteA", "SiteB"), latitude = -31, longitude = 141.8)
  tz <- "Australia/Sydney"
  rise <- suncalc::getSunlightTimes(as.Date("2026-05-01"), -31, 141.8,
                                    keep = "sunrise")$sunrise
  clock <- lubridate::force_tz(lubridate::with_tz(rise, tz), "UTC")
  # One window 10 min after sunrise scores high; the rest of the hour is quiet.
  start <- clock - 3600
  w <- dplyr::tibble(
    file_name = "SiteA_20260501_060000.wav",
    begin_time_s = seq(0, by = 3, length.out = 2400),
    end_time_s = begin_time_s + 3,
    `Common Name` = "Laughing Kookaburra",
    Confidence = ifelse(begin_time_s == 3600 + 600, 0.95, 0.01),
    start_time = start,
    recording_window_time = start + begin_time_s
  )
  diel <- diel_density(w, coords, tz = tz, confidence = 0.9, bin_min = 5)
  expect_true(all(diel$anchor == "sunrise"))
  hit <- diel[diel$detections > 0, ]
  expect_equal(hit$offset_min, 10)
  expect_equal(hit$hours, 100 * 3 / 3600)
  expect_equal(sum(diel$hours), 2)
  expect_s3_class(plot_diel_density(diel, min_days = 1), "ggplot")
  by_day <- diel_density(w, coords, tz = tz, confidence = 0.9, bin_min = 5, by_date = TRUE)
  expect_true("date" %in% names(by_day))
  expect_equal(sum(by_day$detections), 1L)
})

test_that("plot_site_map sizes by log total and separates zero from no recording", {
  totals <- dplyr::tibble(
    Site = c("A", "B", "C", "D"),
    latitude = c(-31.00, -31.01, -31.02, -31.03),
    longitude = c(141.80, 141.81, 141.82, 141.83),
    detections = c(1500L, 12L, 0L, 0L),
    hours = c(100, 100, 100, 0)
  )
  p <- plot_site_map(totals, labels = FALSE, scale_bar = FALSE)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), 2)  # zero + no-recording symbols
  expect_equal(nrow(built$data[[2]]), 2)  # sized points
  expect_error(plot_site_map(totals[, -2]), "latitude")
})

test_that("weather_by_session summarises the hours around sunrise and sunset", {
  skip_if_not_installed("suncalc")
  times <- seq(as.POSIXct("2026-05-01 00:00", tz = "UTC"),
               as.POSIXct("2026-05-02 23:00", tz = "UTC"), by = "hour")
  hour <- as.integer(format(times, "%H"))
  w <- dplyr::tibble(
    time = times,
    temperature_2m = ifelse(hour < 12, 5, 20),
    precipitation = ifelse(hour == 17, 2, 0),
    wind_gusts_10m = hour,
    surface_pressure = 1000 + seq_along(times) / 10
  )
  s <- weather_by_session(w, -31, 141.8, tz = "Australia/Sydney")
  expect_equal(nrow(s), 4)
  expect_equal(levels(s$session), c("dawn", "dusk"))
  expect_true(all(s$temperature_2m[s$session == "dawn"] == 5))
  expect_true(all(s$temperature_2m[s$session == "dusk"] == 20))
  expect_equal(s$precipitation[s$session == "dusk"], c(2, 2))  # sum in window
  expect_equal(s$rain_day[s$session == "dawn"], c(2, 2))
  expect_true(all(s$moon_illumination >= 0 & s$moon_illumination <= 1))
  expect_equal(s$pressure_change_24h[s$date == as.Date("2026-05-02")][1], 2.4)
})

test_that("density_by_session splits hours at noon", {
  dens <- detection_density(all_window_table(), confidence = 0.5, unit = "hour")
  ses <- density_by_session(dens)
  expect_true(all(ses$session == "dawn"))
  expect_equal(sum(ses$detections), 4L)
})

test_that("read_bom_dwo parses a BOM daily observations file", {
  path <- test_path("fixtures", "bom_dwo_example.csv")
  w <- read_bom_dwo(path)
  expect_equal(w$date[1], as.Date("2026-06-01"))
  expect_equal(w$tmin[1], 7.0)
  expect_equal(w$wind_9am[6], 0)  # "Calm"
  expect_equal(w$pressure_3pm[2], 1003.7)
  expect_equal(w$rain_day[1], w$rain_prev_day_to_9am[2])
})

test_that("session_anomaly removes each session's seasonal trend", {
  dates <- rep(seq(as.Date("2026-05-01"), by = "day", length.out = 60), 2)
  w <- dplyr::tibble(date = dates, session = rep(c("dawn", "dusk"), each = 60),
                     temperature_2m = ifelse(session == "dawn", 5, 20) -
                       as.numeric(date - min(date)) / 10)
  a <- session_anomaly(w, "temperature_2m", df = 1)
  expect_true(all(abs(a$temperature_2m_anomaly) < 1e-8))  # linear trend fully removed
  a0 <- session_anomaly(w, "temperature_2m", df = 0)
  expect_equal(mean(a0$temperature_2m_anomaly[a0$session == "dawn"]), 0)
})

test_that("local_sun_times returns the event on each LOCAL date, whatever suncalc's convention", {
  skip_if_not_installed("suncalc")
  d <- as.Date(c("2026-05-01", "2026-12-25"))
  s <- local_sun_times(d, -31, 141.8, "Australia/Sydney")
  expect_equal(as.Date(s$sunrise), d)
  expect_equal(as.Date(s$sunset), d)
  expect_true(all(s$sunrise < s$sunset))
  expect_equal(nrow(local_sun_times(as.Date(character(0)), -31, 141.8, "UTC")), 0)
})

test_that("density_by_night joins an evening with the next morning", {
  t <- as.POSIXct(c("2026-05-01 17:00", "2026-05-02 06:00", "2026-05-02 17:00"), tz = "UTC")
  dens <- dplyr::tibble(Site = "A", time = t, `Common Name` = "x",
                        hours = 1, detections = c(2L, 3L, 5L), rate = c(2, 3, 5))
  n <- density_by_night(dens)
  expect_equal(n$time, as.Date(c("2026-05-01", "2026-05-02")))
  expect_equal(n$detections, c(5L, 5L))
  expect_equal(n$hours, c(2, 1))
})

test_that("detection_density_files matches detection_density on the same data", {
  skip_if_not_installed("nanoparquet")
  dir <- withr::local_tempdir()
  w <- all_window_table()
  for (f in unique(w$file_name)) {
    x <- w[w$file_name == f, ]
    nanoparquet::write_parquet(
      data.frame(`Start (s)` = x$begin_time_s, `End (s)` = x$end_time_s,
                 `Common name` = x$`Common Name`, Confidence = x$Confidence, check.names = FALSE),
      file.path(dir, sub("\\.wav$", ".BirdNET.results.parquet", f)))
  }
  a <- detection_density_files(list.files(dir, full.names = TRUE), confidence = 0.9, progress = 0)
  b <- detection_density(w, confidence = 0.9)
  expect_equal(a$detections, b$detections)
  expect_equal(a$hours, b$hours)
})

test_that("candidate_bouts merges windows separated by short gaps", {
  high <- dplyr::tibble(Site = "A", file_name = "A_20260501_060000.wav",
                        date = as.Date("2026-05-01"),
                        begin_time_s = c(0, 3, 12, 30), end_time_s = c(3, 6, 15, 33),
                        Confidence = c(0.6, 0.9, 0.7, 0.8))
  b <- candidate_bouts(high, gap_s = 6)
  expect_equal(nrow(b), 2)          # 0-15 s (gap 6 s) and 30-33 s (gap 15 s)
  expect_equal(b$n_windows, c(3L, 1L))
  expect_equal(b$max_score, c(0.9, 0.8))
})

test_that("score_summary_files and log_quantile_edges recover halving bins", {
  skip_if_not_installed("nanoparquet")
  dir <- withr::local_tempdir()
  set.seed(1)
  p <- stats::runif(20000)
  nanoparquet::write_parquet(
    data.frame(`Start (s)` = seq(0, by = 3, length.out = 20000), `End (s)` = seq(3, by = 3, length.out = 20000),
               `Common name` = "x", Confidence = p, check.names = FALSE),
    file.path(dir, "A_20260501_060000.BirdNET.results.parquet"))
  s <- score_summary_files(list.files(dir, full.names = TRUE), keep_above = 0.99, progress = 0)
  expect_equal(sum(s$hist$n), 20000)
  expect_equal(nrow(s$high), sum(p >= 0.99))
  e <- log_quantile_edges(s$hist, s$grid, n_bins = 4)
  expect_equal(e$upper[1:3], c(0.5, 0.75, 0.875), tolerance = 0.02)
  expect_equal(sum(e$windows), 20000)
})

test_that("sample_windows_by_bin samples within bins and records inclusion probabilities", {
  set.seed(2)
  w <- dplyr::tibble(Site = rep(c("A", "B"), each = 500), Confidence = stats::runif(1000))
  edges <- dplyr::tibble(lower = c(0, 0.5, 0.9), upper = c(0.5, 0.9, 1))
  s <- sample_windows_by_bin(w, edges, n_per_bin = c(A = 10, B = 4), seed = 3)
  expect_equal(nrow(s), 3 * 10 + 3 * 4)
  pop <- attr(s, "population")
  expect_equal(sum(pop$bin_windows), 1000)
  a_top <- s[s$Site == "A" & s$bin == 3, ]
  expect_equal(unique(a_top$inclusion_prob), 10 / pop$bin_windows[pop$Site == "A" & pop$bin == 3])
  expect_true(all(a_top$Confidence >= 0.9))
})

test_that("cut_review_clips writes blinded, shuffled clips", {
  skip_if(!nzchar(Sys.which("ffmpeg")))
  dir <- withr::local_tempdir()
  src <- file.path(dir, "SiteA_20260501_060000.wav")
  system2("ffmpeg", c("-v", "error", "-f", "lavfi", "-i", "sine=frequency=440:duration=30:sample_rate=16000", shQuote(src)))
  clips <- dplyr::tibble(source_path = src, begin_time_s = c(3, 15, 27), end_time_s = c(6, 18, 30),
                         Site = "SiteA")
  r <- cut_review_clips(clips, file.path(dir, "packet"), seed = 1)
  expect_true(all(r$key$cut_ok))
  expect_false(any(grepl("SiteA|2026", names(r$sheet)) | grepl("SiteA", unlist(r$sheet))))
  expect_equal(sort(r$key$begin_time_s), c(3, 15, 27))
  expect_equal(r$sheet$target_start_s[r$key$begin_time_s == 3], 3)  # clip starts at 0
  expect_true(all(file.exists(file.path(dir, "packet", r$sheet$clip_file))))
})

test_that("sample_windows_by_bin accepts a quota per bin", {
  set.seed(4)
  w <- dplyr::tibble(pool = "all", Confidence = stats::runif(2000))
  edges <- dplyr::tibble(lower = c(0, 0.5, 0.9), upper = c(0.5, 0.9, 1))
  s <- sample_windows_by_bin(w, edges, n_per_bin = c(2, 5, 30), strata = "pool", seed = 1)
  expect_equal(as.vector(table(s$bin)), c(2, 5, 30))
  expect_error(sample_windows_by_bin(w, edges, n_per_bin = c(1, 2), strata = "pool"), "one entry per bin")
})
