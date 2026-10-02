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
