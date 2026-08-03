test_that("detection_time prefers the call time over the file start time", {
  base <- lubridate::ymd_hms("2026-02-03 06:00:00")
  df <- dplyr::tibble(
    start_time = rep(base, 3),
    recording_window_time = base + lubridate::minutes(c(0, 10, 20))
  )

  # A combined Raven export has many detections per source file; keying on
  # start_time alone would collapse them all onto the same instant.
  expect_equal(birdnetprocess:::detection_time(df), df$recording_window_time)
})

test_that("detection_time falls back when the window time is missing", {
  base <- lubridate::ymd_hms("2026-02-03 06:00:00")

  df <- dplyr::tibble(start_time = base + lubridate::minutes(0:2))
  expect_equal(birdnetprocess:::detection_time(df), df$start_time)

  df2 <- dplyr::tibble(
    start_time = base + lubridate::minutes(0:2),
    recording_window_time = c(base, NA, base + lubridate::minutes(20))
  )
  got <- birdnetprocess:::detection_time(df2)
  expect_equal(got[2], df2$start_time[2])
  expect_equal(got[3], df2$recording_window_time[3])

  expect_error(birdnetprocess:::detection_time(dplyr::tibble(x = 1)),
               "recording_window_time")
})

test_that("report_bundle assembles the numbers a report needs", {
  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A")
  write_test_detections(file.path(dir, "b"), site = "B")

  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "a"),
                    list(id = "B", detections = "b"))
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))
  b <- suppressWarnings(birdnetprocess:::report_bundle(cfg, data = d))

  expect_equal(b$stats$n_sites, 2)
  expect_equal(b$stats$n_species, 2)
  expect_equal(b$stats$n_detections, nrow(d))
  expect_equal(nrow(b$per_site), 2)
  expect_equal(nrow(b$species_table), 2)
  expect_s3_class(b$cfg, "birdnet_deployment")
})

test_that("report_bundle applies the confidence threshold", {
  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A", confidence = 0.2)

  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "a"))
  cfg$analysis$confidence <- 0.5
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))
  b <- suppressWarnings(birdnetprocess:::report_bundle(cfg, data = d))

  expect_equal(b$stats$n_detections, 0)
})

test_that("a failing figure is recorded rather than killing the bundle", {
  dir <- withr::local_tempdir()
  # A single species: plot_species_counts-style figures need more than one,
  # so at least one panel is expected to fail here.
  write_test_detections(file.path(dir, "a"), site = "A", species = "Zebra Finch")

  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "a"))
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))
  b <- suppressWarnings(birdnetprocess:::report_bundle(cfg, data = d))

  expect_type(b$failures, "character")
  expect_true(is.list(b$plots))
})

test_that("plot_species_ranked shows the top species with a linear scale", {
  data <- dplyr::tibble(`Common Name` = c(rep("A", 10), rep("B", 5), rep("C", 1)))

  p <- birdnetprocess:::plot_species_ranked(data, n_top = 2, n_total = 3)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 2)
  expect_match(p$labels$subtitle, "Top 2 of 3")
})

test_that("slugify makes a safe file name", {
  expect_equal(birdnetprocess:::slugify("Smiths Lake phenology"),
               "smiths_lake_phenology")
  expect_equal(birdnetprocess:::slugify("Wild Deserts — CES 1"),
               "wild_deserts_ces_1")
})

test_that("birdnet_report renders a self-contained HTML report", {
  skip_on_cran()
  skip_if_not_installed("quarto")
  skip_if(is.null(quarto::quarto_path()), "Quarto CLI not installed")
  # The template lives in inst/; without an installed package there is nothing
  # for system.file() to find in a check environment.
  skip_if(!nzchar(system.file("report", "deployment_report.qmd",
                              package = "birdnetprocess")),
          "Report template not installed")

  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A")
  write_test_detections(file.path(dir, "b"), site = "B")

  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "a"),
                    list(id = "B", detections = "b"))
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  out <- suppressWarnings(suppressMessages(
    birdnet_report(cfg, output_dir = file.path(dir, "report"))
  ))

  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Test deployment")
  # Self-contained: figures inlined, nothing left pointing at a local file.
  expect_no_match(html, 'src="figures/')
})

test_that("birdnet_report rejects something that is not a deployment", {
  expect_error(birdnet_report(list(name = "x")), "must be a birdnet_deployment")
})

test_that("recordings are counted from the audio, not the detection table", {
  # The perch arm writes one predictions.csv per site, so file_name is constant
  # and only Begin Path distinguishes the recordings.
  d <- dplyr::tibble(
    `Begin Path` = c("/s/a.wav", "/s/a.wav", "/s/b.wav"),
    file_name = "perch_predictions.csv"
  )
  expect_equal(dplyr::n_distinct(source_recording(d)), 2L)

  # BirdNET's one-table-per-recording output has no Begin Path in some formats.
  expect_equal(
    source_recording(dplyr::tibble(file_name = c("x.txt", "y.txt"))),
    c("x.txt", "y.txt")
  )
})
