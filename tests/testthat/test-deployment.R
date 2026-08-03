test_that("read_deployment reads a valid config", {
  dir <- withr::local_tempdir()
  path <- write_test_deployment(dir)

  cfg <- read_deployment(path)

  expect_s3_class(cfg, "birdnet_deployment")
  expect_equal(cfg$name, "Test deployment")
  expect_equal(cfg$location$latitude, -33.5)
  expect_length(cfg$sites, 2)
  expect_equal(vapply(cfg$sites, function(s) s$id, character(1)),
               c("Site A", "Site B"))
})

test_that("relative paths resolve against the config file, not the working directory", {
  dir <- withr::local_tempdir()
  path <- write_test_deployment(dir)

  # Read from somewhere else entirely; the paths must still point into `dir`.
  other <- withr::local_tempdir()
  withr::with_dir(other, {
    cfg <- read_deployment(path)
    expect_true(startsWith(cfg$sites[[1]]$detections, normalizePath(dir)))
  })
})

test_that("read_deployment accepts a directory containing a deployment.yml", {
  dir <- withr::local_tempdir()
  write_test_deployment(dir)
  expect_s3_class(read_deployment(dir), "birdnet_deployment")
})

test_that("a missing required field is an error naming the field", {
  dir <- withr::local_tempdir()

  cfg <- test_deployment_list()
  cfg$name <- NULL
  expect_error(read_deployment(write_test_deployment(dir, cfg)), "name")

  cfg <- test_deployment_list()
  cfg$detector <- NULL
  expect_error(read_deployment(write_test_deployment(dir, cfg)), "detector")

  cfg <- test_deployment_list()
  cfg$location$latitude <- NULL
  expect_error(read_deployment(write_test_deployment(dir, cfg)), "latitude")
})

test_that("out-of-range coordinates are rejected", {
  dir <- withr::local_tempdir()

  # A latitude of 151 is the classic transposed lat/lon.
  cfg <- test_deployment_list()
  cfg$location$latitude <- 151.2
  cfg$location$longitude <- -33.5
  expect_error(read_deployment(write_test_deployment(dir, cfg)),
               "latitude must be")
})

test_that("an unknown time zone is rejected", {
  dir <- withr::local_tempdir()
  cfg <- test_deployment_list()
  cfg$location$tz <- "Australia/Sidney"
  expect_error(read_deployment(write_test_deployment(dir, cfg)),
               "Unknown time zone")
})

test_that("duplicate site ids are rejected", {
  dir <- withr::local_tempdir()
  cfg <- test_deployment_list()
  cfg$sites[[2]]$id <- "Site A"
  expect_error(read_deployment(write_test_deployment(dir, cfg)),
               "Duplicate site ids")
})

test_that("a site with no id is rejected", {
  dir <- withr::local_tempdir()
  cfg <- test_deployment_list()
  cfg$sites[[1]]$id <- NULL
  expect_error(read_deployment(write_test_deployment(dir, cfg)), "needs an 'id'")
})

test_that("a confidence outside 0-1 is rejected", {
  dir <- withr::local_tempdir()
  cfg <- test_deployment_list()
  cfg$analysis$confidence <- 50
  expect_error(read_deployment(write_test_deployment(dir, cfg)),
               "confidence must be")
})

test_that("a site date becomes a whole-day window", {
  dir <- withr::local_tempdir()
  cfg <- test_deployment_list()
  cfg$sites[[1]]$date <- "2026-02-03"
  cfg$sites[[2]] <- NULL

  d <- read_deployment(write_test_deployment(dir, cfg))$sites[[1]]

  expect_equal(as.Date(d$start_after), as.Date("2026-02-03"))
  expect_equal(as.Date(d$end_before), as.Date("2026-02-03"))
  expect_equal(format(d$start_after, "%H:%M:%S", tz = "UTC"), "00:00:00")
  expect_equal(format(d$end_before, "%H:%M:%S", tz = "UTC"), "23:59:59")
})

test_that("a site cannot give both date and start/end", {
  dir <- withr::local_tempdir()
  cfg <- test_deployment_list()
  cfg$sites[[1]]$date <- "2026-02-03"
  cfg$sites[[1]]$start <- "2026-02-01"
  expect_error(read_deployment(write_test_deployment(dir, cfg)),
               "both 'date' and")
})

test_that("print gives a readable summary", {
  dir <- withr::local_tempdir()
  cfg <- read_deployment(write_test_deployment(dir))
  expect_output(print(cfg), "Test deployment")
  expect_output(print(cfg), "Site A")
})

test_that("new_deployment writes an editable template that reads back", {
  dir <- withr::local_tempdir()
  target <- file.path(dir, "new_site")

  path <- suppressMessages(new_deployment(target, name = "Fresh deployment"))

  expect_true(file.exists(path))
  raw <- yaml::read_yaml(path)
  expect_equal(raw$name, "Fresh deployment")
  # The template must stay structurally valid, or the starting point is
  # broken. Its placeholder paths don't exist, so warnings are expected — it
  # is errors that would mean the template itself is wrong.
  expect_no_error(suppressWarnings(suppressMessages(
    validate_deployment(structure(
      list(name = raw$name, location = raw$location, detector = raw$detector,
           sites = raw$sites, analysis = raw$analysis,
           path = path, dir = dirname(path)),
      class = "birdnet_deployment"
    ))
  )))

  expect_error(new_deployment(target), "already exists")
})


# --- ingestion --------------------------------------------------------------

test_that("read_deployment_detections takes Site from the config, not the folder", {
  dir <- withr::local_tempdir()
  # Folder names deliberately disagree with the ids we want in the report.
  write_test_detections(file.path(dir, "raw_01"), site = "REC1")
  write_test_detections(file.path(dir, "raw_02"), site = "REC2")

  cfg <- test_deployment_list()
  cfg$sites <- list(
    list(id = "Powerline Strip", detections = "raw_01"),
    list(id = "Mowed Field", detections = "raw_02")
  )
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))

  expect_setequal(unique(d$Site), c("Powerline Strip", "Mowed Field"))
})

test_that("each site is trimmed to its own window", {
  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A", date = "20260203")
  write_test_detections(file.path(dir, "b"), site = "B", date = "20260210")

  cfg <- test_deployment_list()
  cfg$sites <- list(
    list(id = "A", detections = "a", date = "2026-02-03"),
    list(id = "B", detections = "b", date = "2026-02-10")
  )
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))

  expect_setequal(unique(d$Site), c("A", "B"))
  expect_equal(unique(as.Date(d$recording_window_time[d$Site == "A"])),
               as.Date("2026-02-03"))
  expect_equal(unique(as.Date(d$recording_window_time[d$Site == "B"])),
               as.Date("2026-02-10"))
})

test_that("a site window excludes detections from other days", {
  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A", date = "20260203")
  write_test_detections(file.path(dir, "a"), site = "A", date = "20260204")

  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "a", date = "2026-02-03"))
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))

  expect_equal(unique(as.Date(d$recording_window_time)), as.Date("2026-02-03"))
})

test_that("excluded species are dropped", {
  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A")

  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "a"))
  cfg$analysis$exclude <- list("Noisy Miner")
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))

  expect_false("Noisy Miner" %in% d[["Common Name"]])
  expect_true("Zebra Finch" %in% d[["Common Name"]])
})

test_that("the config is attached to the detections", {
  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A")
  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "a"))
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))

  expect_s3_class(attr(d, "deployment"), "birdnet_deployment")
})

test_that("a missing detections folder warns rather than failing silently", {
  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A")

  cfg <- test_deployment_list()
  cfg$sites <- list(
    list(id = "A", detections = "a"),
    list(id = "B", detections = "nonexistent")
  )
  cfg <- suppressMessages(read_deployment(write_test_deployment(dir, cfg)))

  expect_warning(d <- read_deployment_detections(cfg), "Has the detector run")
  expect_equal(unique(d$Site), "A")
})

test_that("no detections anywhere is an error, not an empty report", {
  dir <- withr::local_tempdir()
  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "nope"))
  cfg <- suppressMessages(read_deployment(write_test_deployment(dir, cfg)))

  expect_error(suppressWarnings(read_deployment_detections(cfg)),
               "No detections found")
})

test_that("read_deployment_detections rejects a plain list", {
  expect_error(read_deployment_detections(list(name = "x")),
               "must be a birdnet_deployment")
})

test_that("an exclude mapping drops the species and keeps the reason", {
  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A")

  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "a"))
  cfg$analysis$exclude <- list("Noisy Miner" = "Not present at this site.")
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))
  ex <- attr(d, "excluded")

  expect_false("Noisy Miner" %in% d[["Common Name"]])
  expect_equal(ex$species, "Noisy Miner")
  expect_equal(ex$reason, "Not present at this site.")
  expect_gt(ex$detections, 0)
})

test_that("a plain exclude list gives species with empty reasons", {
  cfg <- structure(
    list(analysis = list(exclude = list("Rain", "Stream"))),
    class = "birdnet_deployment"
  )
  ex <- exclude_species(cfg)

  expect_equal(names(ex), c("Rain", "Stream"))
  expect_equal(unname(ex), c("", ""))
})

test_that("species never detected are left out of the excluded tally", {
  dir <- withr::local_tempdir()
  write_test_detections(file.path(dir, "a"), site = "A")

  cfg <- test_deployment_list()
  cfg$sites <- list(list(id = "A", detections = "a"))
  cfg$analysis$exclude <- list("Thick-billed Grasswren" = "Not here.")
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  d <- suppressMessages(read_deployment_detections(cfg))

  expect_equal(nrow(attr(d, "excluded")), 0L)
})
