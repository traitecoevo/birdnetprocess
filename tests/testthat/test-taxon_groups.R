test_that("read_detector_labels splits scientific and common names", {
  dir <- withr::local_tempdir()
  labels <- read_detector_labels(write_test_labels(dir))

  expect_equal(nrow(labels), 5)
  expect_equal(labels$common[1], "Zebra Finch")
  expect_equal(labels$scientific[1], "Taeniopygia guttata")
  expect_equal(labels$genus[1], "Taeniopygia")
})

test_that("a label with no underscore survives as a non-taxonomic class", {
  dir <- withr::local_tempdir()
  labels <- read_detector_labels(write_test_labels(dir))

  noise <- labels[labels$common == "Noise", ]
  expect_equal(nrow(noise), 1)
  expect_equal(noise$scientific, "")
})

test_that("an empty or missing labels file is an error", {
  dir <- withr::local_tempdir()
  empty <- file.path(dir, "empty.txt")
  writeLines(character(0), empty)

  expect_error(read_detector_labels(empty), "empty")
  expect_error(read_detector_labels(file.path(dir, "nope.txt")), "not found")
})

test_that("assign_groups sorts species by genus", {
  dir <- withr::local_tempdir()
  labels <- read_detector_labels(write_test_labels(dir))

  data <- dplyr::tibble(`Common Name` = c(
    "Zebra Finch", "Black Field Cricket", "Eastern Dwarf Tree Frog"
  ))
  rules <- list(
    Insects = list(genera = list("Teleogryllus")),
    Frogs = list(genera = list("Litoria")),
    Birds = list(default = TRUE)
  )

  out <- assign_groups(data, labels = labels, rules = rules)

  expect_equal(as.character(out$group),
               c("Birds", "Insects", "Frogs"))
  expect_equal(levels(out$group), c("Insects", "Frogs", "Birds"))
})

test_that("unmatched species fall into the default group", {
  dir <- withr::local_tempdir()
  labels <- read_detector_labels(write_test_labels(dir))

  data <- dplyr::tibble(`Common Name` = c("Zebra Finch", "Not In Labels"))
  rules <- list(Insects = list(genera = list("Teleogryllus")),
                Birds = list(default = TRUE))

  out <- expect_message(
    assign_groups(data, labels = labels, rules = rules),
    "not found in the labels file"
  )
  expect_equal(as.character(out$group), c("Birds", "Birds"))
})

test_that("rules can name species directly for cases genus can't express", {
  dir <- withr::local_tempdir()
  labels <- read_detector_labels(write_test_labels(dir))

  data <- dplyr::tibble(`Common Name` = c("Zebra Finch", "Noisy Miner"))
  rules <- list(Introduced = list(species = list("Noisy Miner")),
                Birds = list(default = TRUE))

  out <- assign_groups(data, labels = labels, rules = rules)
  expect_equal(as.character(out$group), c("Birds", "Introduced"))
})

test_that("the first matching rule wins", {
  dir <- withr::local_tempdir()
  labels <- read_detector_labels(write_test_labels(dir))

  data <- dplyr::tibble(`Common Name` = "Black Field Cricket")
  rules <- list(
    Crickets = list(genera = list("Teleogryllus")),
    Insects = list(genera = list("Teleogryllus")),
    Birds = list(default = TRUE)
  )

  out <- assign_groups(data, labels = labels, rules = rules)
  expect_equal(as.character(out$group), "Crickets")
})

test_that("more than one default group is an error", {
  data <- dplyr::tibble(`Common Name` = "Zebra Finch")
  rules <- list(A = list(default = TRUE), B = list(default = TRUE))

  expect_error(
    assign_groups(data, labels = dplyr::tibble(common = character(),
                                               genus = character()),
                  rules = rules),
    "More than one group"
  )
})

test_that("assign_groups falls back to the attached deployment config", {
  dir <- withr::local_tempdir()
  labels_path <- write_test_labels(dir)

  cfg <- test_deployment_list()
  cfg$detector$labels <- labels_path
  cfg$analysis$groups <- list(
    Insects = list(genera = list("Teleogryllus")),
    Birds = list(default = TRUE)
  )
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  data <- dplyr::tibble(`Common Name` = c("Zebra Finch", "Black Field Cricket"))
  attr(data, "deployment") <- cfg

  out <- assign_groups(data)
  expect_equal(as.character(out$group), c("Birds", "Insects"))
})

test_that("assign_groups without rules or labels is a clear error", {
  data <- dplyr::tibble(`Common Name` = "Zebra Finch")
  expect_error(assign_groups(data), "No grouping rules")
  expect_error(assign_groups(data, rules = list(Birds = list(default = TRUE))),
               "No labels file")
})


# --- colours ----------------------------------------------------------------

test_that("group_colours honours declared colours and greys out residual bins", {
  rules <- list(
    Insects = list(genera = list("Teleogryllus")),
    Frogs = list(colour = "#123456"),
    Other = list(),
    Birds = list(default = TRUE)
  )

  cols <- group_colours(rules)

  expect_named(cols, c("Insects", "Frogs", "Other", "Birds"))
  expect_equal(unname(cols["Frogs"]), "#123456")
  # A leftover bin is not a category to track, so it gets muted grey rather
  # than an identity hue.
  expect_equal(unname(cols["Other"]), birdnetprocess:::birdnet_ink$residual)
  expect_true(all(grepl("^#", cols)))
})

test_that("group_colours accepts a deployment and handles no rules", {
  dir <- withr::local_tempdir()
  cfg <- test_deployment_list()
  cfg$analysis$groups <- list(Birds = list(default = TRUE))
  cfg <- read_deployment(write_test_deployment(dir, cfg))

  expect_length(group_colours(cfg), 1)
  expect_length(group_colours(list()), 0)
})

test_that("birdnet_palette returns colours in fixed order and warns past its limit", {
  expect_equal(unname(birdnet_palette(1)), "#2a78d6")
  expect_length(birdnet_palette(3), 3)
  expect_length(birdnet_palette(4, all_pairs = TRUE), 4)

  # The order is the colour-vision-safety mechanism, so taking n colours must
  # always take the same n from the front.
  expect_equal(birdnet_palette(3), birdnet_palette(8)[1:3])

  expect_warning(birdnet_palette(20), "only 8")
})


# --- per-species thresholds -------------------------------------------------

threshold_cfg <- function(dir) {
  cfg <- test_deployment_list()
  cfg$analysis$confidence <- 0.431
  cfg$analysis$confidence_by_species <- list(
    `White-winged Fairywren` = 0.444,
    `Brown Songlark` = 0.852
  )
  cfg$analysis$unvalidated_species <- list("Singing Honeyeater", "Galah")
  read_deployment(write_test_deployment(dir, cfg))
}

test_that("species_thresholds prefers a calibrated cut, else the default", {
  cfg <- threshold_cfg(withr::local_tempdir())

  expect_equal(species_thresholds(cfg, "White-winged Fairywren"), 0.444)
  expect_equal(species_thresholds(cfg, "Brown Songlark"), 0.852)
  expect_equal(species_thresholds(cfg, "Budgerigar"), 0.431)
  expect_equal(
    species_thresholds(cfg, c("Brown Songlark", "Budgerigar")),
    c(0.852, 0.431)
  )
})

test_that("threshold_source separates never-tested from tested-and-failed", {
  cfg <- threshold_cfg(withr::local_tempdir())

  # The distinction the report depends on: both fall back to the default
  # threshold, but they are opposite claims about the evidence.
  expect_equal(threshold_source(cfg, "White-winged Fairywren"), "calibrated")
  expect_equal(threshold_source(cfg, "Budgerigar"), "default")
  expect_equal(threshold_source(cfg, "Singing Honeyeater"), "unvalidated")
  expect_equal(threshold_source(cfg, "Galah"), "unvalidated")
})

test_that("a config with no per-species entries gives everything the default", {
  dir <- withr::local_tempdir()
  cfg <- read_deployment(write_test_deployment(dir))  # confidence 0.5, no map

  expect_equal(species_thresholds(cfg, c("A", "B")), c(0.5, 0.5))
  expect_equal(threshold_source(cfg, c("A", "B")), c("default", "default"))
})

test_that("apply_thresholds filters each species at its own cut", {
  cfg <- threshold_cfg(withr::local_tempdir())
  d <- dplyr::tibble(
    `Common Name` = c("White-winged Fairywren", "White-winged Fairywren",
                      "Brown Songlark", "Brown Songlark",
                      "Budgerigar", "Budgerigar"),
    Confidence = c(0.50, 0.40, 0.90, 0.50, 0.45, 0.40)
  )

  out <- apply_thresholds(d, cfg)

  # Brown Songlark's 0.50 is dropped at its own 0.852 while the fairywren's
  # 0.50 survives at 0.444 - the whole point of a per-species cut.
  expect_equal(nrow(out), 3)
  expect_equal(out$Confidence, c(0.50, 0.90, 0.45))
  expect_equal(out$threshold, c(0.444, 0.852, 0.431))
  expect_equal(out$threshold_source, c("calibrated", "calibrated", "default"))
})

test_that("apply_thresholds handles an empty table", {
  cfg <- threshold_cfg(withr::local_tempdir())
  d <- dplyr::tibble(`Common Name` = character(), Confidence = numeric())
  expect_equal(nrow(apply_thresholds(d, cfg)), 0)
})
