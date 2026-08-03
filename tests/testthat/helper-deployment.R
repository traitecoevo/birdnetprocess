# Helpers for the deployment-config tests. Everything is built on the fly in a
# temp directory so the tests never depend on a real field deployment.

# A minimal valid config as a nested list, ready to be tweaked per test and
# written out with write_test_deployment().
test_deployment_list <- function(...) {
  cfg <- list(
    name = "Test deployment",
    location = list(
      latitude = -33.5,
      longitude = 151.2,
      tz = "Australia/Sydney",
      place = "Nowhere, NSW"
    ),
    detector = list(name = "BirdNET-Analyzer", version = "2.4"),
    sites = list(
      list(id = "Site A", detections = "detections/site_a"),
      list(id = "Site B", detections = "detections/site_b")
    ),
    analysis = list(confidence = 0.5)
  )
  utils::modifyList(cfg, list(...))
}

# Write a config to `dir` and return the path.
write_test_deployment <- function(dir, cfg = test_deployment_list()) {
  path <- file.path(dir, "deployment.yml")
  yaml::write_yaml(cfg, path)
  path
}

# Write BirdNET CSV detections for one site, named so that
# parse_birdnet_filename_datetime() can read the timestamp back out.
write_test_detections <- function(dir, site = "SiteA", date = "20260203",
                                  species = c("Zebra Finch", "Noisy Miner"),
                                  n_each = 4, confidence = 0.9) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(dir, sprintf("%s_%s_060000.BirdNET.results.csv", site, date))
  df <- data.frame(
    `Start (s)` = rep(seq(0, by = 3, length.out = n_each), length(species)),
    `End (s)` = rep(seq(3, by = 3, length.out = n_each), length(species)),
    `Scientific name` = rep(c("Taeniopygia guttata", "Manorina melanocephala")[
      seq_along(species)], each = n_each),
    `Common name` = rep(species, each = n_each),
    Confidence = confidence,
    check.names = FALSE
  )
  utils::write.csv(df, file, row.names = FALSE)
  file
}

# A labels file in BirdNET's "Genus species_Common Name" format.
write_test_labels <- function(dir) {
  path <- file.path(dir, "labels.txt")
  writeLines(c(
    "Taeniopygia guttata_Zebra Finch",
    "Manorina melanocephala_Noisy Miner",
    "Teleogryllus commodus_Black Field Cricket",
    "Litoria fallax_Eastern Dwarf Tree Frog",
    "Noise"
  ), path)
  path
}
