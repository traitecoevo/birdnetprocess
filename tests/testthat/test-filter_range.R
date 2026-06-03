test_that("normalize_species_name maps common names to layer style", {
  expect_equal(normalize_species_name("Grey Teal"), "gray_teal")
  expect_equal(normalize_species_name("Bourke's Parrot"), "bourke_s_parrot")
  expect_equal(normalize_species_name("Black-faced Woodswallow"), "black_faced_woodswallow")
  expect_equal(normalize_species_name("Zebra Finch"), "zebra_finch")
})

test_that("filter_by_range removes out-of-range birds and keeps the rest", {
  skip_if_not_installed("terra")
  r <- make_test_raster()
  d <- dplyr::tibble(
    `Common Name` = c(rep("Zebra Finch", 4), rep("Noisy Miner", 3), rep("Cattle", 2)),
    Confidence = 0.9
  )

  out <- filter_by_range(d, r, latitude = -29, longitude = 141)
  rep <- attr(out, "range_report")

  # Noisy Miner is NA at the point -> removed; Zebra Finch in range -> kept;
  # Cattle has no layer -> unassessed -> kept.
  expect_false("Noisy Miner" %in% out$`Common Name`)
  expect_true(all(c("Zebra Finch", "Cattle") %in% out$`Common Name`))

  status_of <- function(sp) rep$status[rep$species == sp]
  expect_match(status_of("Noisy Miner"), "removed")
  expect_match(status_of("Zebra Finch"), "kept")
  expect_match(status_of("Cattle"), "unassessed")
})

test_that("name_overrides lets an unmatched name be assessed", {
  skip_if_not_installed("terra")
  r <- make_test_raster()
  d <- dplyr::tibble(`Common Name` = c("My Finch", "My Finch"), Confidence = 0.9)

  out <- filter_by_range(
    d, r, latitude = -29, longitude = 141,
    name_overrides = c("My Finch" = "zebra_finch")
  )
  rep <- attr(out, "range_report")
  expect_equal(rep$matched_layer[rep$species == "My Finch"], "zebra_finch")
  expect_match(rep$status[rep$species == "My Finch"], "kept")
})
