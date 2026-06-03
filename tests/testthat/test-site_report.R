test_that("site_report returns objects and writes files when given a raster", {
  skip_if_not_installed("terra")
  r <- make_test_raster()
  d <- make_site_data()
  out_dir <- withr::local_tempdir()

  res <- site_report(
    d,
    latitude = -29, longitude = 141,
    abundance_raster = r,
    confidence = 0.5,
    output_dir = out_dir,
    tz = "UTC"
  )

  expect_named(res, c("report", "summary", "plots", "data_filtered", "output_dir"))
  expect_s3_class(res$report, "data.frame")
  expect_false("Noisy Miner" %in% res$data_filtered$`Common Name`)

  expect_true(file.exists(file.path(out_dir, "species_filter_report.csv")))
  expect_true(file.exists(file.path(out_dir, "01_species_counts.png")))
  expect_true(file.exists(file.path(out_dir, "02_top_species_daynight.png")))
})

test_that("site_report works without a raster (no filtering)", {
  d <- make_site_data()
  res <- site_report(d, latitude = -29, longitude = 141, confidence = 0.5)
  expect_null(res$report)
  expect_equal(nrow(res$data_filtered), nrow(d))
})

test_that("start_after / end_before trim to the deployment window", {
  d <- make_site_data() # times are 2024-01-01 06:01 .. 06:25 (UTC)
  cutoff <- lubridate::ymd_hms("2024-01-01 06:10:00", tz = "UTC")

  res <- site_report(
    d,
    latitude = -29, longitude = 141, confidence = 0.5,
    start_after = cutoff
  )
  expect_true(all(res$data_filtered$recording_window_time >= cutoff))
  expect_lt(nrow(res$data_filtered), nrow(d))

  # string bound parsed as UTC works too
  res2 <- site_report(
    d,
    latitude = -29, longitude = 141, confidence = 0.5,
    start_after = "2024-01-01 06:10:00"
  )
  expect_equal(nrow(res2$data_filtered), nrow(res$data_filtered))
})
