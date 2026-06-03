# Shared test helpers for range-filtering / site-report tests.

# A tiny 2-layer abundance stack. zebra_finch covers the whole extent (value 5);
# noisy_miner is NA everywhere (modelled-absent over the test point).
make_test_raster <- function() {
  skip_if_not_installed("terra")
  zf <- terra::rast(
    nrows = 2, ncols = 2,
    xmin = 140, xmax = 142, ymin = -30, ymax = -28,
    crs = "EPSG:4326", vals = 5
  )
  nm <- terra::rast(zf)
  terra::values(nm) <- NA_real_
  names(zf) <- "zebra_finch"
  names(nm) <- "noisy_miner"
  c(zf, nm)
}

# Minimal detections with the time columns produced by read_birdnet_*.
make_site_data <- function() {
  base <- lubridate::ymd_hms("2024-01-01 06:00:00")
  dplyr::tibble(
    `Common Name` = c(rep("Zebra Finch", 12), rep("Noisy Miner", 8), rep("Cattle", 5)),
    Confidence = 0.9,
    begin_time_s = 0,
    start_time = base + lubridate::minutes(seq_len(25)),
    recording_window_time = base + lubridate::minutes(seq_len(25))
  )
}
