devtools::load_all(".")
test_that("read_birdnet_file handles Raven TXT and CSV", {
    # Paths to example data
    raven_path <- system.file("extdata", "example_raven.txt", package = "birdnetprocess")
    csv_path <- system.file("extdata", "example_birdnet.csv", package = "birdnetprocess")

    # Check if files exist (development environment might not have installed package yet)
    # If running from devtools::test(), system.file might not find it unless installed.
    # So we fallback to local path if system.file returns empty.
    if (raven_path == "") raven_path <- "../../inst/extdata/example_raven.txt"
    if (csv_path == "") csv_path <- "../../inst/extdata/example_birdnet.csv"

    expect_true(file.exists(raven_path))
    expect_true(file.exists(csv_path))

    # Test Raven reading
    df_raven <- read_birdnet_file(raven_path)
    expect_s3_class(df_raven, "tbl_df")
    expect_true("begin_time_s" %in% names(df_raven))
    expect_true("Common Name" %in% names(df_raven))
    expect_equal(df_raven$begin_time_s[1], 1.5)

    # Test CSV reading
    df_csv <- read_birdnet_file(csv_path)
    expect_s3_class(df_csv, "tbl_df")
    expect_true("begin_time_s" %in% names(df_csv)) # Should be renamed from Start (s)
    expect_equal(df_csv$begin_time_s[1], 1.5)

    # Check warning for no timestamp in filename
    expect_warning(
        read_birdnet_file(raven_path),
        "Could not parse datetime"
    )
})

test_that("read_birdnet_file handles filenames with timestamps", {
    # Mock a file with a timestamp
    # Reuse the CSV content
    csv_path <- system.file("extdata", "example_birdnet.csv", package = "birdnetprocess")
    if (csv_path == "") csv_path <- "../../inst/extdata/example_birdnet.csv"

    temp_dir <- tempdir()
    mock_name <- "SiteA_20240101_120000.BirdNET.results.csv"
    mock_path <- file.path(temp_dir, mock_name)

    file.copy(csv_path, mock_path, overwrite = TRUE)

    df <- read_birdnet_file(mock_path)
    expect_false(any(is.na(df$start_time)))
    expect_equal(df$start_time[1], as.POSIXct("2024-01-01 12:00:00", tz = "UTC"))
    expect_equal(df$recording_window_time[1], df$start_time[1] + 1.5)
})
