test_that("read_birdnet_file handles Raven TXT and CSV", {
    # Paths to example data
    raven_path <- system.file("extdata", "SiteA_20240101_120000.BirdNET.selection.table.txt", package = "birdnetprocess")
    csv_path <- system.file("extdata", "SiteA_20240101_120000.BirdNET.results.csv", package = "birdnetprocess")

    # Check if files exist (development environment might not have installed package yet)
    # If running from devtools::test(), system.file might not find it unless installed.
    # So we fallback to local path if system.file returns empty.
    if (raven_path == "") raven_path <- "../../inst/extdata/SiteA_20240101_120000.BirdNET.selection.table.txt"
    if (csv_path == "") csv_path <- "../../inst/extdata/SiteA_20240101_120000.BirdNET.results.csv"

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
})

test_that("read_birdnet_file handles filenames with timestamps", {
    # Mock a file with a timestamp
    # Reuse the CSV content
    csv_path <- system.file("extdata", "SiteA_20240101_120000.BirdNET.results.csv", package = "birdnetprocess")
    if (csv_path == "") csv_path <- "../../inst/extdata/SiteA_20240101_120000.BirdNET.results.csv"

    temp_dir <- tempdir()
    mock_name <- "SiteA_20240101_120000.BirdNET.results.csv"
    mock_path <- file.path(temp_dir, mock_name)

    file.copy(csv_path, mock_path, overwrite = TRUE)

    df <- read_birdnet_file(mock_path)
    expect_false(any(is.na(df$start_time)))
    expect_equal(df$start_time[1], as.POSIXct("2024-01-01 12:00:00", tz = "UTC"))
    expect_equal(df$recording_window_time[1], df$start_time[1] + 1.5)
})

test_that("read_birdnet_file derives times from Begin Path when filename lacks a timestamp", {
    # Combined Raven export: one timestamp-less filename spanning two recordings.
    # The per-recording start lives in `Begin Path`; `File Offset (s)` is the
    # offset within that wav, while `Begin Time (s)` is cumulative across files.
    header <- c(
        "Selection", "Begin Time (s)", "End Time (s)", "Common Name",
        "Confidence", "File Offset (s)", "Begin Path"
    )
    rows <- rbind(
        c(1, 10, 13, "Galah", 0.9, 10, "/vol/REC_20240101_120000.wav"),
        c(2, 30, 33, "Galah", 0.8, 30, "/vol/REC_20240101_120000.wav"),
        # begin_time_s is cumulative (4000) with a gap; the true time must come
        # from the wav stamp (13:00:00) + file offset (5) = 13:00:05.
        c(3, 4000, 4003, "Emu", 0.7, 5, "/vol/REC_20240101_130000.wav")
    )
    path <- file.path(tempdir(), "BirdNET_SelectionTable.txt")
    writeLines(
        c(
            paste(header, collapse = "\t"),
            apply(rows, 1, paste, collapse = "\t")
        ),
        path
    )

    # Fallback succeeds, so no parse warning should be emitted.
    expect_no_warning(df <- read_birdnet_file(path))

    expect_false(any(is.na(df$start_time)))
    expect_false(any(is.na(df$recording_window_time)))
    expect_equal(df$recording_window_time[1], as.POSIXct("2024-01-01 12:00:10", tz = "UTC"))
    # Uses File Offset (s), NOT the cumulative begin_time_s.
    expect_equal(df$recording_window_time[3], as.POSIXct("2024-01-01 13:00:05", tz = "UTC"))
    expect_equal(df$start_time[3], as.POSIXct("2024-01-01 13:00:00", tz = "UTC"))
})

test_that("read_birdnet_file still warns when neither filename nor Begin Path has a timestamp", {
    header <- c("Selection", "Begin Time (s)", "End Time (s)", "Common Name", "Confidence")
    rows <- rbind(c(1, 10, 13, "Galah", 0.9))
    path <- file.path(tempdir(), "BirdNET_SelectionTable_notime.txt")
    writeLines(
        c(
            paste(header, collapse = "\t"),
            apply(rows, 1, paste, collapse = "\t")
        ),
        path
    )

    expect_warning(df <- read_birdnet_file(path), "Could not parse datetime")
    expect_true(all(is.na(df$start_time)))
    expect_true(all(is.na(df$recording_window_time)))
})
