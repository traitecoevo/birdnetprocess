test_that("read_birdnet_file handles txt and csv with different headers", {
    # Mock TXT file with "Begin Time (s)"
    txt_content <- "Selection\tView\tChannel\tBegin Time (s)\tEnd Time (s)\tLow Freq (Hz)\tHigh Freq (Hz)\tCommon Name\tSpecies Code\tConfidence\n1\tSpectrogram 1\t1\t1.5\t4.5\t150\t8000\tAmerican Robin\tAMRO\t0.95"
    txt_file <- tempfile(fileext = ".txt")
    writeLines(txt_content, txt_file)

    # Mock CSV file with "Start (s)"
    csv_content <- "Start (s),End (s),Scientific name,Common name,Confidence,File\n5.0,8.0,Turdus migratorius,American Robin,0.85,test.wav"
    csv_file <- tempfile(fileext = ".results.csv")
    writeLines(csv_content, csv_file)

    # Mock filename for datetime parsing "Station_20240101_120000.BirdNET.selection.table.txt"
    # Since read_birdnet_file calls basename() and parse_birdnet_filename_datetime(),
    # we need the tempfile to have a compliant name OR we fix the function to be looser.
    # But the function `read_birdnet_file` uses `basename(file_path)` and parses it.
    # So we must rename our tempfiles to match the pattern expected by `parse_birdnet_filename_datetime`.

    valid_name_txt <- file.path(tempdir(), "Test_20240101_120000.BirdNET.selection.table.txt")
    writeLines(txt_content, valid_name_txt)

    valid_name_csv <- file.path(tempdir(), "Test_20240101_130000.BirdNET.results.csv")
    writeLines(csv_content, valid_name_csv)

    # Test TXT reading
    df_txt <- read_birdnet_file(valid_name_txt)
    expect_true("recording_window_time" %in% names(df_txt))
    expect_equal(df_txt$begin_time_s[1], 1.5)
    expect_equal(lubridate::hour(df_txt$recording_window_time[1]), 12)
    expect_equal(lubridate::second(df_txt$recording_window_time[1]), 1.5)

    # Test CSV reading
    df_csv <- read_birdnet_file(valid_name_csv)
    expect_true("recording_window_time" %in% names(df_csv))
    expect_equal(df_csv$begin_time_s[1], 5.0)
    expect_equal(lubridate::hour(df_csv$recording_window_time[1]), 13)
    expect_equal(lubridate::second(df_csv$recording_window_time[1]), 5.0) # 13:00:00 + 5s

    # Cleanup
    unlink(c(txt_file, csv_file, valid_name_txt, valid_name_csv))
})
