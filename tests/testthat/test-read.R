test_that("read_birdnet_folder aggregates files correctly", {
    # Create a temporary directory
    temp_dir <- file.path(tempdir(), "test_folder")
    if (!dir.exists(temp_dir)) dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    # Create two mock files matching the pattern
    # ..._YYYYMMDD_HHMMSS.BirdNET.selection.table.txt

    content1 <- "Selection\tView\tChannel\tBegin Time (s)\tEnd Time (s)\tLow Freq (Hz)\tHigh Freq (Hz)\tCommon Name\tSpecies Code\tConfidence\n1\tSpectrogram 1\t1\t1.0\t4.0\t150\t8000\tRobin\tAMRO\t0.95"
    file1 <- file.path(temp_dir, "S1_20240101_120000.BirdNET.selection.table.txt")
    writeLines(content1, file1)

    content2 <- "Selection\tView\tChannel\tBegin Time (s)\tEnd Time (s)\tLow Freq (Hz)\tHigh Freq (Hz)\tCommon Name\tSpecies Code\tConfidence\n1\tSpectrogram 1\t1\t2.0\t5.0\t150\t8000\tSparrow\tSPAR\t0.85"
    file2 <- file.path(temp_dir, "S1_20240101_130000.BirdNET.selection.table.txt")
    writeLines(content2, file2)

    # create a noise file that shouldn't be read
    writeLines("garbage", file.path(temp_dir, "noise.txt"))

    df <- read_birdnet_folder(folder = temp_dir)

    expect_equal(nrow(df), 2)
    expect_true("Robin" %in% df$`Common Name`)
    expect_true("Sparrow" %in% df$`Common Name`)

    # Check datetimes
    # File 1: 12:00:00 + 1.0s = 12:00:01
    # File 2: 13:00:00 + 2.0s = 13:00:02

    times <- sort(df$recording_window_time)
    expect_equal(lubridate::hour(times[1]), 12)
    expect_equal(lubridate::hour(times[2]), 13)
})

test_that("read_birdnet_sites adds Site column", {
    # Create two site folders
    root_dir <- tempdir()
    siteA <- file.path(root_dir, "SiteA")
    siteB <- file.path(root_dir, "SiteB")

    if (!dir.exists(siteA)) dir.create(siteA)
    if (!dir.exists(siteB)) dir.create(siteB)

    on.exit(
        {
            unlink(siteA, recursive = TRUE)
            unlink(siteB, recursive = TRUE)
        },
        add = TRUE
    )

    content <- "Selection\tView\tChannel\tBegin Time (s)\tEnd Time (s)\tLow Freq (Hz)\tHigh Freq (Hz)\tCommon Name\tSpecies Code\tConfidence\n1\tSpectrogram 1\t1\t1.0\t4.0\t150\t8000\tRobin\tAMRO\t0.95"

    writeLines(content, file.path(siteA, "A_20240101_120000.BirdNET.selection.table.txt"))
    writeLines(content, file.path(siteB, "B_20240101_120000.BirdNET.selection.table.txt"))

    df <- read_birdnet_sites(c(siteA, siteB))

    expect_equal(nrow(df), 2)
    expect_true("Site" %in% names(df))
    expect_true("SiteA" %in% df$Site)
    expect_true("SiteB" %in% df$Site)
})

test_that("read_birdnet_folder handles mixed-type Confidence columns", {
    # Create a temporary directory
    temp_dir <- file.path(tempdir(), "test_mixed_types")
    if (!dir.exists(temp_dir)) dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    # File 1: Numeric confidence
    content1 <- "Selection\tView\tChannel\tBegin Time (s)\tEnd Time (s)\tLow Freq (Hz)\tHigh Freq (Hz)\tCommon Name\tSpecies Code\tConfidence\n1\tSpectrogram 1\t1\t1.0\t4.0\t150\t8000\tRobin\tAMRO\t0.95"
    file1 <- file.path(temp_dir, "S1_20240101_120000.BirdNET.selection.table.txt")
    writeLines(content1, file1)

    # File 2: Character confidence (e.g. "missing" or "NA" string)
    content2 <- "Selection\tView\tChannel\tBegin Time (s)\tEnd Time (s)\tLow Freq (Hz)\tHigh Freq (Hz)\tCommon Name\tSpecies Code\tConfidence\n1\tSpectrogram 1\t1\t2.0\t5.0\t150\t8000\tSparrow\tSPAR\tmissing"
    file2 <- file.path(temp_dir, "S1_20240101_130000.BirdNET.selection.table.txt")
    writeLines(content2, file2)

    # Should not throw error
    expect_no_error(
        df <- read_birdnet_folder(folder = temp_dir)
    )

    expect_equal(nrow(df), 2)
    expect_type(df$Confidence, "double")
    expect_equal(df$Confidence[1], 0.95)
    expect_true(is.na(df$Confidence[2]))
})
