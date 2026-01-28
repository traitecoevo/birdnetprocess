test_that("summarise_detections calculates stats correctly", {
    # Mock data
    # 2 days: 2024-01-01 and 2024-01-02
    t1 <- lubridate::ymd_hms("2024-01-01 10:00:00")
    t2 <- lubridate::ymd_hms("2024-01-01 11:00:00")
    t3 <- lubridate::ymd_hms("2024-01-02 10:00:00")

    df <- dplyr::tibble(
        `Common Name` = c("Robin", "Robin", "Sparrow", "Robin", "Sparrow"),
        Confidence = c(0.9, 0.9, 0.9, 0.9, 0.9),
        start_time = c(t1, t1, t1, t2, t3)
    )

    # Robin: 3, Sparrow: 2. Most common: Robin.
    # Days: 2.
    # Total recordings: 5.
    # Peak hour: 2024-01-01 10:00:00 has 3 recordings (t1).
    # Av per day: 5 / 2 = 2.5.

    stats <- summarise_detections(df, confidence = 0.5)

    # summarise_detections returns a tibble with "statistic" and "value" columns (character)

    # Helper to extract value
    get_val <- function(s) {
        stats |>
            dplyr::filter(statistic == s) |>
            dplyr::pull(value)
    }

    expect_equal(get_val("Number of species"), "2")
    expect_equal(get_val("Number of recordings"), "5")
    expect_equal(get_val("Most common bird"), "Robin")
    expect_equal(get_val("Average recordings per day"), "2.5")

    # Check date range string
    expect_true(grepl("01 Jan 24", get_val("Recording window")))
    expect_true(grepl("02 Jan 24", get_val("Recording window")))
})
