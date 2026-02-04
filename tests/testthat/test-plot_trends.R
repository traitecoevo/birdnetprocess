test_that("plot_top_species generates a ggplot object", {
    mock_data <- dplyr::tibble(
        `Common name` = c(rep("Robin", 5), rep("Sparrow", 5), "Crow"),
        recording_window_time = seq(
            from = lubridate::ymd_hms("2024-01-01 12:00:00"),
            by = "10 min",
            length.out = 11
        ),
        Confidence = c(rep(0.9, 10), 0.4)
    )

    p <- plot_top_species(mock_data, n_top_species = 5, confidence_threshold = 0.5)
    expect_s3_class(p, "ggplot")

    plot_df <- p$data
    expect_false("Crow" %in% plot_df$`Common name`)
    expect_true("Robin" %in% plot_df$`Common name`)
})

test_that("plot_species filters correctly", {
    mock_data <- dplyr::tibble(
        `Common name` = c("Robin", "Sparrow", "Eagle"),
        recording_window_time = lubridate::ymd_hms("2024-01-01 12:00:00"),
        Confidence = 0.9
    )

    p <- plot_species(mock_data, species_list = c("Robin"), confidence_threshold = 0.5)
    plot_df <- p$data

    expect_true("Robin" %in% plot_df$`Common name`)
    expect_false("Sparrow" %in% plot_df$`Common name`)
    expect_false("Eagle" %in% plot_df$`Common name`)
})

test_that("plot_top_species fills zeros correctly", {
    t1 <- lubridate::ymd_hms("2024-01-01 10:00:00")
    t2 <- lubridate::ymd_hms("2024-01-01 11:00:00")
    t3 <- lubridate::ymd_hms("2024-01-01 12:00:00")

    mock_data <- dplyr::tibble(
        `Common name` = c("Bird A", "Bird A"),
        recording_window_time = c(t1, t3),
        Confidence = c(1.0, 1.0)
    )

    p <- plot_top_species(mock_data, n_top_species = 1, confidence_threshold = 0.5, unit = "hour")
    plot_data <- p$data

    expect_equal(nrow(plot_data), 3) # Gap filled
    expect_equal(plot_data$n[2], 0)
})

test_that("plot_top_species adds shading layer", {
    skip_if_not_installed("suncalc")

    mock_data <- dplyr::tibble(
        `Common name` = "Owl",
        recording_window_time = seq(
            lubridate::ymd_hms("2024-01-01 12:00:00", tz = "UTC"),
            lubridate::ymd_hms("2024-01-02 12:00:00", tz = "UTC"),
            by = "1 hour"
        ),
        Confidence = 0.9
    )

    p <- plot_top_species(mock_data, latitude = 40, longitude = -74, tz = "America/New_York")

    has_rect <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomRect")))
    expect_true(has_rect)
})

test_that("plot_species fills zeros based on full data range", {
    # Scenario: Bird A present at T1 and T3. Bird B present only at T2.
    # Plotting Bird B should still show T1(0), T2(1), T3(0).

    t1 <- lubridate::ymd_hms("2024-01-01 10:00:00")
    t2 <- lubridate::ymd_hms("2024-01-01 11:00:00")
    t3 <- lubridate::ymd_hms("2024-01-01 12:00:00")

    mock_data <- dplyr::tibble(
        `Common name` = c("Bird A", "Bird B", "Bird A"),
        recording_window_time = c(t1, t2, t3),
        Confidence = c(1.0, 1.0, 1.0)
    )

    p <- plot_species(mock_data, species_list = "Bird B", confidence_threshold = 0.5, unit = "hour")
    plot_data <- p$data

    expect_equal(nrow(plot_data), 3)

    # specific checks
    row_t1 <- plot_data %>% dplyr::filter(time_bin == t1)
    expect_equal(row_t1$n, 0)

    row_t3 <- plot_data %>% dplyr::filter(time_bin == t3)
    expect_equal(row_t3$n, 0)
})


test_that("plot_species filters correctly", {
    mock_data <- dplyr::tibble(
        `Common name` = c("Robin", "Sparrow", "Eagle"),
        recording_window_time = lubridate::ymd_hms("2024-01-01 12:00:00"),
        Confidence = 0.9
    )

    p <- plot_species(mock_data, species_list = c("Robin"), confidence_threshold = 0.5)
    plot_df <- p$data

    expect_true("Robin" %in% plot_df$`Common name`)
    expect_false("Sparrow" %in% plot_df$`Common name`)
    expect_false("Eagle" %in% plot_df$`Common name`)
})

test_that("plot_top_species fills zeros correctly", {
    t1 <- lubridate::ymd_hms("2024-01-01 10:00:00")
    t2 <- lubridate::ymd_hms("2024-01-01 11:00:00")
    t3 <- lubridate::ymd_hms("2024-01-01 12:00:00")

    mock_data <- dplyr::tibble(
        `Common name` = c("Bird A", "Bird A"),
        recording_window_time = c(t1, t3),
        Confidence = c(1.0, 1.0)
    )

    p <- plot_top_species(mock_data, n_top_species = 1, confidence_threshold = 0.5, unit = "hour")
    plot_data <- p$data

    expect_equal(nrow(plot_data), 3) # Gap filled
    expect_equal(plot_data$n[2], 0)
})

test_that("plot_top_species adds shading layer", {
    skip_if_not_installed("suncalc")

    mock_data <- dplyr::tibble(
        `Common name` = "Owl",
        recording_window_time = seq(
            lubridate::ymd_hms("2024-01-01 12:00:00", tz = "UTC"),
            lubridate::ymd_hms("2024-01-02 12:00:00", tz = "UTC"),
            by = "1 hour"
        ),
        Confidence = 0.9
    )

    p <- plot_top_species(mock_data, latitude = 40, longitude = -74, tz = "America/New_York")

    has_rect <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomRect")))
    expect_true(has_rect)
})

test_that("plot_species fills zeros based on full data range", {
    # Scenario: Bird A present at T1 and T3. Bird B present only at T2.
    # Plotting Bird B should still show T1(0), T2(1), T3(0).

    t1 <- lubridate::ymd_hms("2024-01-01 10:00:00")
    t2 <- lubridate::ymd_hms("2024-01-01 11:00:00")
    t3 <- lubridate::ymd_hms("2024-01-01 12:00:00")

    mock_data <- dplyr::tibble(
        `Common name` = c("Bird A", "Bird B", "Bird A"),
        recording_window_time = c(t1, t2, t3),
        Confidence = c(1.0, 1.0, 1.0)
    )

    p <- plot_species(mock_data, species_list = "Bird B", confidence_threshold = 0.5, unit = "hour")
    plot_data <- p$data

    expect_equal(nrow(plot_data), 3)

    # specific checks
    row_t1 <- plot_data %>% dplyr::filter(time_bin == t1)
    expect_equal(row_t1$n, 0)

    row_t3 <- plot_data %>% dplyr::filter(time_bin == t3)
    expect_equal(row_t3$n, 0)
})

test_that("plot_top_species handles log_scaling correctly", {
    mock_data <- dplyr::tibble(
        `Common name` = "Bird A",
        recording_window_time = seq(
            lubridate::ymd_hms("2024-01-01 10:00:00"),
            lubridate::ymd_hms("2024-01-01 15:00:00"),
            by = "1 hour"
        ),
        Confidence = 1.0
    )
    # Filter to only two points to create zeros in between
    mock_data <- mock_data[c(1, 6), ]

    # Default (log_scaling = FALSE)
    p_linear <- plot_top_species(mock_data, log_scaling = FALSE)
    # Check y scale - should be linear (default)
    expect_null(p_linear$scales$get_scales("y")) # Or check for absence of trans

    # log_scaling = TRUE
    p_log <- plot_top_species(mock_data, log_scaling = TRUE)

    # In ggplot2, if a scale is added, it will be in p$scales
    # We expect a scale with a pseudo_log transformation
    y_scale <- p_log$scales$get_scales("y")
    expect_false(is.null(y_scale))
    expect_match(y_scale$trans$name, "pseudo_log")
})
