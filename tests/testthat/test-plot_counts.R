test_that("plot_species_counts returns a ggplot object", {
    # Mock data
    df <- dplyr::tibble(
        `Common Name` = c(rep("Robin", 10), rep("Sparrow", 5), "nocall"),
        Confidence = c(rep(0.9, 10), rep(0.8, 5), 0.1),
        start_time = seq(
            from = lubridate::ymd_hms("2024-01-01 10:00:00"),
            by = "1 min",
            length.out = 16
        )
    )

    p <- plot_species_counts(df, confidence = 0.5, remove.dominants = FALSE)
    expect_s3_class(p, "ggplot")

    # Check data in plot
    plot_data <- p$data
    expect_true("Robin" %in% plot_data$`Common Name`)
    expect_true("Sparrow" %in% plot_data$`Common Name`)
    expect_false("nocall" %in% plot_data$`Common Name`)
})

test_that("plot_species_counts handles remove.dominants = TRUE", {
    # Mock data with a dominant species
    # Robin: 100 calls (mean will be high)
    # Sparrow: 5 calls
    # Eagle: 2 calls
    # Mean ~ (100+5+2)/3 = 35.6
    # 10*mean = 356 -> Wait, let's adjust to trigger logic
    # Logic: n > 10 * mean(n) & n > 5 * highest_non_dom

    # Let's try:
    # A: 1000
    # B: 10
    # C: 10
    # Mean = 340. 10*Mean = 3400. 1000 is NOT > 3400.

    # The logic in plot_species_counts seems hard to trigger with small numbers or specific distributions.
    # Let's look at the logic:
    # n > 10 * mean(n)
    # If we have [1000, 1, 1], mean is 334. 10*mean is 3340. 1000 is not > 3340.
    # If we have [100, 1, 1, 1, 1, 1, 1, 1, 1, 1], mean is ~10.9. 10*mean = 109. 100 is not > 109.

    # Wait, if n is HUGE.
    # Let's say [1000, 1]. Mean = 500.5. 10*mean = 5005.
    # It seems mathematically hard to satisfy n > 10 * mean(n) if n is included in the mean calculation?
    # sum / N = mean.
    # max > 10 * (sum / N) -> max * N > 10 * sum
    # If max is the only large one, sum ~ max.
    # max * N > 10 * max -> N > 10.

    # So we need at least 11 species for this to be possible!

    species <- c("Dom", paste0("Sp", 1:15))
    counts <- c(1000, rep(1, 15))
    # Total species = 16.
    # Sum = 1015. Mean = 63.4.
    # 10 * Mean = 634.
    # 1000 > 634. Should be dominant.

    df_list <- lapply(seq_along(species), function(i) {
        dplyr::tibble(
            `Common Name` = rep(species[i], counts[i]),
            Confidence = 0.9,
            start_time = lubridate::ymd_hms("2024-01-01 10:00:00")
        )
    })
    df <- dplyr::bind_rows(df_list)

    # Capture output to check for print message
    expect_output(
        p <- plot_species_counts(df, confidence = 0.5, remove.dominants = TRUE),
        "Dominant species removed"
    )

    expect_s3_class(p, "ggplot")
    plot_data <- p$data
    expect_false("Dom" %in% plot_data$`Common Name`)
    expect_true("Sp1" %in% plot_data$`Common Name`)
})

test_that("plot_species_counts errors if not enough species", {
    df <- dplyr::tibble(
        `Common Name` = c("Robin"),
        Confidence = c(0.9),
        start_time = lubridate::ymd_hms("2024-01-01 10:00:00")
    )

    expect_error(plot_species_counts(df, confidence = 0.5), "one or fewer species detected")
})
