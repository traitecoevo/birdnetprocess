# Script to rebuild README plots using local SL21 data.
# Run this script to update the figures in man/figures/

devtools::load_all(".")
library(dplyr)
library(ggplot2)

# Check if data exists
if (!dir.exists("detections_SL21")) {
    stop("Directory 'detections_SL21' not found. Cannot rebuild plots.")
}

message("Reading data from detections_SL21...")
data <- read_birdnet_folder("detections_SL21", recursive = FALSE)

# 1. Top Species Counts (Log Scale)
message("Generating Top Species Counts Plot...")
p1 <- plot_species_counts(data, confidence = 0.5, log.scale = TRUE)
ggsave("man/figures/top10_calls_over_time_package.png", p1, width = 8, height = 6)
message("Saved man/figures/top10_calls_over_time_package.png")

# 2. Day/Night Stream Plot
message("Generating Day/Night Stream Plot...")
# Note: This takes common arguments. Ensure suncalc is installed.
if (requireNamespace("suncalc", quietly = TRUE)) {
    p2 <- plot_top_species(
        data,
        n_top_species = 10,
        confidence = 0.5,
        latitude = -32.44,
        longitude = 152.24,
        tz = "Australia/Sydney"
    )
    ggsave("man/figures/day_night_stream.png", p2, width = 10, height = 6)
    message("Saved man/figures/day_night_stream.png")
} else {
    warning("suncalc package not installed. Skipping Day/Night plot.")
}

message("Done.")
