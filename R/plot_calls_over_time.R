#' Plot Top N BirdNET Species
#'
#' @param data A data frame containing BirdNET detections.
#' @param n_top_species Integer. Number of top species to show. Default 10.
#' @param confidence_threshold Numeric. Minimum confidence. Default 0.5.
#' @param unit Character. Time unit for aggregation, passed to `lubridate::floor_date`. Default "hour".
#' @param facet_by Character. Optional output column name to facet by (e.g. "Site").
#' @param latitude Numeric. Optional. Latitude for day/night shading (requires `suncalc`).
#' @param longitude Numeric. Optional. Longitude for day/night shading (requires `suncalc`).
#' @param tz Character. Timezone for day/night shading alignment. Default "UTC".
#' @param ... Additional arguments passed to internal plotting.
#' @export
plot_top_species <- function(data, n_top_species = 10, confidence_threshold = 0.5, unit = "hour", facet_by = NULL, latitude = NULL, longitude = NULL, tz = "UTC", ...) {
    # Validation
    data <- validate_birdnet_data(data)

    # Filter by confidence
    data_filtered <- data %>% dplyr::filter(Confidence >= confidence_threshold)
    if (nrow(data_filtered) == 0) {
        return(NULL)
    }

    # Identify Top Species
    top_species <- data_filtered %>%
        dplyr::count(`Common name`, sort = TRUE) %>%
        dplyr::slice_head(n = n_top_species) %>%
        dplyr::pull(`Common name`)

    if (length(top_species) == 0) {
        return(NULL)
    }

    # Calculate full time limits from the filtered data (effort)
    time_limits <- range(data_filtered$recording_window_time, na.rm = TRUE)

    # Filter to Top Species
    data_final <- data_filtered %>%
        dplyr::filter(`Common name` %in% top_species)

    # Plot
    plot_aggregated_data(
        data_final,
        species_list = top_species,
        time_limits = time_limits,
        unit = unit,
        facet_by = facet_by,
        latitude = latitude,
        longitude = longitude,
        tz = tz,
        ...
    )
}

#' Plot Specific BirdNET Species
#'
#' @param data A data frame containing BirdNET detections.
#' @param species_list Character vector. Exact names of species to plot.
#' @param confidence_threshold Numeric. Minimum confidence. Default 0.5.
#' @param unit Character. Time unit for aggregation, passed to `lubridate::floor_date`. Default "hour".
#' @param facet_by Character. Optional output column name to facet by (e.g. "Site").
#' @param latitude Numeric. Optional. Latitude for day/night shading (requires `suncalc`).
#' @param longitude Numeric. Optional. Longitude for day/night shading (requires `suncalc`).
#' @param tz Character. Timezone for day/night shading alignment. Default "UTC".
#' @param ... Additional arguments passed to internal plotting.
#' @export
plot_species <- function(data, species_list, confidence_threshold = 0.5, unit = "hour", facet_by = NULL, latitude = NULL, longitude = NULL, tz = "UTC", ...) {
    # Validation
    data <- validate_birdnet_data(data)

    # Filter by confidence
    data_filtered <- data %>% dplyr::filter(Confidence >= confidence_threshold)
    if (nrow(data_filtered) == 0) {
        return(NULL)
    }

    # Identify full time limits BEFORE filtering by species
    time_limits <- range(data_filtered$recording_window_time, na.rm = TRUE)

    # Filter by List
    data_final <- data_filtered %>%
        dplyr::filter(`Common name` %in% species_list)

    # Plot
    # If no data found for those species, we still want to try plotting (will be empty except zeros)
    # or warn.
    if (nrow(data_final) == 0) {
        warning("No detections found for the specified species with given confidence.")
        return(NULL)
    }

    plot_aggregated_data(
        data_final,
        species_list = species_list,
        time_limits = time_limits,
        unit = unit,
        facet_by = facet_by,
        latitude = latitude,
        longitude = longitude,
        tz = tz,
        ...
    )
}

#' @export
plot_calls_over_time <- function(...) {
    warning("plot_calls_over_time() is deprecated. Please use plot_top_species() or plot_species().")
    plot_top_species(...)
}


# --- Internal Helper Functions ---

validate_birdnet_data <- function(data) {
    if ("Common Name" %in% names(data) && !"Common name" %in% names(data)) {
        data <- data %>% dplyr::rename(`Common name` = `Common Name`)
    }
    if (!"Common name" %in% names(data)) stop("Missing 'Common name' column.")
    if (!"recording_window_time" %in% names(data)) stop("Missing 'recording_window_time' column.")
    data
}

plot_aggregated_data <- function(data, species_list, time_limits, unit = "hour", facet_by = NULL, latitude = NULL, longitude = NULL, tz = "UTC") {
    # Prepare Plot Data (Aggregation)
    group_vars <- c("time_bin", "Common name")
    if (!is.null(facet_by)) {
        if (!facet_by %in% names(data)) stop(paste("Facet column", facet_by, "not found."))
        group_vars <- c(group_vars, facet_by)
    }

    if (nrow(data) > 0) {
        plot_data <- data %>%
            dplyr::mutate(time_bin = lubridate::floor_date(recording_window_time, unit = unit)) %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
            dplyr::summarise(n = dplyr::n(), .groups = "drop")
    } else {
        # Handle empty data (species not found but time limits exist)
        plot_data <- dplyr::tibble(
            time_bin = lubridate::floor_date(time_limits[1], unit = unit), # Dummy to set type
            `Common name` = character(),
            n = integer()
        ) %>% dplyr::slice(0) # Empty row with correct types
        if (!is.null(facet_by)) {
            plot_data[[facet_by]] <- character() # Add empty facet col
        }
    }

    # Zero Filling
    # Derive full sequence from time_limits
    # time_limits are POSIXct from the input data
    start_bin <- lubridate::floor_date(time_limits[1], unit = unit)
    end_bin <- lubridate::floor_date(time_limits[2], unit = unit)
    if (start_bin > end_bin) start_bin <- end_bin # Safety for single point

    full_time_seq <- seq(from = start_bin, to = end_bin, by = unit)

    if (is.null(facet_by)) {
        plot_data <- plot_data %>%
            tidyr::complete(time_bin = full_time_seq, `Common name` = species_list, fill = list(n = 0))
    } else {
        # If faceting, we need to know WHICH facets exist.
        # If data is empty for filtered species, we don't know the facets (e.g. sites) unless passed.
        # This is a limitation. If data_final is empty, we lose the Site names.
        # We can try to recover them from existing plot_data if not empty.
        # If empty, we can't facet properly (ggplot blank).

        # Assuming plot_data matches conventions
        if (nrow(plot_data) > 0) {
            plot_data <- plot_data %>%
                tidyr::complete(time_bin = full_time_seq, `Common name` = species_list, !!rlang::sym(facet_by), fill = list(n = 0))
        } else {
            # Data empty. Create grid for time/species, but we miss Facet Column values.
            # User sees empty plot. Acceptable fallback.
            plot_data <- tidyr::expand_grid(time_bin = full_time_seq, `Common name` = species_list) %>%
                dplyr::mutate(n = 0)
            # Warning: Facet column missing. ggplot facet will fail if variables missing.
            # Ideally we'd need 'site_list' too.
            # But for now, if no data, no plot.
        }
    }

    # Plotting
    p <- ggplot2::ggplot(plot_data)

    # Shading
    if (!is.null(latitude) && !is.null(longitude)) {
        if (requireNamespace("suncalc", quietly = TRUE)) {
            date_range <- seq(from = as.Date(min(full_time_seq)) - 1, to = as.Date(max(full_time_seq)) + 1, by = "day")
            sun_times <- suncalc::getSunlightTimes(date = date_range, lat = latitude, lon = longitude, keep = c("sunrise", "sunset"))

            align_sun <- function(t, tzone) lubridate::force_tz(lubridate::with_tz(t, tzone), "UTC")
            sun_times$sunset <- align_sun(sun_times$sunset, tz)
            sun_times$sunrise <- align_sun(sun_times$sunrise, tz)

            rects <- data.frame(
                xmin = sun_times$sunset[1:(nrow(sun_times) - 1)],
                xmax = sun_times$sunrise[2:nrow(sun_times)],
                ymin = -Inf, ymax = Inf
            )

            rects <- rects %>% dplyr::filter(xmax >= min(full_time_seq) & xmin <= max(full_time_seq))

            if (nrow(rects) > 0) {
                p <- p + ggplot2::geom_rect(
                    data = rects,
                    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                    fill = "grey20", alpha = 0.15, inherit.aes = FALSE
                )
            }
        }
    }

    # Main Layers
    if (nrow(plot_data) > 0) {
        p <- p +
            ggplot2::geom_line(ggplot2::aes(x = time_bin, y = n, color = `Common name`), linewidth = 1) +
            ggplot2::geom_point(ggplot2::aes(x = time_bin, y = n, color = `Common name`), size = 1.5, alpha = 0.7)
    }

    p <- p +
        ggplot2::labs(
            title = "BirdNET Detections",
            x = "Time", y = paste("Calls per", unit), color = "Species"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    if (!is.null(facet_by)) {
        p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_by)), scales = "free_y")
    }

    return(p)
}
