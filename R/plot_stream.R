#' Plot Species Stream
#'
#' @description
#' Creates a streamgraph showing the number of calls over time for selected species. This visualization
#' allows for the comparison of species prevalence across different times or seasons.
#'
#' @param df A data frame representing BirdNET results, ideally created by `read_birdnet_file` or `read_birdnet_folder`.
#'   Must contain columns: `start_time`, `Common Name`, and `Confidence`.
#' @param confidence Numeric. The minimum confidence level (0-1) for species identifications to be included.
#'   Defaults to 0.
#' @param bird.names Character vector. One or more species names ("Common Name") to visualize.
#'   Defaults to all unique species in the provided dataframe.
#' @param bw Numeric. The bandwidth for the kernel density estimation performed by `ggstream`.
#'   Adjusts the smoothness of the stream. Defaults to 0.75.
#'
#' @details
#' This function requires the `ggstream` package to be installed. It filters the data based on the provided
#' confidence threshold and species names, then aggregates the count of calls per day.
#' The resulting plot is a "ridge" type streamgraph.
#'
#' @return A `ggplot` object showing the streamgraph of species calls over time.
#'
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' # Assuming 'results' is a dataframe from read_birdnet_folder()
#' # Plot stream for specific species with a confidence threshold
#' plot_species_stream(results, confidence = 0.5, bird.names = c("Galah", "Magpie-lark"))
#' }
#'
#' # Runnable example with mock data
#' data <- data.frame(
#'   start_time = as.POSIXct(c(
#'     "2023-01-01 12:00:00", "2023-01-02 12:00:00",
#'     "2023-01-01 13:00:00", "2023-01-03 12:00:00"
#'   )),
#'   `Common Name` = c("Species A", "Species A", "Species B", "Species B"),
#'   Confidence = c(0.9, 0.8, 0.95, 0.7),
#'   check.names = FALSE
#' )
#' if (requireNamespace("ggstream", quietly = TRUE)) {
#'   plot_species_stream(data, confidence = 0.5)
#' }
#'
plot_species_stream <- function(df, confidence = 0,
                                bird.names = unique(df$`Common Name`),
                                bw = 0.75) {
  # pattern for date only
  pattern <- "(\\d{4}-\\d{2}-\\d{2})"

  # new column for date only, format to revert to character
  # Base R equivalent of str_extract
  matches <- regmatches(df$start_time, regexpr(pattern, df$start_time))
  # Handle potential no-matches (though inputs should have valid times)
  # regexpr returns -1 if no match.
  df$date <- as.Date(matches)

  df1 <- df %>%
    filter(Confidence > confidence, `Common Name` %in% bird.names) %>%
    filter(`Common Name` != "nocall") %>%
    group_by(date, `Common Name`) %>%
    summarise(n = n())

  # from the tanagr bird palette package https://github.com/cdanielcadena/tanagR?tab=readme-ov-file
  palette <- c("#F6AD4F", "#A45336", "#E6E8DB", "#6CB9A9", "#49A5D6", "#000A1A")

  # plot - add bw
  if (!requireNamespace("ggstream", quietly = TRUE)) {
    stop("Package \"ggstream\" needed for this function to work. Please install it.")
  }

  plot <- ggplot(df1, aes(x = date, y = n, fill = `Common Name`)) +
    ggstream::geom_stream(type = "ridge", alpha = 0.7, bw = bw, lwd = 0.25, color = 1) +
    labs(y = "Relative frequency", x = "", fill = "species name") +
    scale_x_date(date_labels = "%b %y") +
    scale_fill_manual(values = palette) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.grid.major = element_line(color = "grey97"),
      axis.text.y = element_blank()
    )

  return(plot)
}
