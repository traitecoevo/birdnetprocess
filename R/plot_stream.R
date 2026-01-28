#' @title Plot Species Stream
#' @description
#' Creates a streamgraph showing number of calls over time for each species.
#' @param df The dataframe created by the `read_birdnet_file` or `read_birdnet_folder` function.
#' @param confidence The minimum confidence level for the bird call identifications.
#' @param bird.names One to six bird names, to view comparative prevalence over time/seasons.
#' @param bw The bandwidth for the kernel density estimation performed by the streamgraph.
#' @return A plot showing calls over time for each species input.
#' @export
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @examples
#' \dontrun{
#' plot_species_stream(df, 0.5, c("Galah", "Brown Songlark", "Little Corella"))
#' }
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
