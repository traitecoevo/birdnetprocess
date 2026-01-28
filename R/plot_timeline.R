#' @title Plot Timeline
#' @description
#' Creates a simple line graph showing number of calls over time.
#' @param df The dataframe created by the `read_birdnet_file` or
#' `read_birdnet_folder` function.
#' @param confidence The minimum confidence level for the bird call
#' identifications.
#' @param bird_names String of bird names for which recordings over time
#' will be shown.
#' @return A figure showing number of calls over time for the given confidence
#' interval.
#' @export
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @examples
#' \dontrun{
#' plot_timeline(df, 0.5)
#' }
plot_timeline <- function(df, confidence = 0,
                          bird_names = unique(df$`Common Name`)) {
  # filter by the names included (if left blank, all birds included)
  df <- df |>
    dplyr::filter(`Common Name` != "nocall" & `Common Name` %in% bird_names)

  # pattern for extracting time
  pattern <- "(\\d{4}-\\d{2}-\\d{2})"

  # single out date only
  matches <- regmatches(
    format(df$start_time),
    regexpr(pattern, format(df$start_time))
  )
  df$date <- as.Date(matches)

  date_range <- paste(
    format(lubridate::date(min(df$start_time)), "%d %B %Y"),
    "-",
    format(lubridate::date(max(df$start_time)), "%d %B %Y")
  )

  # remove no call and filter by confidence, group by date
  df1 <- df |>
    dplyr::filter(Confidence > confidence, `Common Name` != "nocall") |>
    dplyr::group_by(date) |>
    dplyr::summarise(n = dplyr::n())

  # plot
  plot <- ggplot(df1, aes(x = date, y = n, color = )) +
    geom_line() +
    labs(y = "recordings", x = "") +
    scale_x_date(
      date_labels = "%b %y",
      expand = expansion(mult = c(0, 0.1))
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey97")
    )

  if (interactive()) {
    cat("recordings between", date_range, " with confidence > ", confidence)
  }

  plot
}
