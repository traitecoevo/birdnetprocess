#' @title Plot Confidence
#' @description
#' Creates a streamgraph showing confidence of recorded calls over time.
#' @param df The dataframe created by the read_birdnet_file or
#'        read_birdnet_folder function.
#' @param confidence Optional - the minimum confidence level for the bird call
#'        identifications.
#' @param bw Bandwidth for the density estimation (default: 0.75).
#' @return A streamgraph indicating confidence of calls over time.
#' @export
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @import ggstream
#' @examples
#' \dontrun{
#' plot_confidence(df, confidence = 0)
#' }
plot_confidence <- function(df, confidence = 0, bw = 0.75) {
  # mutate confidence into bins
  df <- df |>
    dplyr::mutate(confidence_bin = dplyr::case_when(
      Confidence > 0 & Confidence <= 0.1 ~ "0 - 0.10",
      Confidence > 0.1 & Confidence <= 0.2 ~ "0.1 - 0.2",
      Confidence > 0.2 & Confidence <= 0.3 ~ "0.2 - 0.3",
      Confidence > 0.3 & Confidence <= 0.4 ~ "0.3 - 0.4",
      Confidence > 0.4 & Confidence <= 0.5 ~ "0.4 - 0.5",
      Confidence > 0.5 & Confidence <= 0.6 ~ "0.5 - 0.6",
      Confidence > 0.6 & Confidence <= 0.7 ~ "0.6 - 0.7",
      Confidence > 0.7 & Confidence <= 0.8 ~ "0.7 - 0.8",
      Confidence > 0.8 & Confidence <= 0.9 ~ "0.8 - 0.9",
      Confidence > 0.9 & Confidence <= 1 ~ "0.9 - 1.0"
    ))

  # pattern for date extraction
  pattern <- "(\\d{4}-\\d{2}-\\d{2})"

  # extract date
  df$date <- df$start_time |>
    format() |>
    stringr::str_extract(pattern = pattern) |>
    lubridate::date()

  # filter out no call, filter by confidence (default is 0)
  df1 <- df |>
    dplyr::filter(`Common Name` != "nocall", Confidence > confidence) |>
    dplyr::group_by(date, confidence_bin) |>
    dplyr::summarise(n = dplyr::n())

  # a bird green palette!
  palette <- c(
    "#FFFFFF", "#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E",
    "#78C679", "#41AB5D", "#238443", "#006837", "#004529"
  )

  # plot
  plot <- ggplot2::ggplot(
    df1,
    ggplot2::aes(x = date, y = n, fill = confidence_bin)
  ) +
    ggstream::geom_stream(
      type = "ridge",
      alpha = 0.7,
      bw = bw,
      lwd = 0.25,
      color = 1
    ) +
    ggplot2::labs(y = "Relative frequency", x = "", fill = "confidence") +
    ggplot2::scale_x_date(date_labels = "%b %y") +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey97"),
      axis.text.y = ggplot2::element_blank()
    )

  plot
}
