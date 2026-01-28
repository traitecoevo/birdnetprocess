#' @title Summarise Detections
#' @description
#' Provides a quick summary of BirdNET results, including species counts,
#' total recordings, time range, and average detection rates.
#'
#' @details
#' This function filters the input data based on the provided confidence threshold
#' and then calculates several summary statistics:
#' \itemize{
#'   \item Number of unique species detected
#'   \item Total number of detections (recordings)
#'   \item Date range of the recording window
#'   \item The most frequently detected species
#'   \item The peak hour of activity
#'   \item Average number of detections per day
#'   \item Average number of detections per hour
#' }
#' It returns a tibble with two columns: `statistic` (the name of the metric)
#' and `value` (the calculated value formatted as a string).
#'
#' @param df A dataframe of BirdNET results, typically created by
#'        \code{\link{read_birdnet_file}} or \code{\link{read_birdnet_folder}}.
#' @param confidence Numeric. The minimum confidence level (0 to 1) for a detection
#'        to be included in the summary. Default is 0.5.
#' @return A tibble with columns `statistic` and `value`.
#' @export
#' @import tibble
#' @import lubridate
#' @import dplyr
#' @examples
#' \dontrun{
#' # Summarise with default confidence (0.5)
#' summarise_detections(df)
#'
#' # Summarise with higher confidence threshold
#' summarise_detections(df, confidence = 0.8)
#' }
summarise_detections <- function(df, confidence = 0.5) {
  # filter by confidence and remove 'nocall'
  df <- df %>% filter(Confidence > confidence, `Common Name` != "nocall")

  # list of stats
  stats <- list(
    n_species = length(unique(df$`Common Name`)),
    n_recordings = nrow(df),
    recording_window = paste0(format(as_date(substr(min(df$start_time), 1, 10)), "%d %b %y"), " - ", format(as_date(substr(max(df$start_time), 1, 10)), "%d %b %y")),
    most_common_bird = df %>%
      group_by(`Common Name`) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(1),
    peak_hour = df %>%
      group_by(start_time) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(1),
    av_recordings_per_day = df %>%
      mutate(date_only = as_date(start_time)) %>%
      count(date_only) %>%
      summarise(mean_per_day = mean(n)) %>%
      pull(),
    av_recordings_per_hour = df %>%
      count(start_time) %>%
      summarise(mean_per_hour = mean(n)) %>%
      pull()
  )

  # to keep date-time readable
  for (i in seq_along(stats)) {
    stats[[i]] <- format(stats[[i]])
  }

  # print vertical tibble
  enframe(unlist(stats), name = "statistic", value = "value") %>%
    mutate(statistic = recode(statistic,
      n_species = "Number of species",
      n_recordings = "Number of recordings",
      n_recordings = "Number of recordings",
      recording_window = "Recording window",
      most_common_bird = "Most common species",
      peak_hour = "Peak hour",
      av_recordings_per_day = "Average detections per day",
      av_recordings_per_hour = "Average detections per hour"
    ))
}
