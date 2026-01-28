#' @title Summarise Detections
#' @description
#' Takes BirdNET results and offers a few quick statistics.
#'
#' @param df The dataframe created by the read_birdnet_file or read_birdnet_folder function.
#' @param confidence The minimum confidence level for the bird call identifications.
#' @return A tibble of summary statistics.
#' @export
#' @import tibble
#' @import lubridate
#' @import dplyr
#' @examples
#' \dontrun{
#' summarise_detections(df, 0.5)
#' }
#'
summarise_detections <- function(df, confidence = 0) {
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
      most_common_bird = "Most common bird",
      peak_hour = "Peak hour",
      av_recordings_per_day = "Average recordings per day",
      av_recordings_per_hour = "Average recordings per hour"
    ))
}
