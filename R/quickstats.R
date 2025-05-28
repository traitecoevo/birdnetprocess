#'@title quick statistics
#'@description
#'takes birdnet .txt data and offers a few quick statistics
#'
#'@param df the dataframe created by the read_birdnet_file or read_birdnet_folder function
#'@param confidence the minimum confidence level for the bird call identifications
#'@return a tibble of quick statistics
#'@export
#'@import tibble
#'@import lubridate
#'@import dplyr
#'@examples
#'\dontrun{ quickstats(df, 0.5) }
#'
quickstats <- function(df, confidence = 0){

  # filter by confidence and remove 'nocall'
  df <- df %>% filter(Confidence > confidence, `Common Name` != 'nocall')

  # list of stats
  stats <- list(
    n_species = length(unique(df$`Common Name`)),
    n_recordings = nrow(df),
    recording_window = paste0(format(as_date(substr(min(df$start_time), 1, 10)), '%d %b %y'), " - ", format(as_date(substr(max(df$start_time), 1, 10)), '%d %b %y')),
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
  enframe(unlist(stats), name = "statistic", value = "value")
}

