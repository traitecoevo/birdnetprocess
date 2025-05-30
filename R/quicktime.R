#'@title quick time
#'@description
#'creates a simple line graph showing number of calls over time
#'@param df the dataframe created by the read_birdnet_file or read_birdnet_folder function
#'@param confidence the minimum confidence level for the bird call identifications
#'@param bird.names string of bird names for which recordings over time will be shown
#'@return a figure showing number of calls over time for the given confidence interval
#'@export
#'@import lubridate
#'@import dplyr
#'@import ggplot2
#'@import stringr
#'@examples
#'\dontrun{ quicktime(df, 0.5) }

quicktime <- function(df, confidence = 0, bird.names = unique(df$`Common Name`)){

# filter by the names included (if left blank, all birds included)
df <- df %>%
  filter(`Common Name` != 'nocall' &`Common Name` %in% bird.names)

# pattern for extracting time
pattern <- "(\\d{4}-\\d{2}-\\d{2})"

# single out date only
df$date <- df$start_time %>%
  format() %>%
  stringr::str_extract(pattern = pattern) %>%
  date()

date_range <- paste(format(lubridate::date(min(df$start_time)), "%d %B %Y"), " - ", format(date(max(df$start_time)), "%d %B %Y"))

# remove no call and filter by confidence, group by date
df1 <- df %>%
  filter(Confidence > confidence, `Common Name` != 'nocall') %>%
  group_by(date) %>%
  summarise(n = n())

# plot
plot <- ggplot(df1, aes(x = date, y = n, color = )) +
  geom_line() +
  labs(y = 'recordings', x = '') +
  scale_x_date(date_labels = "%b %y",
               expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'grey97'))

if (!shiny::isRunning()) {
  cat("recordings between", date_range, " with confidence > ", confidence)
  }

return(plot)
}

