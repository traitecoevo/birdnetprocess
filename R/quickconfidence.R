#'@title quick confidence
#'@description
#'creates a streamgraph showing confidence of recorded calls over time
#'@param df the dataframe created by the read_birdnet_file or read_birdnet_folder function
#'@param confidence optional - the minimum confidence level for the bird call identifications
#'@return a streamgraph indicating confidence of calls over time
#'@export
#'@import lubridate
#'@import dplyr
#'@import ggplot2
#'@import ggstream
#'@examples
#'\dontrun{ quickconfidence(df, confidence = 0) }

quickconfidence <- function(df, confidence = 0){

# mutate confidence into bins
df <- df %>%
  mutate(confidence_bin = case_when(Confidence > 0 & Confidence <= 0.1 ~ '0 - 0.10',
                                    Confidence > 0.1 & Confidence <= 0.2 ~ '0.1 - 0.2',
                                    Confidence > 0.2 & Confidence <= 0.3 ~ '0.2 - 0.3',
                                    Confidence > 0.3 & Confidence <= 0.4 ~ '0.3 - 0.4',
                                    Confidence > 0.4 & Confidence <= 0.5 ~ '0.4 - 0.5',
                                    Confidence > 0.5 & Confidence <= 0.6 ~ '0.5 - 0.6',
                                    Confidence > 0.6 & Confidence <= 0.7 ~ '0.6 - 0.7',
                                    Confidence > 0.7 & Confidence <= 0.8 ~ '0.7 - 0.8',
                                    Confidence > 0.8 & Confidence <= 0.9 ~ '0.8 - 0.9',
                                    Confidence > 0.9 & Confidence <= 1 ~ '0.9 - 1.0'))

# pattern for date extraction
pattern <- "(\\d{4}-\\d{2}-\\d{2})"

# extract date
df$date <- df$start_time %>%
  format() %>%
  str_extract(pattern = pattern) %>%
  date()

# filter out no call, filter by confidence (default is 0)
df1 <- df %>%
  filter(`Common Name` != 'nocall', Confidence > confidence) %>%
  group_by(date, confidence_bin) %>%
  summarise(n = n())

# a bird green palette!
palette = c("#FFFFFF", "#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529")

# plot
plot <- ggplot(df1, aes(x = date, y = n, fill = confidence_bin)) +
  geom_stream(type = 'ridge', alpha = 0.7, lwd = 0.25, color = 1) +
  labs(y = 'recordings', x = '', fill = 'confidence') +
  scale_x_date(date_labels = "%b %y") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(color = 'grey97'))

return(plot)
}
