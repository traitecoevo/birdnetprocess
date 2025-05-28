#'@title quick calls
#'@description
#'takes birdnet .txt data creates a figure representing number of recordings of
#'each bird, over the given confidence interval
#'@param df the dataframe created by the read_birdnet_file or read_birdnet_folder function
#'@param confidence the minimum confidence level for the bird call identifications
#'@param remove.dominants removes dominant species from plot for cleaner visualisation
#'@return a plot showing the number of calls per bird species + prints removed species if applicable
#'@export
#'@import lubridate
#'@import dplyr
#'@import ggplot2
#'@examples
#'\dontrun{ quickcalls(df, 0.5) }
#'
quickcalls <- function(df, confidence = 0, remove.dominants = F){

  # filters the dataframe by the confidence given + removes 'nocall'
  df1 <- df %>%
    filter(Confidence > confidence, `Common Name` != 'nocall') %>%
    group_by(`Common Name`) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

  # extract date to print later
  date_range <- paste(format(lubridate::date(min(df$start_time)), "%d %B %Y"), " - ", format(date(max(df$start_time)), "%d %B %Y"))

  # to handle dominant species skewing the figure and making it illegible,
  # (i.e 1000000000000 white-winged fairywren calls)
  # find species where # calls is > 10 * mean & > 5 * other birbs
  # so they can be scaled by /10 on a secondary axis if remove.dominants = F
  # or they can be removed if remove.dominants = T
  df1 <- df1 %>%
    mutate(highest_non_dom = max(n[!n > 10 * mean(n)]),
           is_dom = n > 10 * mean(n) & n > 5 * highest_non_dom,
           n_scaled = ifelse(is_dom, n / 10, n))

  if(nrow(df1) < 2){
    stop("one or fewer species detected at this confidence level, figure will not generate")
  }

  if (remove.dominants == F){

  if (any(df1$is_dom)){

    # make colour gradient + diff colour to symbolise dominant species axis
    df1 <- df1 %>%
      mutate(fill_colour = ifelse(is_dom, "#708090",
                                  suppressWarnings(scales::col_numeric(palette = c("#a1dab4", "#41b6c4"),
                                                      domain = df1$n_scaled[!df1$is_dom])(n_scaled))))

    # plot
    plot <- ggplot(df1, aes(x = reorder(`Common Name`, n), y = n_scaled, fill = fill_colour)) +
      geom_bar(stat = 'identity') +
      scale_fill_identity(guide = 'none') +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)), # remove space below x axis
                         name = "recordings",
                         sec.axis = sec_axis(~ . * 10, name = "recordings (dominant species)")) +
      labs(y = 'recordings', x = '') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
            axis.text.y.right = element_text(colour = '#708090'),
            axis.title.y.right = element_text(colour = '#708090', margin = margin(l = 10)),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = 'grey97'),
            panel.grid.major.x = element_blank(),
            axis.title.x.bottom = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)))
  } else {
    # plot
    plot <- ggplot(df1, aes(x = reorder(`Common Name`, n), y = n_scaled, fill = n)) +
      geom_bar(stat = 'identity') +
      scale_fill_gradient(low = "#a1dab4", high = "#41b6c4", guide = 'none') +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)), # remove space below x axis
                         name = "recordings") +
      labs(y = 'recordings', x = '') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major = element_line(color = 'grey97'),
            axis.title.x.bottom = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)))
  }
} else {

  # store dominants to print to user knows which birbs removed from plot
  dominants <- df1 %>%
    filter(is_dom == TRUE)

  # print for user the removed dominant species
  if (nrow(dominants) > 0) {
    cat("Dominant species removed:\n")
    dominants %>%
      mutate(dominants_info = paste0("- ", `Common Name`, " (n = ", n, ")")) %>%
      pull(dominants_info) %>%
      cat(sep = "\n")
  } else {
    cat("No dominant species removed.\n")
  }

    # filter to remove dominant species
    df1 <- df1 %>%
      filter(is_dom != TRUE)

    # plot
    plot <- ggplot(df1, aes(x = reorder(`Common Name`, n), y = n_scaled, fill = n)) +
      geom_bar(stat = 'identity') +
      scale_fill_gradient(low = "#a1dab4", high = "#41b6c4", guide = 'none') +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)), # remove space below x axis
                         name = "recordings") +
      labs(y = 'recordings', x = '') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major = element_line(color = 'grey97'),
            axis.title.x.bottom = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)))

}
  cat("recordings between", date_range, " with confidence > ", confidence)
  return(plot)

}

