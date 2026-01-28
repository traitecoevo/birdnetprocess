#' @title Plot Species Counts
#' @description
#' Takes BirdNET data and creates a figure representing number of recordings of
#' each species, over the given confidence interval.
#' @param df The dataframe created by the read_birdnet_file or
#'        read_birdnet_folder function
#' @param confidence The minimum confidence level for the species identifications
#' @param remove.dominants Removes dominant species from plot for cleaner
#'        visualisation
#' @return A plot showing the number of calls per species + prints removed
#'         species if applicable
#' @export
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @examples
#' \dontrun{
#' plot_species_counts(df, 0.5)
#' }
#'
plot_species_counts <- function(df, confidence = 0, remove.dominants = FALSE) {
  # filters the dataframe by the confidence given + removes 'nocall'
  df1 <- df |>
    dplyr::filter(Confidence > confidence & `Common Name` != "nocall") |>
    dplyr::group_by(`Common Name`) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::arrange(dplyr::desc(n))

  # extract date to print later
  min_date <- format(lubridate::date(min(df$start_time)), "%d %B %Y")
  max_date <- format(lubridate::date(max(df$start_time)), "%d %B %Y")
  date_range <- paste(min_date, "-", max_date)

  # to handle dominant species skewing the figure and making it illegible,
  # (i.e 1000000000000 white-winged fairywren calls)
  # find species where # calls is > 10 * mean & > 5 * other birds
  # so they can be scaled by /10 on a secondary axis if remove.dominants = F
  # or they can be removed if remove.dominants = T
  df1 <- df1 |>
    dplyr::mutate(
      highest_non_dom = max(n[!n > 10 * mean(n)]),
      is_dom = n > 10 * mean(n) & n > 5 * highest_non_dom,
      n_scaled = ifelse(is_dom, n / 10, n)
    )

  if (nrow(df1) < 2) {
    stop("one or fewer species detected at this confidence level, figure will not generate")
  }

  if (remove.dominants == FALSE) {
    if (any(df1$is_dom)) {
      # make colour gradient + diff colour to symbolise dominant species axis
      df1 <- df1 |>
        dplyr::mutate(fill_colour = ifelse(
          is_dom,
          "red",
          suppressWarnings(scales::col_numeric(
            palette = c("#ffe6f0", "#ff1493"),
            domain = df1$n_scaled[!df1$is_dom]
          )(n_scaled))
        ))

      # plot
      plot <- ggplot2::ggplot(
        df1,
        ggplot2::aes(
          x = stats::reorder(`Common Name`, n),
          y = n_scaled,
          fill = fill_colour
        )
      ) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_identity(guide = "none") +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.1)), # remove space below x
          name = "recordings",
          sec.axis = ggplot2::sec_axis(
            ~ . * 10,
            name = "recordings (dominant species)"
          )
        ) +
        ggplot2::labs(y = "recordings", x = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 90, hjust = 1, vjust = 0.5, size = 8
          ),
          axis.text.y.right = ggplot2::element_text(colour = "red"),
          axis.title.y.right = ggplot2::element_text(
            colour = "red", margin = ggplot2::margin(l = 10)
          ),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(color = "grey97"),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.title.x.bottom = ggplot2::element_text(
            hjust = 0.5, size = 10, margin = ggplot2::margin(t = 10)
          )
        )
    } else {
      # plot
      plot <- ggplot2::ggplot(
        df1,
        ggplot2::aes(
          x = stats::reorder(`Common Name`, n),
          y = n_scaled,
          fill = n
        )
      ) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_gradient(
          low = "#ffe6f0", high = "#ff1493", guide = "none"
        ) +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.1)), # remove space below x
          name = "recordings"
        ) +
        ggplot2::labs(y = "recordings", x = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 90, hjust = 1, vjust = 0.5, size = 8
          ),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(color = "grey97"),
          axis.title.x.bottom = ggplot2::element_text(
            hjust = 0.5, size = 10, margin = ggplot2::margin(t = 10)
          )
        )
    }
  } else {
    # store dominants to print to user knows which birds removed from plot
    dominants <- df1 |>
      dplyr::filter(is_dom == TRUE)

    # print for user the removed dominant species
    if (nrow(dominants) > 0) {
      cat("Dominant species removed:\n")
      dominants |>
        dplyr::mutate(
          dominants_info = paste0("- ", `Common Name`, " (n = ", n, ")")
        ) |>
        dplyr::pull(dominants_info) |>
        cat(sep = "\n")
    } else {
      cat("No dominant species removed.\n")
    }

    # filter to remove dominant species
    df1 <- df1 |>
      dplyr::filter(is_dom != TRUE)

    # plot
    plot <- ggplot2::ggplot(
      df1,
      ggplot2::aes(
        x = stats::reorder(`Common Name`, n),
        y = n_scaled,
        fill = n
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient(
        low = "#ffe6f0", high = "#ff1493", guide = "none"
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.1)), # remove space below x
        name = "recordings"
      ) +
      ggplot2::labs(y = "recordings", x = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90, hjust = 1, vjust = 0.5, size = 8
        ),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "grey97"),
        axis.title.x.bottom = ggplot2::element_text(
          hjust = 0.5, size = 10, margin = ggplot2::margin(t = 10)
        )
      )
  }
  if (interactive()) {
    cat("recordings between", date_range, " with confidence > ", confidence)
  }
  plot
}
