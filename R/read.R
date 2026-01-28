#' Extract a start datetime from a BirdNET selection table filename
#'
#' This function assumes a filename of the form:
#'   SOMETHING_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt
#'
#' @param file_name The full name or path of the BirdNET selection file.
#' @return A POSIXct object representing the start datetime.
#' @export
#'
#' @examples
#' parse_birdnet_filename_datetime("1STSMM2_20241105_050000.BirdNET.selection.table.txt")
parse_birdnet_filename_datetime <- function(file_name) {
  # 1) Extract the chunk that looks like: YYYYMMDD_HHMMSS
  #    We'll do so with a regex capturing group
  #    We'll look for digits_ digits, i.e. something like 20241105_050000
  pattern <- "(\\d{8}_\\d{6})"
  match <- stringr::str_match(file_name, pattern)[, 2] # first capturing group

  if (is.na(match)) {
    stop("Filename does not match the expected pattern YYYYMMDD_HHMMSS.")
  }

  # 2) We have a string like "20241105_050000"
  #    Let's split it at the underscore.
  date_time_parts <- strsplit(match, "_")[[1]]
  date_part <- date_time_parts[1] # e.g. "20241105"
  time_part <- date_time_parts[2] # e.g. "050000"

  # 3) Convert date_part into "YYYY-MM-DD" and time_part into "HH:MM:SS"
  #    date_part "20241105" => "2024-11-05"
  #    time_part "050000"  => "05:00:00"
  date_formatted <- paste0(
    substr(date_part, 1, 4), "-",
    substr(date_part, 5, 6), "-",
    substr(date_part, 7, 8)
  )
  time_formatted <- paste0(
    substr(time_part, 1, 2), ":",
    substr(time_part, 3, 4), ":",
    substr(time_part, 5, 6)
  )

  # 4) Combine into one string and parse with lubridate
  date_time_str <- paste(date_formatted, time_formatted)
  start_time <- lubridate::ymd_hms(date_time_str, tz = "UTC") # or your local tz

  start_time
}

#' Read a single BirdNET selection table file
#'
#' @param file_path Path to a BirdNET .selection.table.txt file
#' @param tz Timezone if you want to override or specify a different timezone
#'           than what is parsed from the filename (if any). Default "UTC".
#'
#' @return A tibble with all columns from the BirdNET file plus:
#'   * `file_name` for reference
#'   * `start_time` the parsed date-time from the filename
#'   * `recording_window_time` the absolute time of each detection
#' @export
#'
#' @examples
#' \dontrun{
#' df <- read_birdnet_file("1STSMM2_20241105_050000.BirdNET.selection.table.txt")
#' head(df)
#' }
read_birdnet_file <- function(file_path, tz = "UTC") {
  file_name <- basename(file_path) # remove directory path
  start_time <- parse_birdnet_filename_datetime(file_name)

  # Check file extension to guess delimiter
  # But also be robust: try reading comma first if .csv, else tab
  if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    df <- readr::read_csv(file_path, show_col_types = FALSE)
  } else {
    df <- readr::read_delim(file_path, delim = "\t", show_col_types = FALSE)
  }

  # Ensure the Start Time column exists.
  # Standard BirdNET is "Begin Time (s)", but some exports/versions use "Start (s)"
  if ("Begin Time (s)" %in% names(df)) {
    df <- df |> dplyr::rename(begin_time_s = `Begin Time (s)`)
  } else if ("Start (s)" %in% names(df)) {
    df <- df |> dplyr::rename(begin_time_s = `Start (s)`)
  } else {
    stop(paste(
      "Could not find 'Begin Time (s)' or 'Start (s)' column in file:",
      file_path
    ))
  }

  # add columns
  df |>
    dplyr::mutate(
      file_name = file_name,
      start_time = start_time,
      recording_window_time = start_time + begin_time_s
    )
}

#' Read all BirdNET selection files in a folder
#'
#' @param folder A folder path containing BirdNET .selection.table.txt files
#' @param pattern A regex pattern to match the files.
#'        Default is "BirdNET.selection.table.txt$", meaning any file ending with that.
#' @param recursive Whether to search recursively in subfolders. Default FALSE.
#'
#' @return A single tibble combining all data from the matched files,
#'         with columns:
#'         - all columns from each BirdNET file
#'         - `file_name`
#'         - `start_time`
#'         - `recording_window_time`
#' @export
#'
#' @examples
#' \dontrun{
#' all_detections <- read_birdnet_folder("path/to/folder")
#' dplyr::glimpse(all_detections)
#' }
read_birdnet_folder <- function(folder = ".",
                                pattern = "BirdNET.selection.table.txt$",
                                recursive = FALSE) {
  # gather all matching files
  files <- list.files(
    path = folder,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive
  )

  if (length(files) == 0) {
    warning("No files found matching pattern in folder.")
    return(dplyr::tibble())
  }

  # read them all, combining into one data frame
  purrr::map_dfr(files, ~ read_birdnet_file(.x))
}

#' Read BirdNET selection files from multiple sites (folders)
#'
#' @param folder_paths A character vector of folder paths, one for each site.
#' @param pattern A regex pattern to match the files. Default "BirdNET.selection.table.txt$".
#' @param recursive Whether to search recursively in subfolders. Default FALSE.
#'
#' @return A single tibble combining all data.
#'         Includes a `Site` column derived from the folder name.
#' @export
#'
#' @examples
#' \dontrun{
#' folders <- c("detections_SiteA", "detections_SiteB")
#' all_sites <- read_birdnet_sites(folders, pattern = "BirdNET.results.csv$")
#' }
read_birdnet_sites <- function(folder_paths,
                               pattern = "BirdNET.selection.table.txt$",
                               recursive = FALSE) {
  # Helper to read one folder and add Site column
  read_one_site <- function(fp) {
    site_name <- basename(fp)
    df <- read_birdnet_folder(folder = fp, pattern = pattern, recursive = recursive)

    # If empty, return empty with Site column if possible, or just empty
    if (nrow(df) > 0) {
      df <- df |> dplyr::mutate(Site = site_name)
    }
    df
  }

  # Read all and combine
  purrr::map_dfr(folder_paths, read_one_site)
}
