

#' Extract a start datetime from a BirdNET selection table filename
#'
#' This function assumes a filename of the form:
#'   SOMETHING_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt
#'
#' @param file_name The full name or path of the BirdNET selection file.
#' @return A POSIXct object representing the start datetime.
#'
#' @examples
#' parse_birdnet_filename_datetime("1STSMM2_20241105_050000.BirdNET.selection.table.txt")
parse_birdnet_filename_datetime <- function(file_name) {
  # 1) Extract the chunk that looks like: YYYYMMDD_HHMMSS
  #    We'll do so with a regex capturing group
  #    We'll look for digits_ digits, i.e. something like 20241105_050000
  pattern <- "(\\d{8}_\\d{6})"
  match   <- stringr::str_match(file_name, pattern)[,2]  # first capturing group

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
    substr(date_part,1,4), "-",
    substr(date_part,5,6), "-",
    substr(date_part,7,8)
  )
  time_formatted <- paste0(
    substr(time_part,1,2), ":",
    substr(time_part,3,4), ":",
    substr(time_part,5,6)
  )

  # 4) Combine into one string and parse with lubridate
  date_time_str <- paste(date_formatted, time_formatted)
  start_time    <- lubridate::ymd_hms(date_time_str, tz = "UTC")  # or your local tz

  return(start_time)
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
#'
#' @examples
#' \dontrun{
#'   df <- read_birdnet_file("1STSMM2_20241105_050000.BirdNET.selection.table.txt")
#'   head(df)
#' }
read_birdnet_file <- function(file_path, tz = "UTC") {
  file_name <- basename(file_path)  # remove directory path
  start_time <- parse_birdnet_filename_datetime(file_name)

  # If you want to force a different timezone:
  # start_time <- lubridate::force_tz(start_time, tz)

  # read in the BirdNET selection table
  # Commonly, BirdNET exports tab-delimited text, so read_delim(..., delim="\t") or similar
  df <- readr::read_delim(file_path, delim = "\t", show_col_types = FALSE)

  # Ensure the "Begin Time (s)" column is named exactly or adapt if needed
  # Some BirdNET versions call it "Begin Time (s)", others might call it "Begin Time (s) "
  # or something else. Adjust if necessary.
  begin_col <- "Begin Time (s)"
  if (!begin_col %in% names(df)) {
    stop("Could not find 'Begin Time (s)' column in the BirdNET file.")
  }

  # add columns
  df <- df %>%
    dplyr::mutate(
      file_name             = file_name,
      start_time           = start_time,
      recording_window_time = start_time + .data[[begin_col]]
    )

  return(df)
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
#'
#' @examples
#' \dontrun{
#'   all_detections <- read_birdnet_folder("path/to/folder")
#'   dplyr::glimpse(all_detections)
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
  df_all <- purrr::map_dfr(files, ~ read_birdnet_file(.x))

  return(df_all)
}


