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

  # Use regexec/regmatches instead of stringr::str_match
  match_data <- regexec(pattern, file_name)
  match_result <- regmatches(file_name, match_data)

  if (length(match_result[[1]]) == 0) {
    stop("Filename does not match the expected pattern YYYYMMDD_HHMMSS.")
  }

  # The first element is the full match, second is the capturing group
  # However, since our pattern is just the capturing group, full match == group.
  # But regexec returns full match at index 1 and groups at 2+.
  # Let's just use the full match if it found it.
  match <- match_result[[1]][1]

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
#' @param file_path Path to a BirdNET file (either tab-delimited Raven selection table or CSV).
#' @param tz Timezone to be used for the start time. Default "UTC".
#'
#' @return A tibble with all columns from the BirdNET file plus:
#'   * `file_name` for reference
#'   * `start_time` the parsed date-time from the filename
#'   * `recording_window_time` the absolute time of each detection
#' @details
#' The function attempts to read the file based on its extension.
#' * Ends in `.csv` or `.CSV`: reads as comma-separated.
#' * Otherwise: reads as tab-separated (Raven selection table default).
#'
#' It also standardizes the start time column:
#' * If `Begin Time (s)` exists (Raven), it is used.
#' * If `Start (s)` exists (CSV), it is renamed to `Begin Time (s)` internally (or standardizing to `begin_time_s`).
#'
#' Filenames must ideally follow the pattern `YYYYMMDD_HHMMSS` to allow automatic `start_time` parsing.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a Raven selection table
#' df_raven <- read_birdnet_file("data/SiteA_20240101_120000.BirdNET.selection.table.txt")
#'
#' # Read a CSV
#' df_csv <- read_birdnet_file("data/SiteA_20240101_120000.BirdNET.results.csv")
#' }
read_birdnet_file <- function(file_path, tz = "UTC") {
  file_name <- basename(file_path)
  start_time <- tryCatch(
    {
      parse_birdnet_filename_datetime(file_name)
    },
    error = function(e) {
      warning(paste(
        "Could not parse datetime from filename:", file_name,
        "- start_time will be NA. Ensure filename matches 'YYYYMMDD_HHMMSS' pattern."
      ))
      return(as.POSIXct(NA, tz = tz))
    }
  )

  # Check file extension to detect format
  if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    df <- readr::read_csv(file_path, show_col_types = FALSE)
  } else {
    # Default to tab-delimited (Raven)
    df <- readr::read_delim(file_path, delim = "\t", show_col_types = FALSE)
  }

  # Standardize "Begin Time (s)" column
  # Raven uses "Begin Time (s)", BirdNET CSV often uses "Start (s)"
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

  # Standardize "End Time (s)" column
  if ("End Time (s)" %in% names(df)) {
    df <- df |> dplyr::rename(end_time_s = `End Time (s)`)
  } else if ("End (s)" %in% names(df)) {
    df <- df |> dplyr::rename(end_time_s = `End (s)`)
  }

  # Standardize "Common Name"
  if ("Common name" %in% names(df)) {
    df <- df |> dplyr::rename(`Common Name` = `Common name`)
  }

  # Standardize "Confirmation" or "Confidence"
  # Raven: "Confidence", CSV: "Confidence" usually, but sometimes could be different?
  # Let's assume Confidence is standard, if not we can add logic here.
  # Ensure column names are clean? For now we just focus on the time columns required for processing.

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
#' @param folder A folder path containing BirdNET files.
#' @param pattern A regex pattern to match the files.
#'        Default matches `.txt` or `.csv` files that look like BirdNET outputs.
#'        e.g. "BirdNET.*\\.(txt|csv)$"
#' @param recursive Whether to search recursively in subfolders. Default FALSE.
#'
#' @return A single tibble combining all data.
#' @export
#'
#' @examples
#' \dontrun{
#' all_detections <- read_birdnet_folder("path/to/folder")
#' }
read_birdnet_folder <- function(folder = ".",
                                pattern = "BirdNET.*\\.(txt|csv)$",
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
  # Replace purrr::map_dfr with lapply + dplyr::bind_rows
  data_list <- lapply(files, read_birdnet_file)
  dplyr::bind_rows(data_list)
}

#' Read BirdNET selection files from multiple sites (folders)
#'
#' @param folder_paths A character vector of folder paths, one for each site.
#' @param pattern A regex pattern to match the files. Default "BirdNET.*\\.(txt|csv)$".
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
                               pattern = "BirdNET.*\\.(txt|csv)$",
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
  # Replace purrr::map_dfr with lapply + dplyr::bind_rows
  data_list <- lapply(folder_paths, read_one_site)
  dplyr::bind_rows(data_list)
}
