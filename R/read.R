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
  # 1) Try Format 1: YYYYMMDD_HHMMSS
  pattern1 <- "(\\d{8}_\\d{6})"
  match_data1 <- regexec(pattern1, file_name)
  match_result1 <- regmatches(file_name, match_data1)

  if (length(match_result1[[1]]) > 0) {
    match <- match_result1[[1]][1]
    date_time_parts <- strsplit(match, "_")[[1]]
    date_part <- date_time_parts[1]
    time_part <- date_time_parts[2]

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

    return(lubridate::ymd_hms(paste(date_formatted, time_formatted), tz = "UTC"))
  }

  # 2) Try Format 2: ISO-like YYYYMMDDTHHMMSS (e.g. 20190417T035757)
  pattern2 <- "(\\d{8}T\\d{6})"
  match_data2 <- regexec(pattern2, file_name)
  match_result2 <- regmatches(file_name, match_data2)

  if (length(match_result2[[1]]) > 0) {
    match <- match_result2[[1]][1]
    # lubridate::ymd_hms can often parse T format directly
    return(lubridate::ymd_hms(match, tz = "UTC"))
  }

  stop("Filename does not match known pattern (YYYYMMDD_HHMMSS or YYYYMMDDTHHMMSS).")
}

#' Convert time values to numeric seconds
#'
#' Internal helper to handle various time formats from BirdNET outputs.
#' Converts POSIXct, hms, difftime, or character time values to numeric seconds.
#'
#' @param x A vector of time values (numeric, POSIXct, hms, difftime, or character)
#' @return A numeric vector of seconds
#' @noRd
ensure_numeric_seconds <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    # POSIXct datetime: calculate seconds from first timestamp
    # This happens when BirdNET GUI outputs full datetime strings
    first_time <- min(x, na.rm = TRUE)
    return(as.numeric(difftime(x, first_time, units = "secs")))
  }

  if (inherits(x, "difftime") || inherits(x, "hms")) {
    # hms/difftime: convert directly to numeric seconds
    return(as.numeric(x, units = "secs"))
  }

  if (is.character(x)) {
    # Try parsing as HMS time string
    parsed <- tryCatch(
      {
        hms::parse_hms(x)
      },
      error = function(e) NULL
    )
    if (!is.null(parsed)) {
      return(as.numeric(parsed, units = "secs"))
    }

    # Try parsing as numeric string
    as_num <- suppressWarnings(as.numeric(x))
    if (!all(is.na(as_num))) {
      return(as_num)
    }
  }

  # Fallback: try coercing to numeric
  warning("Could not convert time column to numeric seconds. Attempting direct coercion.")
  as.numeric(x)
}

#' Coerce a vector to numeric, handling characters and suppressing warnings
#'
#' @param x A vector to convert
#' @return A numeric vector
#' @noRd
coerce_to_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  # If it's character, we try to convert.
  # We suppress warnings because NAs are expected for non-numeric strings
  suppressWarnings(as.numeric(as.character(x)))
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

  # Ensure time columns are numeric (handles POSIXct, hms, difftime from GUI)

  df <- df |>
    dplyr::mutate(
      begin_time_s = ensure_numeric_seconds(begin_time_s)
    )
  if ("end_time_s" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        end_time_s = ensure_numeric_seconds(end_time_s)
      )
  }

  # Standardize "Common Name"
  if ("Common name" %in% names(df)) {
    df <- df |> dplyr::rename(`Common Name` = `Common name`)
  }

  # Standardize "Confirmation" or "Confidence"
  # Raven: "Confidence", CSV: "Confidence" usually
  # FORCE numeric to avoid bind_rows type mismatch if some files have 'NA' as strings or are empty
  if ("Confidence" %in% names(df)) {
    df <- df |> dplyr::mutate(Confidence = coerce_to_numeric(Confidence))
  }

  # Also ensure other likely numeric columns are numeric to prevent downstream bind_rows issues
  numeric_cols <- c("Selection", "Low Freq (Hz)", "High Freq (Hz)")
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- coerce_to_numeric(df[[col]])
    }
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

  # Exclude known non-detection files (like BirdNET_analysis_params.csv)
  # Filter out files containing "analysis_params" or "config" (case insensitive)
  files <- files[!grepl("analysis_params|config", basename(files), ignore.case = TRUE)]

  if (length(files) == 0) {
    warning("No valid detection files found (after filtering out config/params files).")
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
