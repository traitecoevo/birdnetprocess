#' Fetch hourly historical weather from Open-Meteo
#'
#' @description
#' Downloads hourly weather for one location from the Open-Meteo historical
#' archive (ERA5 reanalysis and its higher-resolution land product). No account
#' or key is needed. Reanalysis is gridded at roughly 10-25 km, so it suits
#' wind, temperature and pressure better than patchy convective rain; prefer a
#' local gauge for rainfall where one exists.
#'
#' @param latitude,longitude Location in decimal degrees.
#' @param start,end First and last date (`Date` or "YYYY-MM-DD").
#' @param tz Time zone for the returned clock times. Use the zone the recorder
#'   clocks were set to, so weather lines up with recording times.
#' @param variables Open-Meteo hourly variable names.
#'
#' @return A tibble with `time` (clock time in `tz`, carried as UTC like the
#'   recording times elsewhere in this package) and one column per variable,
#'   plus attributes `grid_latitude` and `grid_longitude` for the grid cell
#'   actually used.
#' @export
#' @examples
#' \dontrun{
#' w <- fetch_openmeteo_weather(-33.5, 151.2, "2024-01-01", "2024-01-31",
#'                              tz = "Australia/Sydney")
#' }
fetch_openmeteo_weather <- function(latitude, longitude, start, end, tz,
                                    variables = c("temperature_2m", "precipitation",
                                                  "wind_speed_10m", "wind_gusts_10m",
                                                  "surface_pressure", "cloud_cover")) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("fetch_openmeteo_weather() needs the 'jsonlite' package.", call. = FALSE)
  }
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=", latitude, "&longitude=", longitude,
    "&start_date=", as.Date(start), "&end_date=", as.Date(end),
    "&hourly=", paste(variables, collapse = ","),
    "&timezone=", utils::URLencode(tz, reserved = TRUE),
    "&models=era5_seamless"
  )
  res <- jsonlite::fromJSON(url)
  if (!is.null(res$error) && isTRUE(res$error)) {
    stop("Open-Meteo: ", res$reason, call. = FALSE)
  }
  out <- tibble::as_tibble(res$hourly)
  out$time <- as.POSIXct(out$time, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  attr(out, "grid_latitude") <- res$latitude
  attr(out, "grid_longitude") <- res$longitude
  out
}

#' Summarise hourly weather over sun-relative recording sessions
#'
#' @description
#' For each date, takes the hours inside a window around sunrise ("dawn") and
#' around sunset ("dusk") and summarises the weather there: what conditions
#' were like while the recorder was listening. Adds moon illumination, and the
#' 24-hour change in pressure (a common cue for weather-driven behaviour).
#'
#' @param weather Hourly weather, e.g. from [fetch_openmeteo_weather()], with a
#'   `time` column of clock times carried as UTC.
#' @param latitude,longitude Location used for sun and moon times.
#' @param tz Time zone of the clock times in `weather`.
#' @param before_min,after_min Window either side of sunrise and sunset, in
#'   minutes. Default 60 each.
#'
#' @return A tibble with one row per date and session (`"dawn"`, `"dusk"`):
#'   the mean of each numeric weather column over the window (sums for
#'   `precipitation`, maxima for `wind_gusts_10m`), plus `rain_day` (that day's
#'   total precipitation, when present), `pressure_change_24h` (when
#'   `surface_pressure` is present) and `moon_illumination` (0-1).
#' @export
weather_by_session <- function(weather, latitude, longitude, tz,
                               before_min = 60, after_min = 60) {
  if (!requireNamespace("suncalc", quietly = TRUE)) {
    stop("weather_by_session() needs the 'suncalc' package.", call. = FALSE)
  }
  w <- weather
  w$date <- as.Date(w$time)
  if ("surface_pressure" %in% names(w)) {
    w <- w[order(w$time), ]
    w$pressure_change_24h <- w$surface_pressure - dplyr::lag(w$surface_pressure, 24)
  }
  dates <- sort(unique(w$date))
  st <- local_sun_times(dates, latitude, longitude, tz)
  sun <- dplyr::tibble(date = dates, sunrise = st$sunrise, sunset = st$sunset)

  value_cols <- setdiff(names(w)[vapply(w, is.numeric, logical(1))], "date")
  summarise_window <- function(event, session) {
    w |>
      dplyr::left_join(sun, by = "date") |>
      dplyr::filter(
        # an hourly value describes the hour starting at `time`
        .data$time + 3600 > .data[[event]] - before_min * 60,
        .data$time < .data[[event]] + after_min * 60
      ) |>
      dplyr::group_by(.data$date) |>
      dplyr::summarise(dplyr::across(
        dplyr::all_of(value_cols),
        ~ if (dplyr::cur_column() == "precipitation") sum(.x, na.rm = TRUE)
          else if (dplyr::cur_column() == "wind_gusts_10m") max(.x, na.rm = TRUE)
          else mean(.x, na.rm = TRUE)
      ), .groups = "drop") |>
      dplyr::mutate(session = session)
  }
  out <- dplyr::bind_rows(summarise_window("sunrise", "dawn"),
                          summarise_window("sunset", "dusk"))
  if ("precipitation" %in% names(w)) {
    daily <- w |> dplyr::group_by(.data$date) |>
      dplyr::summarise(rain_day = sum(.data$precipitation, na.rm = TRUE), .groups = "drop")
    out <- dplyr::left_join(out, daily, by = "date")
  }
  moon <- suncalc::getMoonIllumination(date = dates, keep = "fraction")
  out <- dplyr::left_join(out, dplyr::tibble(date = dates, moon_illumination = moon$fraction),
                          by = "date")
  out |>
    dplyr::mutate(session = factor(.data$session, levels = c("dawn", "dusk"))) |>
    dplyr::relocate("date", "session") |>
    dplyr::arrange(.data$date, .data$session)
}

#' Read Bureau of Meteorology "Daily Weather Observations" files
#'
#' @description
#' Parses the monthly CSVs the Australian Bureau of Meteorology publishes for
#' each automatic weather station (product `IDCJDW****`, downloadable for the
#' last ~13 months from `bom.gov.au/climate/dwo/`). Station measurements, as
#' opposed to the gridded reanalysis from [fetch_openmeteo_weather()].
#'
#' @details
#' Rainfall in these files is the 24 hours **to 9 am** on the stated date, so
#' most of it fell on the previous day. `rain_prev_day_to_9am` keeps BOM's
#' value; `rain_day` re-attributes it to the calendar day it mostly fell on
#' (the next row's value).
#'
#' @param paths One or more monthly CSV files.
#' @return A tibble, one row per date, with snake-case columns: `date`,
#'   `tmin`, `tmax`, `rain_prev_day_to_9am`, `rain_day`, `gust_max`,
#'   `gust_dir`, and `t_9am`, `rh_9am`, `wind_9am`, `wind_dir_9am`,
#'   `pressure_9am` with the same for `3pm`. "Calm" winds are 0.
#' @export
read_bom_dwo <- function(paths) {
  read_one <- function(path) {
    lines <- readLines(path, encoding = "latin1", warn = FALSE)
    header <- grep('^,"Date"', lines)
    if (!length(header)) stop("No data header in ", basename(path), call. = FALSE)
    utils::read.csv(text = lines[header:length(lines)], header = TRUE,
                    check.names = FALSE, stringsAsFactors = FALSE,
                    na.strings = c("", " "))[, -1]
  }
  raw <- do.call(rbind, lapply(paths, read_one))
  num <- function(x) {
    x <- trimws(as.character(x))
    suppressWarnings(as.numeric(ifelse(x == "Calm", "0", x)))
  }
  col <- function(pattern) raw[[grep(pattern, names(raw), fixed = TRUE)[1]]]
  out <- tibble::tibble(
    date = as.Date(col("Date"), format = "%Y-%m-%d"),
    tmin = num(col("Minimum temperature")),
    tmax = num(col("Maximum temperature")),
    rain_prev_day_to_9am = num(col("Rainfall")),
    gust_max = num(col("Speed of maximum wind gust")),
    gust_dir = trimws(col("Direction of maximum wind gust")),
    t_9am = num(col("9am Temperature")),
    rh_9am = num(col("9am relative humidity")),
    wind_9am = num(col("9am wind speed")),
    wind_dir_9am = trimws(col("9am wind direction")),
    pressure_9am = num(col("9am MSL pressure")),
    t_3pm = num(col("3pm Temperature")),
    rh_3pm = num(col("3pm relative humidity")),
    wind_3pm = num(col("3pm wind speed")),
    wind_dir_3pm = trimws(col("3pm wind direction")),
    pressure_3pm = num(col("3pm MSL pressure"))
  )
  out <- out[order(out$date), ]
  out$rain_day <- dplyr::lead(out$rain_prev_day_to_9am)
  out
}

#' Weather anomalies relative to the seasonal norm for each session
#'
#' @description
#' For each session (e.g. dawn and dusk separately), fits a smooth seasonal
#' trend to a weather variable across dates and returns each value's departure
#' from it, in the variable's own units. "2 degrees warmer than a usual dawn at
#' this time of year", rather than a raw temperature that mostly measures the
#' time of day and the season.
#'
#' @param weather Output of [weather_by_session()] (or any table with `date`,
#'   `session` and the variable).
#' @param var Column to convert, e.g. `"temperature_2m"`.
#' @param df Degrees of freedom of the natural-spline seasonal trend. Default 3.
#'   Use 0 for a plain departure from the session mean.
#' @param date_range Optional two dates: fit the trend over this period only.
#'
#' @return `weather` with an added column `<var>_anomaly`.
#' @export
session_anomaly <- function(weather, var, df = 3, date_range = NULL) {
  w <- weather
  if (!is.null(date_range)) {
    date_range <- as.Date(date_range)
    w <- w[w$date >= date_range[1] & w$date <= date_range[2], ]
  }
  out_col <- paste0(var, "_anomaly")
  w[[out_col]] <- NA_real_
  for (s in unique(w$session)) {
    i <- which(w$session == s & !is.na(w[[var]]))
    x <- as.numeric(w$date[i])
    y <- w[[var]][i]
    trend <- if (df > 0) stats::fitted(stats::lm(y ~ splines::ns(x, df))) else mean(y)
    w[[out_col]][i] <- y - trend
  }
  w
}
