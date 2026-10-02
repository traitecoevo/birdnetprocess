# Sunrise and sunset on each LOCAL calendar date, as clock time in `tz` labelled
# UTC -- the same footing as recorder window times, so the two subtract cleanly.
#
# Which day suncalc::getSunlightTimes() assigns an event to changed between
# versions: 0.5.1 returns the event of the local solar day, 0.5.3 the event on
# the UTC calendar day, which east of Greenwich is the NEXT local sunrise. So
# ask for the neighbouring days too and keep the event whose local date matches.
# Vectorised over rows: `lat`/`lon` may be per-date (e.g. one row per site-day).
local_sun_times <- function(date, lat, lon, tz) {
  date <- as.Date(date)
  n <- length(date)
  if (n == 0) {
    empty <- as.POSIXct(character(0), tz = "UTC")
    return(data.frame(sunrise = empty, sunset = empty))
  }
  lat <- rep_len(lat, n)
  lon <- rep_len(lon, n)
  q <- data.frame(date = c(date - 1, date, date + 1),
                  lat = rep(lat, 3), lon = rep(lon, 3))
  st <- suncalc::getSunlightTimes(data = q, keep = c("sunrise", "sunset"))
  row <- rep(seq_len(n), 3)
  want <- rep(date, 3)
  pick <- function(x) {
    x <- lubridate::force_tz(lubridate::with_tz(x, tz), "UTC")
    ok <- which(!is.na(x) & as.Date(x) == want)
    x[ok[match(seq_len(n), row[ok])]]
  }
  data.frame(sunrise = pick(st$sunrise), sunset = pick(st$sunset))
}
