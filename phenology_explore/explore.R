# Diel calling-pattern heatmap: all days folded onto a single 24h clock
# (time-of-day x-axis), one panel per site. Focus = daily calling patterns of
# the insect chorus + a few contrasting birds.

suppressMessages(devtools::load_all("."))
library(dplyr); library(ggplot2)

outdir <- "phenology_explore"; dir.create(outdir, showWarnings = FALSE)

# --- Inputs ---
insects <- c(
  "Mimicking Snout-nose", "Lesson's Mimicking Snout-nose", "Razor Grinder",
  "Black Prince", "Bronze Tree-Buzzer", "Paperbark Cicada",
  "Black Field Cricket", "Southern Mole Cricket", "Dark Night Mole Cricket",
  "Whittish Meadow Katydid", "Greenish Meadow Katydid"
)
birds <- c("Australian Owlet Nightjar",  # nocturnal
           "Laughing Kookaburra", "Red-browed Finch", "White-cheeked Honeyeater",
           "Grey Fantail", "Lewin's Honeyeater", "Brown Gerygone",
           "Masked Lapwing", "White-throated Treecreeper", "Green Catbird",
           "White-browed Scrubwren", "Eastern Yellow Robin", "Brown Thornbill",
           "Silvereye", "Eastern Whipbird", "Yellow Thornbill")
target <- c(insects, birds)

lat <- -33.4; lon <- 151.4          # placeholder coords for day/night
local_tz <- "Australia/Sydney"
conf <- 0.5                          # below 0.5 is mostly false positives
bin_min <- 20                        # time-of-day bin width (minutes)
ref <- as.POSIXct("2000-01-01", tz = "UTC")   # arbitrary date to carry time-of-day

# --- Read both sites; Site = filename prefix before the _YYYYMMDD_ stamp ---
sites_to_plot <- "POWERLINES"   # NULL = all sites

d <- read_birdnet_folder("/Users/z3484779/Documents/birdnet_play/detections") |>
  filter(Confidence >= conf, `Common Name` %in% target,
         !is.na(recording_window_time)) |>
  mutate(
    Site = sub("_[0-9]{8}_[0-9]{6}.*$", "", file_name),
    group = ifelse(`Common Name` %in% birds, "Birds", "Insects"),
    `Common Name` = ifelse(
      `Common Name` %in% c("Whittish Meadow Katydid", "Greenish Meadow Katydid"),
      "Meadow Katydids", `Common Name`)
  )
if (!is.null(sites_to_plot)) d <- dplyr::filter(d, Site %in% sites_to_plot)

# --- Fold to time-of-day: seconds since midnight (local clock = naive UTC) ---
lt <- as.POSIXlt(d$recording_window_time, tz = "UTC")
d$tod_sec <- lt$hour * 3600 + lt$min * 60 + lt$sec
d$tod <- ref + (floor(d$tod_sec / (bin_min * 60)) * bin_min * 60)

# --- Aggregate per site x species x time-of-day bin, scale each row to its peak ---
pd <- d |>
  count(Site, `Common Name`, tod, name = "n") |>
  group_by(Site, `Common Name`) |>
  mutate(rel = n / max(n)) |>
  ungroup()

# --- Representative sunrise/sunset as time-of-day (median over deployment) ---
dr <- sort(unique(as.Date(d$recording_window_time)))
st <- suncalc::getSunlightTimes(date = dr, lat = lat, lon = lon,
                                keep = c("sunrise", "sunset"))
tod_of <- function(x) { p <- as.POSIXlt(lubridate::with_tz(x, local_tz)); p$hour*3600 + p$min*60 }
sunrise_sec <- median(tod_of(st$sunrise), na.rm = TRUE)
sunset_sec  <- median(tod_of(st$sunset),  na.rm = TRUE)

# Order species into a nocturnal block then a diurnal block (by fraction of
# calls in daylight), and within each block by circular-mean call time so the
# midnight wrap doesn't scatter the night species.
sp_levels <- d |>
  mutate(is_day = tod_sec >= sunrise_sec & tod_sec <= sunset_sec) |>
  group_by(`Common Name`) |>
  summarise(day_frac = mean(is_day),
            circ = atan2(mean(sin(2 * pi * tod_sec / 86400)),
                         mean(cos(2 * pi * tod_sec / 86400))),
            .groups = "drop") |>
  arrange(day_frac, circ) |> pull(`Common Name`)
pd$`Common Name` <- factor(pd$`Common Name`, levels = sp_levels)

# Colour each y-axis name by group (birds vs insects), in axis-break order
grp_col <- c(Birds = "#3b82c4", Insects = "#e9a33b")
grp_lookup <- d |> dplyr::distinct(`Common Name`, group) |> tibble::deframe()
label_cols <- grp_col[grp_lookup[sp_levels]]

# Night = two bands on the 24h clock: [00:00, sunrise] and [sunset, 24:00]
night <- data.frame(
  xmin = c(ref,                 ref + sunset_sec),
  xmax = c(ref + sunrise_sec,   ref + 86400)
)
nlev <- nlevels(pd$`Common Name`)
icons <- data.frame(
  x = c(ref + sunrise_sec/2,                       # pre-dawn moon
        ref + mean(c(sunrise_sec, sunset_sec)),     # midday sun
        ref + mean(c(sunset_sec, 86400))),          # late-night moon
  y = nlev + 0.9,
  lab = c("\U0001F319", "☀️", "\U0001F319")
)

p <- ggplot(pd, aes(tod, `Common Name`)) +
  geom_rect(data = night, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey25", alpha = 0.18, inherit.aes = FALSE) +
  geom_tile(aes(fill = rel), height = 0.85) +
  geom_rect(data = night, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey10", alpha = 0.10, inherit.aes = FALSE) +
  geom_vline(xintercept = as.numeric(c(ref + sunrise_sec, ref + sunset_sec)),
             linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_text(data = icons, aes(x = x, y = y, label = lab), inherit.aes = FALSE, size = 6) +
  facet_grid(Site ~ ., switch = "y") +
  scale_fill_viridis_c(option = "magma", name = "relative\nactivity", begin = 0.05) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "3 hours",
                   limits = c(ref, ref + 86400), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.5))) +
  coord_cartesian(clip = "off") +
  labs(title = "Daily calling patterns — all days folded onto a 24h clock",
       subtitle = "Each row scaled to its own peak; grey band = night; names: birds (blue) / insects (orange)",
       x = "time of day", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = label_cols),
        strip.text.y.left = element_text(angle = 0, face = "bold"),
        strip.placement = "outside")

n_sites <- length(unique(pd$Site))
ggsave(file.path(outdir, "A_heatmap.png"), p, width = 11,
       height = 1.6 + 0.3 * nlev * n_sites, dpi = 150,
       device = ragg::agg_png)
cat("wrote A_heatmap.png — sites:", paste(unique(pd$Site), collapse = ", "), "\n")
