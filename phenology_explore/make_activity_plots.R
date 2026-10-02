# Daily (diel) activity heatmaps, one PNG per site.
# All the plotting logic lives in plot_daily_activity(); this script is just the
# inputs + a loop, so adding a site = adding a name to `sites`.

suppressMessages(devtools::load_all("."))
library(dplyr)

outdir <- "phenology_explore"; dir.create(outdir, showWarnings = FALSE)

# --- Inputs -----------------------------------------------------------------
detections <- "/Users/z3484779/Documents/birdnet_play/detections"
sites      <- c("POWERLINES")        # add more site names here
conf       <- 0.5                     # confidence threshold
lat        <- -33.4; lon <- 151.4     # for sunrise/sunset shading
tz         <- "Australia/Sydney"
sel_date   <- NULL                    # NULL = average all days; or "2026-02-02"

insects <- c(
  "Mimicking Snout-nose", "Lesson's Mimicking Snout-nose", "Razor Grinder",
  "Black Prince", "Bronze Tree-Buzzer", "Paperbark Cicada",
  "Black Field Cricket", "Southern Mole Cricket", "Dark Night Mole Cricket",
  "Whittish Meadow Katydid", "Greenish Meadow Katydid"
)
birds <- c("Australian Owlet Nightjar",
           "Laughing Kookaburra", "Red-browed Finch", "White-cheeked Honeyeater",
           "Grey Fantail", "Lewin's Honeyeater", "Brown Gerygone",
           "Masked Lapwing", "White-throated Treecreeper", "Green Catbird",
           "White-browed Scrubwren", "Eastern Yellow Robin", "Brown Thornbill",
           "Silvereye", "Eastern Whipbird", "Yellow Thornbill")
target <- c(insects, birds)

# --- Read once, tag site + bird/insect group -------------------------------
d <- read_birdnet_folder(detections) |>
  mutate(Site  = sub("_[0-9]{8}_[0-9]{6}.*$", "", file_name),
         group = ifelse(`Common Name` %in% birds, "Birds", "Insects"))

# --- One plot per site ------------------------------------------------------
for (s in sites) {
  p <- plot_daily_activity(d, site = s, species = target, confidence = conf,
                           date = sel_date, group_col = "group",
                           lat = lat, lon = lon, tz = tz)
  nlev <- length(unique(p$data$`Common Name`))
  fname <- paste0("activity_", s, ".png")
  ggplot2::ggsave(file.path(outdir, fname), p,
                  width = 11, height = 1.6 + 0.3 * nlev, dpi = 150,
                  device = ragg::agg_png)
  cat("wrote", fname, "\n")
}
