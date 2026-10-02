# Three-panel daily-activity figure for the Smiths Lake sites, each over its own
# continuous midnight-to-midnight day. Run phenology_explore/run_detector.py
# first to produce the detection tables this reads.

suppressMessages(devtools::load_all("."))
library(dplyr)

outdir <- "phenology_explore"; dir.create(outdir, showWarnings = FALSE)

# --- Inputs -----------------------------------------------------------------
det_base <- "/Users/z3484779/Documents/birdnet_play/smiths_lake"
labels_file <- "/Users/z3484779/Library/CloudStorage/OneDrive-UNSW/call_library/recognizers/pelican0-15_Labels.txt"
# site folder -> its target 24h window (must match run_detector.py)
# Only the two sites recorded on the same day, for a like-for-like comparison.
targets <- c("Powerline Strip" = "2026-02-03",
             "Mowed Field"     = "2026-02-03")
conf           <- 0.5    # confidence threshold for inclusion
min_detections <- 5      # drop species with fewer than this many detections
lat <- -32.38; lon <- 152.51          # Smiths Lake, NSW (sunrise/sunset)
tz  <- "Australia/Sydney"

# species (Common Names) to drop from the figure
exclude <- c("Rain", "Stream", "Australasian Grass-Owl",
             "Black Squeaker", "Fence Buzzer",
             "Little Shrikethrush", "Ground Parrot")

# --- Bird vs insect from the classifier's own scientific names ---------------
# Labels are "Genus species_Common Name". Insects = cicadas (Cicadidae) +
# crickets/katydids (Orthoptera) + flies; frogs/mammals/noise = Other;
# everything else animal = Birds.
insect_genera <- c(
  # cicadas
  "Aleeta","Arunta","Atrapsalta","Birrima","Burbunga","Clinopsalta","Cyclochila",
  "Cystosoma","Diemeniana","Froggattina","Galanga","Haemopsalta","Henicopsaltria",
  "Myopsalta","Palapsalta","Pauropsalta","Popplepsalta","Psaltoda","Tamasa",
  "Telmapsalta","Thopha","Yoyetta","Xeropsalta",
  # crickets & katydids + flies
  "Austrosalomona","Conocephalus","Elephantodeta","Eurepa","Gryllodes",
  "Gryllotalpa","Lepidogryllus","Oecanthus","Pseudorhynchus","Teleogryllus","Diptera")
frog_genera <- c(
  "Adelotus","Chlorohyla","Colleeneremia","Crinia","Cyclorana","Drymomantis",
  "Limnodynastes","Litoria","Neobatrachus","Notaden","Paracrinia","Pelodryas",
  "Pengilleyia","Platyplectrum","Pseudophryne","Rawlinsonia","Rhyaconastes","Uperoleia")
other_genera <- c("Bos","Canis","Capra","Felis","Homo","Ovis","Petaurus",
                  "Phascolarctos","Pseudocheirus","Pteropus","Trichosurus","Vulpes",
                  "Environment")

labs <- readLines(labels_file)
lab_map <- tibble(
  sci    = sub("_.*$", "", labs),
  common = sub("^[^_]*_", "", labs)) |>
  mutate(genus = sub(" .*$", "", sci),
         group = case_when(
           genus %in% insect_genera ~ "Insects",
           genus %in% frog_genera   ~ "Frogs",
           genus %in% other_genera  ~ "Other",
           TRUE                     ~ "Birds")) |>
  select(common, group)

group_colours <- c(Birds = "#3b82c4", Insects = "#e9a33b",
                   Frogs = "#5ba053", Other = "grey60")

# --- Read all three sites; Site = folder name; tag group --------------------
d <- read_birdnet_sites(file.path(det_base, names(targets))) |>
  left_join(lab_map, by = c(`Common Name` = "common")) |>
  mutate(group = tidyr::replace_na(group, "Birds"),
         # lump the mole cricket species (Gryllotalpa spp.) into one row
         `Common Name` = ifelse(grepl("Mole Cricket$", `Common Name`),
                                "Mole Cricket sp.", `Common Name`))

# --- Keep each site only on its target day; label panels with the window -----
d <- d |>
  mutate(day = as.Date(recording_window_time)) |>
  filter(day == as.Date(targets[Site])) |>
  mutate(Site = paste0(Site, "\n", format(as.Date(targets[Site]), "%d %b %Y"))) |>
  select(-day)

# --- One faceted figure (panels share the midnight-midnight clock) ----------
p <- plot_daily_activity(
  d, confidence = conf, min_detections = min_detections,
  exclude = exclude, group_col = "group", group_colours = group_colours,
  lat = lat, lon = lon, tz = tz, layout = "columns",
  title = "Diel calling patterns — Smiths Lake, 3 Feb 2026",
  subtitle = paste("Midnight-midnight; each row scaled to its own peak;",
                   "grey = night; labels: birds (blue) / insects (orange)"))

nlev <- length(unique(p$data$`Common Name`))
ggplot2::ggsave(file.path(outdir, "smiths_lake_activity.png"), p,
                width = 4 + 3.2 * length(targets), height = 2 + 0.22 * nlev,
                dpi = 150, device = ragg::agg_png)
cat("species shown:", nlev, "| wrote smiths_lake_activity.png\n")
