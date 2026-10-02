# Species-by-confidence diagnostic: distribution of detection confidence per
# target taxon, so we can judge which are reliable before reading their timing.

suppressMessages(devtools::load_all("."))
library(dplyr); library(ggplot2)

outdir <- "phenology_explore"; dir.create(outdir, showWarnings = FALSE)

site <- "POWERLINES"   # which site to plot

insects <- c(
  "Mimicking Snout-nose", "Lesson's Mimicking Snout-nose", "Razor Grinder",
  "Black Prince", "Bronze Tree-Buzzer", "Paperbark Cicada",
  "Black Field Cricket", "Southern Mole Cricket", "Dark Night Mole Cricket",
  "Whittish Meadow Katydid", "Greenish Meadow Katydid"
)
birds <- c("Australian Owlet Nightjar",
           "Laughing Kookaburra", "Red-browed Finch", "White-cheeked Honeyeater",
           "Grey Fantail", "Lewin's Honeyeater", "Brown Gerygone",
           "Masked Lapwing", "White-throated Treecreeper", "Green Catbird")
target <- c(insects, birds)

d <- read_birdnet_folder("/Users/z3484779/Documents/birdnet_play/detections") |>
  mutate(Site = sub("_[0-9]{8}_[0-9]{6}.*$", "", file_name)) |>
  filter(Site == site, `Common Name` %in% target, Confidence > 0) |>
  mutate(group = ifelse(`Common Name` %in% birds, "Birds", "Insects"))

# Order species by median confidence; annotate n
stat <- d |> group_by(`Common Name`) |>
  summarise(med = median(Confidence), n = n(), .groups = "drop") |>
  arrange(med)
d$`Common Name` <- factor(d$`Common Name`, levels = stat$`Common Name`)

p <- ggplot(d, aes(Confidence, `Common Name`, fill = group)) +
  geom_vline(xintercept = c(0.25, 0.5), linetype = "dashed",
             colour = "grey60", linewidth = 0.3) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.25,
               linewidth = 0.3, width = 0.65) +
  geom_text(data = stat, aes(x = 1.02, y = `Common Name`, label = paste0("n=", n)),
            inherit.aes = FALSE, hjust = 0, size = 3, colour = "grey40") +
  scale_fill_manual(values = c(Insects = "#e9a33b", Birds = "#3b82c4"), name = NULL) +
  scale_x_continuous(limits = c(0, 1.12), breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0.01, 0))) +
  labs(title = paste0("Detection confidence by species — ", site),
       subtitle = "Ordered by median confidence; dashed = 0.25 / 0.5 thresholds",
       x = "BirdNET confidence", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "top")

fname <- paste0("00_confidence_", site, ".png")
ggsave(file.path(outdir, fname), p, width = 9, height = 6, dpi = 150,
       device = ragg::agg_png)
cat("wrote", fname, "\n")
