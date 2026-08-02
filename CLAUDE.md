# birdnetprocess — Project Notes for Claude

R package for processing and visualising **BirdNET detection results** from field
deployments. Remote: `traitecoevo/birdnetprocess`.

## Where this repo sits — read before building anything new

**This repo owns:** the *downstream* end — reading detection output, range-filtering,
summarising, and plotting it (confidence, counts, daily activity, stream, timeline,
trends, site reports).
**It does NOT own:** training, curation, evaluation, or embeddings. Note the distinction
from `birdnetEmbed`, which also plots but plots **embedding space**, not detections —
if you're plotting a `.npz`, you're in the wrong repo.

Full ownership table, seams, venvs, shared data: **`~/Documents/ecoacoustics/ECOACOUSTICS.md`**.

## Gotchas

- **`nsw_abundance_stack_3km.tif` (152 MB) is untracked and duplicated.** A second,
  *different* copy (different checksum, later date) sits in `~/Documents/desert_detect/`,
  read by `match_pelican.R`. Neither repo generates it — it comes from `ebird_rel_abund`.
  Consumed here by `R/filter_range.R` and `R/site_report.R`. Until this is resolved, don't
  assume the range filter agrees with anything computed in `desert_detect`.
