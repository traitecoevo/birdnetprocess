birdnetprocess
================
Will Cornwell
2026-02-11

# birdnetprocess

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/birdnetprocess/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/traitecoevo/birdnetprocess/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/traitecoevo/birdnetprocess/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/traitecoevo/birdnetprocess/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

`birdnetprocess` helps you process and visualize BirdNET detection
results which can be overwhelming in their volume.

## Install

``` r
# install.packages("devtools") # if needed
devtools::install_github("traitecoevo/birdnetprocess")
#> Downloading GitHub repo traitecoevo/birdnetprocess@HEAD
#> viridisLite (0.4.2 -> 0.4.3) [CRAN]
#> dplyr       (1.1.4 -> 1.2.0) [CRAN]
#> timechange  (0.3.0 -> 0.4.0) [CRAN]
#> ggplot2     (4.0.1 -> 4.0.2) [CRAN]
#> lubridate   (1.9.4 -> 1.9.5) [CRAN]
#> Installing 5 packages: viridisLite, dplyr, timechange, ggplot2, lubridate
#> 
#> The downloaded binary packages are in
#>  /var/folders/1k/cskklf914vd5m3stdrxqyx300000gp/T//Rtmp04VOrL/downloaded_packages
#> Adding 'dplyr_1.2.0.tgz' to the cache
#> Adding 'ggplot2_4.0.2.tgz' to the cache
#> Adding 'lubridate_1.9.5.tgz' to the cache
#> Adding 'timechange_0.4.0.tgz' to the cache
#> Adding 'viridisLite_0.4.3.tgz' to the cache
#> ── R CMD build ────────────────────────────────────────────────────
#>      checking for file ‘/private/var/folders/1k/cskklf914vd5m3stdrxqyx300000gp/T/Rtmp04VOrL/remotes694cf2caa74/traitecoevo-birdnetprocess-2d5e3f4/DESCRIPTION’ ...  ✔  checking for file ‘/private/var/folders/1k/cskklf914vd5m3stdrxqyx300000gp/T/Rtmp04VOrL/remotes694cf2caa74/traitecoevo-birdnetprocess-2d5e3f4/DESCRIPTION’
#>   ─  preparing ‘birdnetprocess’:
#>      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘birdnetprocess_0.0.0.9000.tar.gz’
#>      
#> 
#> Adding 'birdnetprocess_0.0.0.9000.tgz' to the cache
```

## Example Usage

#### 1. Extract Start DateTime from a BirdNET Filename

The function `parse_birdnet_filename_datetime()` assumes filenames
follow this pattern:
`SOMETHING_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt` or similar.

Note that this assumes that you’ve set the time right on your recording
device and that the filename reflects the true time for the start of
that time segment.

The package now supports both common birdnet output types:

- **Raven selection tables** (`.txt`, tab-delimited)

- **BirdNET Analyzer CSV output** (`.csv`, comma-separated)

#### 2. Read a Single BirdNET File

Use `read_birdnet_file()` to read one BirdNET selection table.

This will: \* Detect if it’s a Raven table (tab-separated) or CSV. \*
Parse the filename for the start time. \* Standardize column names
(e.g., `Begin Time (s)`). \* Add `start_time` and
`recording_window_time` columns.

``` r
library(birdnetprocess)
library(dplyr)

# Use example data included in the package
raven_path <- system.file("extdata", "SiteA_20240101_120000.BirdNET.selection.table.txt", package = "birdnetprocess")
csv_path <- system.file("extdata", "SiteA_20240101_120000.BirdNET.results.csv", package = "birdnetprocess")

# The example files in extdata now have valid timestamps in their filenames:
# SiteA_20240101_120000.BirdNET.selection.table.txt

df_raven <- read_birdnet_file(raven_path)
df_csv <- read_birdnet_file(csv_path)

head(df_raven)
#> # A tibble: 2 × 13
#>   Selection View          Channel begin_time_s end_time_s `Low Freq (Hz)`
#>       <dbl> <chr>           <dbl>        <dbl>      <dbl>           <dbl>
#> 1         1 Spectrogram 1       1          1.5        4.5             150
#> 2         2 Spectrogram 1       1          5          8               150
#> # ℹ 7 more variables: `High Freq (Hz)` <dbl>, `Common Name` <chr>,
#> #   `Species Code` <chr>, Confidence <dbl>, file_name <chr>, start_time <dttm>,
#> #   recording_window_time <dttm>
head(df_csv)
#> # A tibble: 2 × 9
#>   begin_time_s end_time_s `Scientific name`  `Common Name`  Confidence
#>          <dbl>      <dbl> <chr>              <chr>               <dbl>
#> 1          1.5        4.5 Turdus migratorius American Robin       0.95
#> 2          5          8   Melospiza melodia  Song Sparrow         0.9 
#> # ℹ 4 more variables: `Species Code` <chr>, file_name <chr>, start_time <dttm>,
#> #   recording_window_time <dttm>
```

**Data Requirements:** 1. **Filename Format**: For automatic time
processing, filenames **MUST** contain a timestamp in the format
`YYYYMMDD_HHMMSS` (e.g., `MySite_20240320_060000.BirdNET.txt`). 2.
**File Format**: \* **Raven Selection Table**: Tab-delimited `.txt`.
Must have `Begin Time (s)`. \* **CSV**: Comma-delimited `.csv`. Must
have `Start (s)` or `Begin Time (s)`.

#### 3. Quick Visualization and Statistics

`plot_species_counts` and `summarise_detections` provide immediate
insights into the detections in your data.

First, let’s read the data from a folder of results (e.g.,
`detections_SL21`):

``` r
library(birdnetprocess)
library(dplyr)

# Read all files in the folder
data <- read_birdnet_folder("detections_SL21", recursive = FALSE)
```

**Quick Stats**

Get a summary of your dataset:

``` r
birdnetprocess::summarise_detections(data, confidence = 0.7)
# # A tibble: 7 × 2
#   statistic                   value
#   <chr>                       <chr>
# 1 Number of species           49
# 2 Number of recordings        8042
# 3 Recording window            18 Jan 26 - 19 Jan 26
# 4 Most common species           Black Field Cricket
# 5 Peak hour                   2026-01-19 04:21:02
# 6 Average detections per day  4021
# 7 Average detections per hour 178.7111
```

**Quick Calls**

Visualize species counts:

``` r
birdnetprocess::plot_species_counts(data, confidence = 0.5)
```

<figure>
<img src="man/figures/top10_calls_over_time_package.png"
alt="Top 10 Calls" />
<figcaption aria-hidden="true">Top 10 Calls</figcaption>
</figure>

### 5. Visualizing Daily Patterns (Day/Night)

You can visualize daily activity patterns with day/night shading (using
the `suncalc` package). Note that you must provide the latitude,
longitude, and timezone for the shading to work.

``` r
# Generate plot with day/night shading
# Example coordinates for Sydney region
birdnetprocess::plot_top_species(
  data,
  n_top_species = 10,
  confidence = 0.6,
  latitude = -32.44, # Required for suncalc
  longitude = 152.24, # Required for suncalc
  tz = "Australia/Sydney"
)
```

<figure>
<img src="man/figures/day_night_stream.png" alt="Day Night Patterns" />
<figcaption aria-hidden="true">Day Night Patterns</figcaption>
</figure>

### 6. Custom Time Binning

By default, trends are plotted by the hour. However, you can use the
`unit` parameter to aggregate detections into any time interval
supported by `lubridate` (e.g., `"10 min"`, `"30 min"`, `"3 hours"`).
This is useful for high-resolution activity analysis.

``` r
# Plot trends with 10-minute binning
birdnetprocess::plot_top_species(
  data,
  n_top_species = 5,
  confidence = 0.5,
  unit = "10 min"
)
```

<figure>
<img src="man/figures/ten_min_trends.png" alt="10-Minute Trends" />
<figcaption aria-hidden="true">10-Minute Trends</figcaption>
</figure>

If you have a folder full of BirdNET files, use `read_birdnet_folder()`
to read them all at once. It will return a single combined tibble (as
shown above).

### Dependencies

Dependencies (lubridate and ggplot2 are key) should be installed
automatically when installing `birdnetprocess` from GitHub.
