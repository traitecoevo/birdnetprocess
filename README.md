birdnetprocess
================
Will Cornwell
2025-02-11

``` r
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

Install

``` r
# install.packages("devtools") # if needed
# devtools::install_github("traitecoevo/birdnetprocess")
```

Example Usage 1. Extract Start DateTime from a BirdNET Filename

The function parse_birdnet_filename_datetime() assumes filenames follow
this pattern:

`SOMETHING_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt`

``` r
library(birdnetprocess)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

# Extract a start date-time from a BirdNET filename

``` r
parsed_time <- birdnetprocess::parse_birdnet_filename_datetime(
  "1STSMM2_20241105_050000.BirdNET.selection.table.txt"
)
parsed_time
#> [1] "2024-11-05 05:00:00 UTC"
```

Expected output is a POSIXct datetime (e.g., “2024-11-05 05:00:00 UTC”).
2. Read a Single BirdNET File

Use read_birdnet_file() to read one BirdNET selection table. This will:

    Read the tab-delimited file.
    Parse the filename for the start time.
    Add columns:
        file_name
        start_time
        recording_window_time, which is start_time + [Begin Time (s)].

``` r
library(birdnetprocess)

# Suppose you have a BirdNET file at this path:
birdnet_file <- "ignore/cc_output/1STSMM2_20241105_050000.BirdNET.selection.table.txt"

df_single <- read_birdnet_file(birdnet_file)
head(df_single)
#> # A tibble: 6 × 15
#>   Selection View         Channel `Begin Time (s)` `End Time (s)` `Low Freq (Hz)`
#>       <dbl> <chr>          <dbl>            <dbl>          <dbl>           <dbl>
#> 1         1 Spectrogram…       1               12             15               0
#> 2         2 Spectrogram…       1               18             21               0
#> 3         3 Spectrogram…       1               24             27               0
#> 4         4 Spectrogram…       1               30             33               0
#> 5         5 Spectrogram…       1               30             33               0
#> 6         6 Spectrogram…       1               30             33               0
#> # ℹ 9 more variables: `High Freq (Hz)` <dbl>, `Common Name` <chr>,
#> #   `Species Code` <chr>, Confidence <dbl>, `Begin Path` <chr>,
#> #   `File Offset (s)` <dbl>, file_name <chr>, start_time <dttm>,
#> #   recording_window_time <dttm>
```

Note:

    Ensure the file’s “Begin Time (s)” column name matches the one you expect in your BirdNET exports.
    You can specify a timezone via the tz argument if needed.

3.  Read All BirdNET Files in a Folder

If you have a folder full of BirdNET .selection.table.txt files, use
read_birdnet_folder() to read them all at once. It will return a single
combined tibble.

``` r
library(birdnetprocess)

# Read all BirdNET files in a directory
all_detections <- read_birdnet_folder(
  folder = "path/to/BirdNET_outputs", 
  pattern = "BirdNET.selection.table.txt$",
  recursive = FALSE
)
#> Warning in read_birdnet_folder(folder = "path/to/BirdNET_outputs", pattern =
#> "BirdNET.selection.table.txt$", : No files found matching pattern in folder.

dplyr::glimpse(all_detections)
#> Rows: 0
#> Columns: 0
```

### Dependencies

Under the hood, birdnetprocess uses:

    stringr for string matching and extraction.
    lubridate for parsing and handling date-time data.
    readr for reading tab-delimited text files.
    dplyr and purrr for data manipulation.

These packages should be installed automatically when installing
birdnetprocess from GitHub or from source (assuming they are listed in
the DESCRIPTION under Imports).
