
# birdnetprocess

<!-- badges: start -->
[![R-CMD-check](https://github.com/traitecoevo/birdnetprocess/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/traitecoevo/birdnetprocess/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/traitecoevo/birdnetprocess/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/traitecoevo/birdnetprocess/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

`birdnetprocess` helps you process and visualize BirdNET detection results.

## Install

```r
# install.packages("devtools") # if needed
devtools::install_github("traitecoevo/birdnetprocess")
```

## Example Usage
#### 1. Extract Start DateTime from a BirdNET Filename

The function parse_birdnet_filename_datetime() assumes filenames follow this pattern:

`SOMETHING_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt`

```r
library(birdnetprocess)
library(dplyr)
```

### Extract a start date-time from a BirdNET filename
```r
parsed_time <- birdnetprocess::parse_birdnet_filename_datetime(
  "1STSMM2_20241105_050000.BirdNET.selection.table.txt"
)
parsed_time
```
Expected output is a POSIXct datetime (e.g., "2024-11-05 05:00:00 UTC").

#### 2. Read a Single BirdNET File

Use read_birdnet_file() to read one BirdNET selection table. This will:

    Read the tab-delimited file.
    Parse the filename for the start time.
    Add columns:
        file_name
        start_time
        recording_window_time, which is start_time + [Begin Time (s)].
```r
# Suppose you have a BirdNET file at this path:
birdnet_file <- "ignore/cc_output/1STSMM2_20241105_050000.BirdNET.selection.table.txt"

df_single <- read_birdnet_file(birdnet_file)
head(df_single)
```
Note:

    Ensure the file’s “Begin Time (s)” column name matches the one you expect in your BirdNET exports.
    You can specify a timezone via the tz argument if needed.


#### 3. Read All BirdNET Files in a Folder


If you have a folder full of BirdNET .selection.table.txt files, use read_birdnet_folder() to read them all at once. It will return a single combined tibble.
```r
# Read all BirdNET files in a directory and combine into one data frame
all_detections <- read_birdnet_folder(
  folder = "ignore/cc_output/",
  pattern = "BirdNET.selection.table.txt$",
  recursive = FALSE
)

dplyr::glimpse(all_detections)
```

Then you can proceed to filter or process as needed for the project. 

#### 4. Quick Visualization and Statistics

`quickcalls` and `quickstats` provide immediate insights into your data.

**Quick Stats**
Get a summary of your dataset:

```r
birdnetprocess::quickstats(all_detections, confidence = 0.5)
```

**Quick Calls**
Visualize species counts:

```r
birdnetprocess::quickcalls(all_detections, confidence = 0.5)
```

![Top 10 Calls](analysis/plots/top10_calls_over_time_package.png)


### Dependencies

Under the hood, `birdnetprocess` uses:

    - `stringr` for string matching and extraction.
    - `lubridate` for parsing and handling date-time data.
    - `readr` for reading tab-delimited text files.
    - `dplyr` and `purrr` for data manipulation.

These packages should be installed automatically when installing `birdnetprocess` from GitHub.

