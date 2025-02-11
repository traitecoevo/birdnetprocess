birdnetprocess
================
Will Cornwell
2025-02-11

## Install

``` r
# install.packages("devtools") # if needed
 devtools::install_github("traitecoevo/birdnetprocess")
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'birdnetprocess' from a github remote, the SHA1 (40d5e059) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example Usage

#### 1. Extract Start DateTime from a BirdNET Filename

The function parse_birdnet_filename_datetime() assumes filenames follow
this pattern:

`SOMETHING_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt`

``` r
library(birdnetprocess)
library(dplyr)
```

### Extract a start date-time from a BirdNET filename

``` r
parsed_time <- birdnetprocess::parse_birdnet_filename_datetime(
  "1STSMM2_20241105_050000.BirdNET.selection.table.txt"
)
parsed_time
#> [1] "2024-11-05 05:00:00 UTC"
```

Expected output is a POSIXct datetime (e.g., “2024-11-05 05:00:00 UTC”).

#### 2. Read a Single BirdNET File

Use read_birdnet_file() to read one BirdNET selection table. This will:

    Read the tab-delimited file.
    Parse the filename for the start time.
    Add columns:
        file_name
        start_time
        recording_window_time, which is start_time + [Begin Time (s)].

``` r

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

#### 3. Read All BirdNET Files in a Folder

If you have a folder full of BirdNET .selection.table.txt files, use
read_birdnet_folder() to read them all at once. It will return a single
combined tibble.

``` r

# Read all BirdNET files in a directory and combine into one data frame
all_detections <- read_birdnet_folder(
  folder = "ignore/cc_output/", 
  pattern = "BirdNET.selection.table.txt$",
  recursive = FALSE
)

dplyr::glimpse(all_detections)
#> Rows: 35,141
#> Columns: 15
#> $ Selection             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
#> $ View                  <chr> "Spectrogram 1", "Spectrogram 1", "Spectrogram 1…
#> $ Channel               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ `Begin Time (s)`      <dbl> 891, 891, 903, 909, 909, 918, 927, 939, 948, 960…
#> $ `End Time (s)`        <dbl> 894, 894, 906, 912, 912, 921, 930, 942, 951, 963…
#> $ `Low Freq (Hz)`       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ `High Freq (Hz)`      <dbl> 12000, 12000, 12000, 12000, 12000, 12000, 12000,…
#> $ `Common Name`         <chr> "Willie-wagtail", "Western Whipbird", "Willie-wa…
#> $ `Species Code`        <chr> "wilwag1", "weswhi1", "wilwag1", "wilwag1", "fla…
#> $ Confidence            <dbl> 0.4031, 0.1383, 0.3234, 0.2425, 0.1355, 0.3680, …
#> $ `Begin Path`          <chr> "cc/1STSMM2_20241020_010000.wav", "cc/1STSMM2_20…
#> $ `File Offset (s)`     <dbl> 891, 891, 903, 909, 909, 918, 927, 939, 948, 960…
#> $ file_name             <chr> "1STSMM2_20241020_010000.BirdNET.selection.table…
#> $ start_time            <dttm> 2024-10-20 01:00:00, 2024-10-20 01:00:00, 2024-…
#> $ recording_window_time <dttm> 2024-10-20 01:14:51, 2024-10-20 01:14:51, 2024-…
```

Then you can proceed to filter or process as needed for the project.

### Dependencies

Under the hood, `birdnetprocess` uses:

    - `stringr` for string matching and extraction.
    - `lubridate` for parsing and handling date-time data.
    - `readr` for reading tab-delimited text files.
    - `dplyr` and `purrr` for data manipulation.

These packages should be installed automatically when installing
`birdnetprocess` from GitHub.
