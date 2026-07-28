# Parse LST file to extract actual NONMEM execution timestamps

Reads NONMEM .lst file to extract the start time (first line) and stop
time (line after "Stop Time:") when available.

## Usage

``` r
extract_nonmem_timestamps(model_name, models_folder = "models")
```

## Arguments

- model_name:

  Character. Model name (e.g., "run1")

- models_folder:

  Character. Path to models folder (default: "models")

## Value

List with start_time and stop_time (POSIXct or NA)

## Details

Extract NONMEM Start and Stop Timestamps from LST File
