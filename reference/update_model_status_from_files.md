# Updates search database with results from NONMEM output files

Reads NONMEM output files and updates database. Includes force flag to
control re-reading of completed models for efficiency.

## Usage

``` r
update_model_status_from_files(search_state, model_name, force = FALSE)
```

## Arguments

- search_state:

  List. Current search state with database and configuration

- model_name:

  Character. Model name to update (e.g., "run11")

- force:

  Logical. If TRUE, forces re-reading even for completed models
  (default: FALSE)

## Value

List with updated search_state

## Details

Update Model Status from Files with Enhanced Error Detection and Force
Flag
