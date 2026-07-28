# Read NONMEM control file with proper path handling

Reads model control file (.ctl or .mod) and stores file path as
attribute

## Usage

``` r
read_model_file(search_state, run_name, extensions = c(".ctl", ".mod"))
```

## Arguments

- search_state:

  List containing search state

- run_name:

  Character. Model name

- extensions:

  Character vector. File extensions to try (default: c(".ctl", ".mod"))

## Value

Character vector of model file lines with file_path attribute

## Details

Read Model File
