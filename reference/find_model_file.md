# Find the actual file path for a NONMEM model

Given a base path (without extension), finds the actual model file by
trying .ctl and .mod extensions in order.

## Usage

``` r
find_model_file(base_path, extensions = c(".ctl", ".mod"))
```

## Arguments

- base_path:

  Character. Base path without extension (e.g., "models/run001")

- extensions:

  Character vector. File extensions to try (default: c(".ctl", ".mod"))

## Value

Character string with the full path to the found file, or NULL if not
found

## Details

Find Model File Path
