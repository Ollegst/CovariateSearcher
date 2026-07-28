# Write modified NONMEM control file back to disk

Writes model file lines back to original location using stored file_path
attribute

## Usage

``` r
write_model_file(search_state, lines)
```

## Arguments

- search_state:

  List containing search state (unchanged in functional version)

- lines:

  Character vector. Model file lines with file_path attribute

## Value

Updated search_state (unchanged)

## Details

Write Model File
