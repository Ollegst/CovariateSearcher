# Create SCM report using existing search database

Simple function that reads your search_database and generates a
formatted report showing all steps, similar to console output

## Usage

``` r
generate_scm_report(
  search_state,
  output_file = "scm_report.txt",
  print_console = TRUE
)
```

## Arguments

- search_state:

  List containing search state with populated database

- output_file:

  Character. Path for report file (default: "scm_report.txt")

- print_console:

  Logical. Also print to console (default: TRUE)

## Value

Invisible NULL (writes to file)

## Details

Generate SCM Report from Existing Database
