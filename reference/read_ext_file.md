# Parse .ext files and detect OFV \> 10^10 and other estimation problems

Reads NONMEM .ext files to extract current OFV and detect estimation
issues including boundary failures (OFV \> 10^10), infinite values, and
other numerical problems.

## Usage

``` r
read_ext_file(search_state, model_name)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- model_name:

  Character. Model name (e.g., "run25")

## Value

List with status, current_ofv, iterations, and issue detection

## Details

Read NONMEM .ext File and Detect Estimation Issues
