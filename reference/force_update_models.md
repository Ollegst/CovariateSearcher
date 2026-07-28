# Force update one or more models with fresh file reads

Forces a complete re-read of model files and updates database. Useful
for correcting database values that may be incorrect.

## Usage

``` r
force_update_models(search_state, model_names)
```

## Arguments

- search_state:

  List containing search state

- model_names:

  Character vector. Model(s) to force update (can be single or multiple)

## Value

Updated search_state

## Details

Force Update Models with Fresh File Read
