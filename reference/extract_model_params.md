# Extract Model Parameters from Control File

Internal function to extract all parameter names from a NONMEM control
file

## Usage

``` r
extract_model_params(model_name, models_folder = "models")
```

## Arguments

- model_name:

  Character string. Name of the model (without .ctl extension)

- models_folder:

  Character string. Path to models folder (default: "models")

## Value

A list containing THETAS, OMEGAS, and SIGMA parameters with
transformations
