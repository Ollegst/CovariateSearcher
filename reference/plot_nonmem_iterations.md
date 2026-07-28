# Plot NONMEM Iteration Data

Creates a multi-panel plot showing the trajectory of objective function
and parameter estimates across iterations. Useful for diagnosing
estimation problems, convergence issues, and parameter stability.

## Usage

``` r
plot_nonmem_iterations(
  model_name,
  models_dir = "models",
  transform = TRUE,
  skip_iterations = 0,
  obj_var = "OBJ",
  max_iterations = 100
)
```

## Arguments

- model_name:

  Character. Name of the model (e.g., "run123")

- models_dir:

  Character. Directory containing model files (default: "models")

- transform:

  Logical. Apply transformations to parameters (default: TRUE)

- skip_iterations:

  Integer. Number of initial iterations to skip (default: 0)

- obj_var:

  Character. Variable to plot (default: "OBJ" for objective function)

- max_iterations:

  Integer. Maximum number of iterations to display (default: 100)

## Value

ggplot2 object with faceted plots showing parameter trajectories

## Details

The function:

- Reads the .ext file for the specified model

- Filters to iteration data (ITER and BURN types)

- Removes fixed parameters (those that don't change)

- Adjusts BURN iteration numbers for continuous display

- Creates faceted plots for each parameter and OBJ

## Examples

``` r
if (FALSE) { # \dontrun{
# Plot iteration data for run123
p <- plot_nonmem_iterations("run123")
print(p)

# Skip first 10 iterations and limit to 50 iterations
p <- plot_nonmem_iterations("run123", skip_iterations = 10, max_iterations = 50)

# Use custom models directory
p <- plot_nonmem_iterations("run123", models_dir = "path/to/models")
} # }
```
