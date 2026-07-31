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

  Logical. Show parameters on their reported scale rather than as
  written in the .ext file (default: TRUE). A THETA annotated `;LOG` in
  the control stream is exponentiated, and a diagonal OMEGA/SIGMA is
  converted to CV% - the same rules
  [`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
  applies - so the trajectories and the parameter table agree.
  Off-diagonal elements and unannotated THETAs are unchanged. `FALSE`
  plots the raw .ext values.

- skip_iterations:

  Integer. Number of initial iterations to skip (default: 0)

- obj_var:

  Character. Column plotted in the leading panel (default: "OBJ"). Must
  name a column of the .ext file;
  [`read_ext_iterations()`](https://ollegst.github.io/CovariateSearcher/reference/read_ext_iterations.md)
  standardises any objective column to `"OBJ"`, so the default fits
  every estimation method.

- max_iterations:

  Integer. Upper bound on the iteration NUMBER displayed (keeps
  `ITERATION < max_iterations`), not a count of points (default: 100)

## Value

ggplot2 object with faceted plots showing parameter trajectories

## Details

The function:

- Reads the .ext file for the specified model

- Filters to iteration data (ITER and BURN types)

- Keeps a single estimation step - the highest-numbered `$EST` that is
  not an evaluation step

- Removes fixed parameters (those that don't change)

- Adjusts BURN iteration numbers for continuous display

- Rescales parameters when `transform = TRUE`

- Creates faceted plots for each parameter and `obj_var`

`transform = TRUE` reads the `$THETA` annotations from the control
stream and aligns them to the .ext THETA columns by position, the same
assumption
[`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
makes. Every `$THETA` line therefore needs its `; NAME ; UNIT ; TRANS`
comment: an unannotated line is not parsed, which would shift the
alignment. When the two counts disagree the function warns and leaves
THETAs as estimated rather than rescaling the wrong one.

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
