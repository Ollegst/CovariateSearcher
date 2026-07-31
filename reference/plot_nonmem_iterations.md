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

  Logical. Use whatever the control stream declares about its own
  parameters (default: TRUE). Each panel is titled with the parameter's
  name from its `$THETA`/`$OMEGA`/`$SIGMA` comment, and a THETA
  annotated `;LOG` is exponentiated so it is plotted on the scale it is
  reported on. `;RATIO` and unannotated THETAs are plotted as estimated,
  and OMEGA/SIGMA are always shown as NONMEM wrote them. Anything the
  model does not declare falls back to the .ext, so an unannotated
  record keeps names like `THETA1` and `OMEGA.1.1.`. `FALSE` plots the
  .ext exactly as written - raw values and raw column names.

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

- Names and rescales parameters when `transform = TRUE`

- Creates faceted plots for each parameter and `obj_var`

`transform = TRUE` matches the control stream's annotations to the .ext
columns by position, the same assumption
[`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
makes. Every line of a record therefore needs its
`; NAME ; UNIT ; TRANS` comment: a line without one is not parsed at
all, and would shift every name after it onto the wrong parameter. Each
record is checked on its own and skipped whole when the counts disagree,
so a fully annotated `$THETA` is still named when a `BLOCK()` `$OMEGA`
cannot be.

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
