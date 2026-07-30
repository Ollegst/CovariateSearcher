# Load existing models and recreate search state

Discovers existing models in the models folder and recreates the search
database. Use this to continue work on an existing project.

## Usage

``` r
load_existing_search(
  base_model_path,
  data_file_path,
  covariate_search_path,
  models_folder = "models",
  timecol = "TIME",
  idcol = "ID",
  threads = 60,
  require_cov_step = TRUE,
  lookup_file = NULL
)
```

## Arguments

- base_model_path:

  Character. Path to base model (e.g., "run1")

- data_file_path:

  Character. Path to NONMEM dataset CSV

- covariate_search_path:

  Character. Path to covariate search CSV

- models_folder:

  Character. Directory containing models (default: "models")

- timecol:

  Character. Time column name (default: "TIME")

- idcol:

  Character. ID column name (default: "ID")

- threads:

  Integer. Number of threads for execution (default: 60)

- require_cov_step:

  Logical. Whether a successful covariance step (a `.cov` file) is
  required for a model to count as completed (default: TRUE). Passed to
  [`initialize_covariate_search()`](https://ollegst.github.io/CovariateSearcher/reference/initialize_covariate_search.md),
  so it governs the base model check and every model discovered on disk.

- lookup_file:

  Character or NULL. Optional path to lookup YAML for categorical
  labels. If NULL, defaults to data/spec/lookup.yaml.

## Value

List containing search state with discovered models

## Details

Load Existing Covariate Search
