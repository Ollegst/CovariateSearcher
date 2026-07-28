# Initialize covariate search state with validation and setup

Main initialization function that sets up the search state, loads data
files, validates configuration, and discovers existing models.

## Usage

``` r
initialize_covariate_search(
  base_model_path,
  data_file_path,
  covariate_search_path,
  models_folder = "models",
  timecol = "TIME",
  idcol = "ID",
  threads = 60,
  validate_parameters = TRUE,
  require_base_run = TRUE,
  lookup_file = NULL,
  starting_model_number = NULL
)
```

## Arguments

- base_model_path:

  Character. Path to base model (e.g., "run1")

- data_file_path:

  NONMEM dataset, given as either an in-memory `data.frame` OR a
  character path to a `.csv`/`.rds` file (loaded here).

- covariate_search_path:

  Covariate search table, given as either an in-memory `data.frame` OR a
  character path to a `.csv`/`.rds` file.

- models_folder:

  Character. Directory containing models (default: "models")

- timecol:

  Character. Time column name (default: "TIME")

- idcol:

  Character. ID column name (default: "ID")

- threads:

  Integer. Number of threads for execution (default: 60)

- validate_parameters:

  Logical. Validate parameter block formatting (default: TRUE)

- require_base_run:

  Logical. Require the base model to have a completed run (readable
  `.lst`/`.ext` with an OFV) before initializing (default: TRUE). Set to
  FALSE for SETUP/TESTING only – it skips the run/OFV check so you can
  build the search_state and exercise covariate add/remove on control
  streams without running NONMEM. A real search still needs a completed
  base model OFV.

- lookup_file:

  Character or NULL. Optional path to lookup YAML for categorical
  covariate labels. If NULL, defaults to data/spec/lookup.yaml.

- starting_model_number:

  Optional integer. Sets the model counter manually. Use this when
  covariate search starts from an existing structural model and newly
  created covariate models should continue numbering from the last model
  already present in the workflow. For example, if the last existing
  model is `run10`, set `starting_model_number = 10` so the next model
  created by covariate search will be `run11`.

## Value

List containing complete search state configuration

## Details

Initialize Covariate Search
