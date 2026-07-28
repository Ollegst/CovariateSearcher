# Create one prepared base model with multiple covariates

Copies a parent model, adds multiple covariates into the same child
model, writes one combined technical log, and returns metadata for later
initialization. Does NOT require search_database. Before adding, it
validates the covariate-\>parameter mapping and each target parameter's
transform (the same checks `initialize_covariate_search` runs, scoped to
the covariates being added) so a mis-parameterized target fails fast.

## Usage

``` r
prepare_search_base_model(
  base_model_path,
  covariate_tags,
  new_model_number,
  data_file_path,
  covariate_search_path,
  models_folder = "models",
  idcol = "ID",
  overwrite = TRUE,
  lookup_file = NULL
)
```

## Arguments

- base_model_path:

  Character. Parent/base model id, e.g. "run1"

- covariate_tags:

  Character vector of covariate tags, e.g. c("beta_WT_CL",
  "beta_AGE_V2")

- new_model_number:

  Integer. Required model number for the new model

- data_file_path:

  NONMEM dataset, given as either an in-memory `data.frame` OR a
  character path to a `.csv`/`.rds` file (loaded here).

- covariate_search_path:

  Covariate search table, given as either an in-memory `data.frame` OR a
  character path to a `.csv`/`.rds` file.

- models_folder:

  Character. Models directory

- idcol:

  Character. ID column name

- overwrite:

  Logical. Overwrite existing model if present

- lookup_file:

  Character or NULL. Optional path to lookup YAML for categorical
  labels. If NULL, defaults to data/spec/lookup.yaml.

## Value

List with status, model_name, model_path, log_file, covariates_added

## Details

Prepare Search Base Model by Adding Multiple Covariates
