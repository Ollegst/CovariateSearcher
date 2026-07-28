# Extend a running search with covariates it did not start with

Appends rows to `search_state$covariate_search`, revalidates the merged
table, regenerates `tags.yaml` and refreshes `search_state$tags`. Use it
when a covariate that was not part of the original search table has to
be tested – typically an extra covariate tried on the final model after
the search has finished. Without this the new covariate has no
`beta_<COV>_<PARAM>` tag and no table row, so
[`add_covariate_to_model`](https://ollegst.github.io/CovariateSearcher/reference/add_covariate_to_model.md)
cannot see it.

The alternative – re-running
[`initialize_covariate_search`](https://ollegst.github.io/CovariateSearcher/reference/initialize_covariate_search.md)
on a folder that already holds the finished search – rebuilds the
database from the model files, which loses every `step_number`,
`delta_ofv` and the real phase labels. Extending the live `search_state`
keeps them.

## Usage

``` r
add_covariates_to_search(
  search_state,
  additions,
  tags_yaml_path = "data/spec/tags.yaml",
  verbose = TRUE
)
```

## Arguments

- search_state:

  List. The live search state to extend (e.g. `results$search_state`, or
  a checkpoint loaded with
  [`load_search_state`](https://ollegst.github.io/CovariateSearcher/reference/load_search_state.md)).

- additions:

  Data frame of new covariate rows (as built by
  [`build_covariate_reference_table`](https://ollegst.github.io/CovariateSearcher/reference/build_covariate_reference_table.md)),
  or a path to a `.csv` holding them. `cov_to_test` is derived when
  absent.

- tags_yaml_path:

  Character. Tags file to regenerate. Default `"data/spec/tags.yaml"`.

- verbose:

  Logical. Print what was added. Default `TRUE`.

## Value

The updated `search_state`, with the merged `covariate_search` and
refreshed `tags`.

## Details

Add Covariates to an Existing Search

## See also

[`build_covariate_reference_table`](https://ollegst.github.io/CovariateSearcher/reference/build_covariate_reference_table.md),
[`add_covariate_to_model`](https://ollegst.github.io/CovariateSearcher/reference/add_covariate_to_model.md)
