# Generate comprehensive SCM results table with evaluation comments

Creates a detailed table showing phase/step, model name, all covariates
currently in the model, OFV, delta OFV, RSE, selection status, and
comments explaining why models were or weren't selected

## Usage

``` r
create_scm_results_table(search_state)
```

## Arguments

- search_state:

  List containing search state with database and tags

## Value

Data frame with comprehensive SCM results including evaluation comments

## Details

Create Enhanced SCM Results Table with RSE Evaluation Comments
