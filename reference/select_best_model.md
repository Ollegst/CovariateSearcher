# Evaluate models and select the best one based on statistical criteria

Evaluates completed models using delta OFV and RSE thresholds to
identify the best performing model. ΔOFV threshold is calculated based
on p-value and covariate degrees of freedom (df=1 for continuous,
df=n_levels-1 for categorical).

## Usage

``` r
select_best_model(
  search_state,
  model_names,
  p_value = NULL,
  rse_threshold = NULL
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- model_names:

  Character vector. Model names to evaluate

- p_value:

  Numeric. P-value for forward selection (uses config if NULL)

- rse_threshold:

  Numeric. Maximum RSE threshold (uses config if NULL)

## Value

List with best model, evaluation details, and updated search_state

## Details

Select Best Model from Statistical Evaluation
