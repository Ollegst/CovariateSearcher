# Handle failed retry model by excluding covariate from step

When a retry model fails, exclude the associated covariate from the
current step but keep it available for final phase testing.

## Usage

``` r
handle_failed_retry(
  search_state,
  retry_model_name,
  exclusion_reason = "retry_failed"
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- retry_model_name:

  Character. Name of failed retry model (e.g., "run25001")

- exclusion_reason:

  Character. Reason for exclusion

## Value

List with exclusion information and updated search_state

## Details

Handle Failed Retry Model
