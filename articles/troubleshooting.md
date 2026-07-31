# Troubleshooting Guide

## Troubleshooting Guide

Common issues and solutions when using CovariateSearcher.

### Model Failures

#### Issue: Models fail with “MINIMIZATION NOT SUCCESSFUL”

**Solution**: The recovery system should handle this automatically.
Check:

``` r

# Check if retry models were created
retries <- search_state$search_database %>%
  filter(phase == "retry")

print(retries)
```

If retries also failed, you may need to:

1.  Adjust initial estimates in base model
2.  Simplify the covariate relationship
3.  Check for collinearity between covariates

#### Issue: “Error 134” or Singular Matrix

**Solution**: This indicates numerical issues. The recovery system will:

1.  Create retry model with different THETA sign
2.  Try alternative parameterization

#### Issue: OFV \> 10^10 (Infinite OFV)

**Solution**: Automatically detected and model marked as failed.

``` r

# View failed models
failed <- search_state$search_database %>%
  filter(status == "failed")

print(failed$estimation_issue)
```

### Selection Issues

#### Issue: No models selected in Step 2+

**Cause**: Models may not meet BOTH criteria (ΔOFV AND RSE)

**Solution**: Check which criterion is failing:

``` r

step2_models <- search_state$search_database %>%
  filter(step_number == 2, phase == "forward_selection")

# Check ΔOFV
step2_models %>%
  select(model_name, covariate_tested, delta_ofv, rse_max) %>%
  mutate(
    passes_ofv = delta_ofv > 3.84,
    passes_rse = rse_max < 50
  )
```

Consider:

- Lowering RSE threshold if needed
- Checking if covariates are too correlated

#### Issue: Backward elimination removes all covariates

**Cause**: Threshold too low or covariates not truly significant

Note: Backward elimination now evaluates both ΔOFV and RSE. Removals can
also be blocked by high RSE. If RSE is unavailable (NA), that is treated
as acceptable.

**Solution**: Use more stringent backward threshold (6.63 vs 3.84)

### Data Issues

#### Issue: “Covariate not found in dataset”

**Solution**: Check column names match exactly:

``` r

# Check dataset columns
names(read.csv(search_state$data_file_path))

# Check covariate config
search_state$covariate$search %>%
  pull(COVARIATE) %>%
  unique()
```

#### Issue: Missing values in covariates

**Solution**: NONMEM requires complete data. Either: 1. Impute missing
values before SCM 2. Exclude subjects with missing covariates

``` r

data <- read.csv(search_state$data_file_path)

# Check for missing
sapply(data, function(x) sum(is.na(x)))
```

### Performance Issues

#### Issue: Models running slowly

**Solution**: 1. Check cluster configuration 2. Reduce number of
simultaneous submissions 3. Use selective forward search instead of full
search

``` r

# Test only significant covariates from step 1
result <- run_scm_selective_forward(
  search_state = search_state,
  forward_p_value  = 0.01
)
```

### Database Issues

#### Issue: Database out of sync with model files

**Solution**: Force database update:

``` r

# Re-read every model that is not already in a terminal state
search_state <- update_all_model_statuses(search_state)

# The call above skips a model whose status is already terminal (completed,
# failed, estimation_error), one tagged do_not_run, and any model this search did
# not create unless another row names it as its parent. To pick up output that
# appeared afterwards -- a model you resubmitted by hand, say -- refresh it with
# force = TRUE, which applies none of those filters.
# Note the assignment: the function returns a modified state and changes nothing
# in place.
for (m in c("run24", "run27")) {
  search_state <- update_model_status_from_files(search_state, m, force = TRUE)
}
```

Resuming a search refreshes the last step for you; see [Understanding
the Recovery
System](https://ollegst.github.io/CovariateSearcher/articles/recovery-system.html#resuming-a-search).

#### Issue: Tags not showing all covariates

**Solution**: This was a bug fixed in recent version. Update to latest
version.

### Getting Help

If issues persist:

1.  Check model .lst files for NONMEM errors
2.  Verify base model runs successfully
3.  Review database for error messages
4.  Open issue on GitHub with reproducible example
