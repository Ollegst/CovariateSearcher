CovariateSearcher: Quick Reference
================
CovariateSearcher Package
2026-05-27

## 🚀 Minimal Working Example

``` r
# Install from GitHub
remotes::install_github("Ollegst/CovariateSearcher", ref = "main", dependencies = TRUE)

library(CovariateSearcher)

# 1. Initialize
search_state <- initialize_covariate_search(
  base_model_path = "run10",
  data_file_path = "data/derived/analysis.csv",
  covariate_search_path = "data/derived/covariate_search.csv",
  lookup_file = "data/spec/lookup.yaml",  # optional
  models_folder = "models", 
  threads = 40)


# 2. Run SCM
result <- run_automated_scm_testing(
        search_state = search_state,
        scm_type = "selective",                     # or "standard"  
    starting_phase = "forward",                 # Starting phase forward/backward
    full_scm = TRUE,                            # Run complete workflow 
    forward_p_value = 0.05,                       # p-value for the forward selection   
    backward_p_value = 0.01,                    # p-value for the backward elimination
    rse_threshold = 50,                         # RSE threshold
    require_cov_step = TRUE,                    # Require successful covariance step
    auto_submit = TRUE,                         # if FALSE – only model creation 
    auto_retry = TRUE,                          # Retry model in case of failure
    save_checkpoints = TRUE,                    # Save each step
    checkpoint_prefix = "scm_auto",
    final_testing = TRUE)                       # Test excluded covariates form failed model


# 3. Check results
search_state <-readRDS("./models/scm_rds/scm_auto_final_complete.rds")
results_table <- create_scm_results_table(search_state)
print_scm_results_table(search_state)
```

## Or if you need to create model with multiple covariates as base model

``` r
library(CovariateSearcher)

data_file_path = "data/test.csv"
covariate_search_path = "data/derived/covariate_search.csv"

data_file <- readr::read_csv(data_file_path, show_col_types = FALSE)
covariate_search <- readr::read_csv(covariate_search_path, show_col_types = FALSE)

covariate_search <- validate_covariate_search_table(
  covariate_search = covariate_search,
  data_file = data_file
)

#Explain code below              

# 1. Prepare base model     
prep <- prepare_search_base_model(
  base_model_path = "run1",
  covariate_tags = c("beta_WT_CL", "beta_AGE_V2", "beta_SEX_KA"),
  new_model_number = 2,
  data_file_path = data_file_path,
  covariate_search_path = covariate_search_path,
  lookup_file = "data/spec/lookup.yaml",  # optional
  models_folder = "models",
  idcol = "ID"
)

mod <- bbr::read_model(file.path("models", prep$model_name))
bbr::submit_model(mod, .bbi_args = list(threads = 4), .overwrite = TRUE)

# 2. Initialize
search_state <- initialize_covariate_search(
  base_model_path = "run2",
  data_file_path = data_file_path,
  covariate_search_path = covariate_search_path,
  lookup_file = "data/spec/lookup.yaml",  # optional
  models_folder = "models", 
  threads = 4)

# 3. Run SCM
result <- run_automated_scm_testing(
        search_state = search_state,
        scm_type = "selective",                     # or "standard"  
    starting_phase = "backward",                # Starting phase
    full_scm = TRUE,                            # Run complete workflow 
    forward_p_value = 0.05,                       # p-value for the forward selection   
    backward_p_value = 0.01,                    # p-value for the backward elimination
    rse_threshold = 50,                         # RSE threshold
    require_cov_step = TRUE,                    # Require successful covariance step
    auto_submit = TRUE,                         # if FALSE – only model creation 
    auto_retry = TRUE,                          # Retry model in case of failure
    save_checkpoints = TRUE,                    # Save each step
    checkpoint_prefix = "scm_auto",
    final_testing = TRUE)                       # Test excluded covariates from failed model


# 4. Check results
search_state <-readRDS("./models/scm_rds/scm_auto_final_complete.rds")
results_table <- create_scm_results_table(search_state)
print_scm_results_table(search_state)
```

------------------------------------------------------------------------

## 📋 Required Files

### 1. Base Model

`models/run10.ctl` - NONMEM control file

**Requirements**:

    $THETA
    ; Simple parameter names 
    0.5 ; CL ; L/h ; RATIO
    10  ; V  ; L   ; RATIO
    ; OR numbered format
    0.5 ; 1_CL ; L/h ; RATIO
    10  ; 2_V  ; L   ; RATIO

    $PK
    ; Use TV_ prefix in PK block for typical values
    TV_CL = THETA(1)
    TV_V  = THETA(2)
    CL = TV_CL * EXP(ETA(1))
    V  = TV_V  * EXP(ETA(2))

    $OMEGA BLOCK(2)
    0.1 ; IIV_CL   ; ; LOG
    0.1 ; IIV_CL_V ; ; RATIO
    0.1 ; IIV_V    ; ; LOG

    $SIGMA
    0.1 ; RUV ; ; RATIO

Format: `value ; NAME ; units ; RATIO|LOG`

**THETA Naming**: Use simple names (e.g., `CL`, `V`) or numbered (e.g.,
`1_CL`, `2_V`) in `$THETA` block. Use `TV_` prefix only in `$PK`/`$PRED`
block.

### 2. Dataset

`data/derived/analysis.csv` - NONMEM dataset with covariates

### 3. Covariate Search

`data/derived/covariate_search.csv`

| PARAMETER | COVARIATE | STATUS | FORMULA | LEVELS | REFERENCE | TIME_DEPENDENT |
|-----------|-----------|--------|---------|--------|-----------|----------------|
| CL        | WT        | con    | power   | NA     | 70        | FALSE          |
| CL        | AGE       | con    | linear  | NA     | 50        | FALSE          |
| CL        | ECOG      | cat    | linear  | 0;1;2  | 1         | FALSE          |
| V2        | WT        | con    | power   | NA     | 70        | FALSE          |

**FORMULA options for continuous covariates (`STATUS = "con"`):** -
`linear`: `PARAM * (1 + (COV - REF) * THETA)` - `power`:
`PARAM * (COV / REF) ** THETA` - `power1`:
`PARAM * (COV / REF) ** THETA` with THETA fixed to 1 - `power0.75`:
`PARAM * (COV / REF) ** THETA` with THETA fixed to 0.75 - `exponential`:
`PARAM * EXP(THETA * (COV - REF))`

------------------------------------------------------------------------

### Save/Load State

``` r
save_search_state(search_state, "state.rds")
search_state <- load_search_state("state.rds")
```

------------------------------------------------------------------------

## 📊 Workflow Types

### Standard SCM/SCM+

``` r
starting_phase = "forward"
full_scm = TRUE
```

➜ Forward → Backward

### SCM/SCM+ with start from previously developed model

``` r
starting_phase = "backward"
full_scm = TRUE
```

➜ Backward → Forward → Backward

### Backward Only

``` r
results <- run_automated_scm_testing(
  search_state = search_state,
  base_model_id = "run10",
  starting_phase = "backward",
  full_scm = FALSE,            # This runs ONLY the specified phase
  backward_p_value = 0.01,
  require_cov_step = TRUE,
  auto_submit = TRUE,
  auto_retry = TRUE
)
```

### Forward only

``` r
results <- run_automated_scm_testing(
 search_state = search_state,
 base_model_id = "run10",
 scm_type = "standard",      # or "selective"
 starting_phase = "forward",
 full_scm = FALSE,           # This runs ONLY forward selection
 forward_p_value = 0.05,
 rse_threshold = 50,
 require_cov_step = TRUE,
 auto_submit = TRUE,
 auto_retry = TRUE
)
```

------------------------------------------------------------------------

## ⚙️ Common Parameters

| Parameter          | Default    | Options                 |
|--------------------|------------|-------------------------|
| `scm_type`         | “standard” | “standard”, “selective” |
| `starting_phase`   | “forward”  | “forward”, “backward”   |
| `full_scm`         | TRUE       | TRUE, FALSE             |
| `forward_p_value`  | 0.05       | 0.001-0.1               |
| `backward_p_value` | 0.01       | 0.001-0.1               |
| `rse_threshold`    | 50         | 0-100                   |
| `require_cov_step` | TRUE       | TRUE, FALSE             |
| `auto_submit`      | TRUE       | TRUE, FALSE             |
| `auto_retry`       | TRUE       | TRUE, FALSE             |

------------------------------------------------------------------------

## 📝 Check Results

``` r
# Final model
results$final_model

# Final covariates
results$final_covariates

# Excluded covariates
results$excluded_covariates

# View database
View(results$search_state$search_database)

# Summary
cat(results$final_summary)

# Parameter table (includes OFV and Conditional number rows)
ft <- model_report(model_names = "run10", models_folder = "models")
ft

# Or if you want to compare several models
ft <- model_report(model_names = c("run10", "run11", "run12"), models_folder = "models")
ft

# With parameter metadata (labels/short names/units/comments)
# IMPORTANT: spec_pk must be an R object (e.g., pk_extended), not a file path.
# If your metadata is in a file, load it first and then pass the object.
ft_with_spec <- model_report(
  model_names = c("run10"),
  models_folder = "models",
  spec_pk = pk_extended
)
ft_with_spec
```

------------------------------------------------------------------------

## 📈 Model Selection Criteria

**Model selected if BOTH**: 1. ΔOFV \> threshold 2. RSE \< threshold
(default: 50%) - In backward elimination, NA RSE is accepted

**Thresholds**: - Forward: ΔOFV \> 3.84 (p=0.05) - Backward: ΔOFV \>
6.63 (p=0.01)

------------------------------------------------------------------------

## ⚠️ Common Issues

### Base model validation fails

➜ Fix parameter format: `value ; NAME ; units ; RATIO|LOG`

### High RSE rejecting models

➜ Increase threshold: `rse_threshold = 100`

### Models failing

➜ Check: `results$search_database$estimation_issue`

### Need to resume

➜ Load checkpoint: `load_search_state("scm_auto_forward_complete.rds")`

------------------------------------------------------------------------

## 💾 Checkpoints (Auto-saved)

- `scm_auto_forward_complete.rds`
- `scm_auto_initial_backward_complete.rds`
- `scm_auto_final_backward_complete.rds`
- `scm_auto_final_complete.rds`

------------------------------------------------------------------------

## 🎨 Parameter Format Examples

### THETA (Population Parameters)

**IMPORTANT**: Use simple names or numbered format in `$THETA` block (NO
TV\_ prefix!)

    $THETA
    ; Option 1: Simple names (recommended)
    (0, 0.5, 100) ; CL ; L/h ; LOG
    (0, 10)       ; V  ; L   ; LOG
    0.5 FIX       ; KA ; 1/h ; LOG

    ; Option 2: Numbered format
    (0, 0.5, 100) ; 1_CL ; L/h ; LOG
    (0, 10)       ; 2_V  ; L   ; LOG
    0.5 FIX       ; 3_KA ; 1/h ; LOG

**Then in `$PK` block, use `TV_` prefix for typical values:**

    $PK
    TV_CL = THETA(1)  ; Use TV_ prefix here
    TV_V  = THETA(2)
    TV_KA = THETA(3)

    CL = TV_CL * EXP(ETA(1))
    V  = TV_V  * EXP(ETA(2))
    KA = TV_KA * EXP(ETA(3))

**✅ Correct THETA naming**: `CL`, `V`, `KA` or `1_CL`, `2_V`, `3_KA`\
**❌ Wrong THETA naming**: `TV_CL`, `TVCL`, `TVV` (don’t use `TV_` in
`$THETA` block!)

### OMEGA Diagonal

    $OMEGA
    0.1 ; IIV_CL ; ; RATIO
    0.1 ; IIV_V  ; ; RATIO

### OMEGA BLOCK (One value per line!)

    $OMEGA BLOCK(2)
    0.1 ; IIV_CL    ; ; LOG
    0.1 ; IIV_CL_V  ; ; RATIO
    0.1 ; IIV_V     ; ; LOG

### SIGMA

    $SIGMA
    0.1  ; RUV_PROP ; ; RATIO
    0.05 ; RUV_ADD  ; mg/L ; RATIO

**❌ WRONG** (multiple values per line):

    $OMEGA BLOCK(2)
    0.1 ; IIV_CL
    0.1 0.1 ; IIV_V      ← Don't do this!

**❌ WRONG** (TV\_ prefix in THETA block):

    $THETA
    0.5 ; TV_CL ; L/h ; LOG    <- Don't use TV_ in $THETA block!
    0.5 ; 1CL   ; L/h ; LOG     ← Missing underscore

------------------------------------------------------------------------

------------------------------------------------------------------------

## 🔧 Advanced Options

### Selective Forward (Faster)

``` r
scm_type = "selective"
```

### Custom Thresholds

``` r
forward_p_value = 0.01,    # More stringent
backward_p_value = 0.001,  # Very stringent
rse_threshold = 30,        # Stricter
require_cov_step = FALSE   # Skip covariance step check
```

### Resume Interrupted

``` r
search_state <- load_search_state("scm_auto_forward_complete.rds")
results <- run_backward_elimination(
  search_state,
  starting_model = "run85",
  rse_threshold = 50
)
```

------------------------------------------------------------------------

## 📚 Documentation

Full guide: `CovariateSearcher_USER_GUIDE.md` For detailed examples and
advanced usage, see the [complete
documentation](https://ollegst.github.io/CovariateSearcher/).

Help in R:

``` r
?initialize_covariate_search
?run_automated_scm_testing
?validate_base_model_parameters
```

------------------------------------------------------------------------

**Quick Start**: Initialize → Run → Check Results
