CovariateSearcher: Quick Reference
================
CovariateSearcher Package
2025-11-06

## 🚀 Minimal Working Example

``` r
library(CovariateSearcher)

# 1. Initialize
search_state <- initialize_covariate_search(
  base_model_path = "models/run10",
  data_file_path = "data/analysis.csv",
  covariate_search_path = "data/covariates.csv"
)

# 2. Run SCM
results <- run_automated_scm_testing(
  search_state = search_state,
  base_model_id = "run10",
  scm_type = "standard",
  starting_phase = "forward",
  full_scm = TRUE
)

# 3. Check results
print(results$final_model)
print(results$final_covariates)
```

------------------------------------------------------------------------

## 📋 Required Files

### 1. Base Model

`models/run10/run10.ctl` - NONMEM control file

**Requirements**:

    $THETA
    ; Typical values: use TV_ prefix or numbers
    0.5 ; TV_CL ; L/h ; LOG
    10  ; TV_V  ; L   ; LOG
    ; OR
    0.5 ; 1_CL ; L/h ; LOG
    10  ; 2_V  ; L   ; LOG

    $OMEGA BLOCK(2)
    0.1 ; IIV_CL ; ; RATIO
    0.1 ; IIV_V2 ; ; RATIO

    $SIGMA
    0.1 ; RUV ; ; RATIO

Format: `value ; NAME ; units ; RATIO|LOG`

**THETA Naming**: Must use `TV_` prefix (e.g., `TV_CL`, `TV_V`) or
numbers (e.g., `1_CL`, `2_V`)

### 2. Dataset

`data/analysis.csv` - NONMEM dataset with covariates

### 3. Covariate Search

`data/covariates.csv`

``` csv
PARAMETER,COVARIATE,STATUS,FORMULA,LEVELS,REFERENCE,TIME_DEPENDENT
CL,WT,continuous,linear,NA,NA,FALSE
CL,AGE,continuous,linear,NA,NA,FALSE
CL,SEX,categorical,linear,2,1,FALSE
V2,WT,continuous,linear,NA,NA,FALSE
```

------------------------------------------------------------------------

## 🎯 Main Functions

### Initialize

``` r
search_state <- initialize_covariate_search(
  base_model_path,
  data_file_path,
  covariate_search_path
)
```

### Run Complete SCM

``` r
results <- run_automated_scm_testing(
  search_state,
  base_model_id,
  scm_type = "standard",      # or "selective"
  starting_phase = "forward", # or "backward"
  full_scm = TRUE
)
```

### Validate Base Model

``` r
validate_base_model_parameters("models/run10")
```

### Save/Load State

``` r
save_search_state(search_state, "state.rds")
search_state <- load_search_state("state.rds")
```

------------------------------------------------------------------------

## 📊 Workflow Types

### Standard SCM

``` r
starting_phase = "forward"
full_scm = TRUE
```

➜ Forward → Backward

### SCM+

``` r
starting_phase = "backward"
full_scm = TRUE
```

➜ Backward → Forward → Backward

### Forward Only

``` r
starting_phase = "forward"
full_scm = FALSE
```

➜ Forward only

------------------------------------------------------------------------

## ⚙️ Common Parameters

| Parameter          | Default    | Options                 |
|--------------------|------------|-------------------------|
| `scm_type`         | “standard” | “standard”, “selective” |
| `starting_phase`   | “forward”  | “forward”, “backward”   |
| `full_scm`         | TRUE       | TRUE, FALSE             |
| `forward_p_value`  | 0.05       | 0.001-0.1               |
| `backward_p_value` | 0.01       | 0.001-0.1               |
| `rse_threshold`    | 50         | 30-100                  |
| `auto_submit`      | TRUE       | TRUE, FALSE             |
| `auto_retry`       | TRUE       | TRUE, FALSE             |

------------------------------------------------------------------------

## 🔍 Check Results

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
```

------------------------------------------------------------------------

## 📈 Model Selection Criteria

**Model selected if BOTH**: 1. ΔOFV \> threshold 2. RSE \< threshold
(default: 50%)

**Thresholds**: - Forward: ΔOFV \> 3.84 (p=0.05) - Backward: ΔOFV \>
6.63 (p=0.01)

------------------------------------------------------------------------

## ⚠️ Common Issues

### Base model validation fails

➜ Fix parameter format: `value ; NAME ; units ; RATIO|LOG`

### High RSE rejecting models

➜ Increase threshold: `rse_threshold = 100`

### Models failing

➜ Check: `search_state$search_database$estimation_issue`

### Need to resume

➜ Load checkpoint: `load_search_state("scm_auto_forward_complete.rds")`

------------------------------------------------------------------------

## 💾 Checkpoints (Auto-saved)

-   `scm_auto_forward_complete.rds`
-   `scm_auto_initial_backward_complete.rds`
-   `scm_auto_final_backward_complete.rds`
-   `scm_auto_final_complete.rds`

------------------------------------------------------------------------

## 🎨 Parameter Format Examples

### THETA (Typical Values)

**IMPORTANT**: Use `TV_` prefix or number prefix

    $THETA
    ; Recommended: TV_ prefix
    (0, 0.5, 100) ; TV_CL ; L/h ; LOG
    (0, 10)       ; TV_V  ; L   ; LOG
    0.5 FIX       ; TV_KA ; 1/h ; LOG

    ; Alternative: Number prefix
    (0, 0.5, 100) ; 1_CL ; L/h ; LOG
    (0, 10)       ; 2_V  ; L   ; LOG
    0.5 FIX       ; 3_KA ; 1/h ; LOG

**✅ Correct**: `TV_CL`, `TV_V`, `1_CL`, `2_V`  
**❌ Wrong**: `TVCL`, `CL`, `TVV` (missing underscore or prefix)

### OMEGA Diagonal

    $OMEGA
    0.1 ; IIV_CL ; ; RATIO
    0.1 ; IIV_V  ; ; RATIO

### OMEGA BLOCK (One value per line!)

    $OMEGA BLOCK(3)
    0.1 ; IIV_CL    ; ; RATIO
    0.1 ; IIV_CL_V2 ; ; RATIO
    0.1 ; IIV_V2    ; ; RATIO

### SIGMA

    $SIGMA
    0.1  ; RUV_PROP ; ; RATIO
    0.05 ; RUV_ADD  ; mg/L ; RATIO

**❌ WRONG** (multiple values per line):

    $OMEGA BLOCK(3)
    0.1 ; IIV_CL
    0.1 0.1 ; IIV_V2      ← Don't do this!

**❌ WRONG** (THETA naming):

    $THETA
    0.5 ; TVCL ; L/h ; LOG    ← Missing underscore
    0.5 ; CL ; L/h ; LOG      ← Missing TV_ or number prefix

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
rse_threshold = 30         # Stricter
```

### Resume Interrupted

``` r
search_state <- load_search_state("scm_auto_forward_complete.rds")
results <- run_backward_elimination(
  search_state,
  starting_model = "run85"
)
```

------------------------------------------------------------------------

## 📚 Documentation

Full guide: `CovariateSearcher_USER_GUIDE.md`

Help in R:

``` r
?initialize_covariate_search
?run_automated_scm_testing
?validate_base_model_parameters
```

------------------------------------------------------------------------

**Quick Start**: Initialize → Run → Check Results
