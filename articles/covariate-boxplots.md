# Covariate Boxplots

## Covariate Boxplots

Exposure in the **actual study population**, split by covariate group:
continuous covariates binned into quartiles, categorical ones by decoded
level, with an optional `Total` panel pooling everyone.

This is a different question from the forest plots. A forest plot varies
one covariate at a time around a typical subject, using parameter
*uncertainty*. A boxplot shows the exposure the real subjects actually
had, with their own parameters, covariates and dose.

    individual parameters + real dosing -> simulate -> exposure metrics per subject
                                                    -> join covariates -> boxplots

------------------------------------------------------------------------

### Inputs

The same setup as the [forest
plots](https://ollegst.github.io/CovariateSearcher/articles/forest-plots.md)
article:

``` r

library(CovariateSearcher)
library(mrgsolve)
library(yspec)     # ys_load, pull_meta
library(yaml)      # read_yaml
library(here)      # here()
library(dplyr)
library(data.table)

## The covariate search table the model was built from
covariate_search <- read.csv("./data/derived/covariate_search.csv")

## Two spec files:
##   lookup.yml    - covariate names, units and level decodes -> axis labels
##   pk-extend.yml - PK parameter labels and units            -> param_info
lookup  <- ys_load(here("data", "spec", "lookup.yml"))
spec_pk <- read_yaml(here("data", "spec", "pk-extend.yml"))

## Analysis dataset, reduced to the modelling population and one row per
## subject - it supplies both each subject's dose regimen and their baseline
## covariates
dat <- read.csv("./data/derived/analysis.csv", stringsAsFactors = FALSE) %>%
  filter(EXFLG == 0) %>%
  group_by(ID) %>%
  slice(1)

## Decode categorical covariates to their labels - required here, the boxplots
## stop on raw numeric codes
flags <- pull_meta(lookup, "flags")
dat   <- decode_dataset(dat, lookup, flags$diagCatCov)

model_folder <- "models"
runno        <- "run19"

set.seed(12345)
```

------------------------------------------------------------------------

### 1. Simulate the population

Each subject is simulated with **their own post-hoc parameters** and
**their own dosing regimen**, which is why this uses
[`idata_set()`](https://mrgsolve.org/docs/reference/idata_set.html)
(parameters per ID) with
[`data_set()`](https://mrgsolve.org/docs/reference/data_set.html)
(events per ID) rather than a single common dose.

``` r

## mrgsolve model - structural only, parameters named THETA1..THETAn
mod <- mread(file.path(model_folder, paste0(runno, ".cpp")))

## Individual post-hoc parameters from the NONMEM $TABLE output.
## One row per subject; rename the model's parameter columns to the THETA
## names the mrgsolve model exposes in $PARAM.
patab <- read.table(file.path(model_folder, runno,
                              paste0("patab", sub("^run", "", runno))),
                    skip = 1, header = TRUE) %>%
  group_by(ID) %>%
  slice(1) %>%
  select(ID, THETA1 = KA, THETA2 = V1, THETA3 = CL)

## Each subject's regimen - `dat` is already one row per subject
dose_rec <- dat %>%
  select(ID, amt = DOSE, ii = REG) %>%         # REG = dosing frequency flag
  mutate(ii = ifelse(ii == 2, 24, 12))         # translate the flag to hours

## Steady-state dose records, one per subject
ev1 <- data.table(
  time = rep(0, nrow(dose_rec)),
  amt  = dose_rec$amt,
  evid = 1,
  cmt  = rep(1, nrow(dose_rec)),
  addl = rep(1, nrow(dose_rec)),
  ii   = dose_rec$ii,
  ss   = rep(1, nrow(dose_rec)),               # start at steady state
  ID   = dose_rec$ID
)

## Simulate one dosing interval
out <- mod %>%
  data_set(ev1) %>%                            # events per subject
  idata_set(patab) %>%                         # parameters per subject
  mrgsim(end = 24, delta = 0.1) %>%
  as.data.frame()

## Reduce to one row per subject
sim_start_dose <- out %>%
  mutate(Scenario = "Starting dose") %>%
  group_by(Scenario, ID) %>%
  summarise(
    AUC  = auc_partial(time, CP),              # your AUC helper
    Cmax = max(CP),
    Cmin = last(CP),
    .groups = "drop"
  )

saveRDS(sim_start_dose,
        paste0("data/derived/exposure_metrics_", runno, "_starting_dose.rds"))
```

Because the parameters come from `patab`, between-subject variability is
already in the numbers - the model must not add it again (no `$OMEGA`,
no `ETA`), exactly as for the forest simulations.

------------------------------------------------------------------------

### 2. Build the plotting dataset

[`create_covariate_boxplots()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_boxplots.md)
needs **one row per subject holding both the exposure metrics and the
covariates**, so join the metrics saved in step 1 onto the dataset:

``` r

res <- readRDS(paste0("data/derived/exposure_metrics_", runno, "_starting_dose.rds")) %>%
  left_join(., dat, by = "ID")
```

`dat` was decoded in the setup chunk, so the categorical covariates
already carry their labels - the boxplots stop on raw numeric codes.
`flags` comes from there too, which is why the plot call below passes
`flags$contcov` / `flags$catcov` instead of listing covariates by hand.

------------------------------------------------------------------------

### 3. Plot

``` r

compound_name <- "Drug X"         # used in titles AND file names

results <- create_covariate_boxplots(
  data          = res,            # metrics + covariates, one row per subject
  spec          = lookup,         # axis labels: "Short (unit)"
  con           = flags$contcov,  # continuous covariates, straight from the spec
  cat           = flags$catcov,   # categorical covariates
  type          = c("AUC", "Cmax", "Cmin"),   # or just "AUC"
  drug          = compound_name,
  param_info    = spec_pk,        # labels/units per metric, from pk-extend.yml
  ss            = TRUE,           # steady state: "ss" on the y-axis metric label
                                  # and "_ss" on every file name, so a single-dose
                                  # run never overwrites these. Labels that already
                                  # end in "ss" are left alone
  total_panel   = FALSE,          # TRUE adds a right-most panel pooling everyone
  output_folder = "results/figure/simulations"
)

results$AUC[[1]]                  # results[[type]][[covariate]] = a ggplot
```

Other options: `stratification` splits into side-by-side panels
(e.g. `"COMB"`), `combined_pdf = TRUE` also writes one multi-page PDF,
`prefix` prepends to every file name, `output_format` picks
`"emf"`/`"png"`, `base_size` sets the font, and `width`/`height`
override the automatic sizing when **both** are supplied.

#### Box annotations

Each box carries its median on the median line and `n = <count>`
underneath. Both are optional, and the median can also show the change
from the first level:

``` r

create_covariate_boxplots(
  ...,
  show_median    = TRUE,    # the median label on each box
  show_n         = TRUE,    # "n = 42" under each box
  percent_change = TRUE,    # median label becomes e.g. 900 (-10%)
  label_size     = 1.8      # shrink the label where boxes are narrow;
                            # NULL (default) derives it from base_size
)
```

`percent_change` compares each box with the **first level of the same
panel**:

| box            | median | label          |
|----------------|--------|----------------|
| Q1 (reference) | 1000   | `1,000`        |
| Q2             | 900    | `900 (-10%)`   |
| Q3             | 1200   | `1,200 (+20%)` |
| Q4             | 1500   | `1,500 (+50%)` |

The first level is the reference so it carries no suffix, and the pooled
`Total` panel holds a single box so it gets none either.

`param_info` supplies the label and unit for each entry of `type`.
Reading them from `pk-extend.yml` keeps them in one place - each entry’s
`short` becomes the label, and its `unit` the unit. A yspec object or a
path to the YAML works the same way.

Every entry of `type` must have a label there, or the call stops.

| argument | default | effect |
|----|----|----|
| `con`, `cat` | `NULL` | continuous and categorical covariate columns to plot |
| `type` | `c("AUC","Cmax","Cmin")` | metric columns of `data`; any numeric column works |
| `ss` | `TRUE` | steady state. Appends “ss” to the metric label on the y axis (`Drug X AUCss, mg*hr/L`) **and** `_ss` to every saved file name (`Drug X-WT-AUC_ss.emf`, `Drug X-AUC-Cmax-Cmin_ss-1405.pdf`), so single-dose and steady-state runs cannot overwrite each other. Nothing is appended where the label or file stem already ends in “ss” |
| `stratification` | `NULL` | column to split into side-by-side panels |
| `total_panel` | `TRUE` | add the pooled `Total` panel |
| `output_folder` | `"results/figure/simulations"` | where files are written |
| `output_format` | `c("emf","png")` | one file per format |
| `combined_pdf` | `TRUE` | additionally write every plot to one PDF |
| `prefix` | `NULL` | prepended to every output file name |
| `base_size` | `10` | ggplot base font size; also scales the `n =` text and, unless `label_size` is given, the median label |
| `width`, `height` | `NULL` | supply **both** to override the automatic sizing |
| `verbose` | `TRUE` | per-covariate progress line |

Returns a nested list, `results[[type]][[covariate]]`, of ggplot
objects - invisibly, since the files are written as a side effect.

------------------------------------------------------------------------

### See also

- [Forest Plots and
  Simulations](https://ollegst.github.io/CovariateSearcher/articles/forest-plots.md) -
  covariate effects around a typical subject
- [Complete
  Workflow](https://ollegst.github.io/CovariateSearcher/articles/complete-workflow.md)
