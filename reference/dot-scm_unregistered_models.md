# On-disk SCM models not yet in the search database

Model directories (`run<N>`) present in the models folder but absent
from `search_state$search_database`. Retry models (`run<N>001`) are
excluded, as in
[`update_model_counter()`](https://ollegst.github.io/CovariateSearcher/reference/update_model_counter.md).
A directory with no `run<N>.yaml` beside it is not a bbr model (a
leftover output folder, a model whose files were deleted): it cannot be
registered, so it is reported and left out rather than read.

## Usage

``` r
.scm_unregistered_models(search_state, models_folder = NULL)
```
