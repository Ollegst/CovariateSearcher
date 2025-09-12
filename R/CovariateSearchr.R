# =============================================================================
# COVARIATESEARCHER PACKAGE
# Package-level documentation and imports
# =============================================================================

#' CovariateSearcher: Automated Stepwise Covariate Modeling for NONMEM
#'
#' @description
#' Functional R package for stepwise covariate modeling with NONMEM integration
#' via the bbr package. Provides automated covariate selection, model execution,
#' progress monitoring, and intelligent error recovery.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{initialize_covariate_search}}: Initialize search state
#'   \item \code{\link{run_stepwise_covariate_modeling}}: Run complete SCM
#'   \item \code{\link{add_covariate_to_model}}: Add single covariate
#'   \item \code{\link{remove_covariate_from_model}}: Remove single covariate
#' }
#'
#' @docType package
#' @name CovariateSearcher-package
#' @aliases CovariateSearcher
#'
#' @importFrom dplyr %>% mutate arrange filter select group_by summarise bind_rows case_when count n desc pull
#' @importFrom tibble tibble
#' @importFrom bbr read_model copy_model_from submit_model add_tags model_summary param_estimates
#' @importFrom yaml read_yaml
#' @importFrom readr read_csv read_lines
#' @importFrom purrr map map_chr map_dfr keep reduce
#' @importFrom stringr str_detect str_extract str_trim str_remove regex
#' @importFrom stats median setNames
#' @importFrom utils head tail
#' @importFrom data.table data.table
#' @importFrom flextable flextable set_flextable_defaults border_remove border_inner border_outer hline_bottom bold fix_border_issues fontsize font align set_table_properties autofit merge_v save_as_docx
#' @importFrom officer fp_border
#' @importFrom tidyr separate
#' @importFrom rlang parse_expr
#' @importFrom dplyr first if_else where
#' @importFrom rlang := !!
NULL


# In CovariateSearchr.R file, update the globalVariables section:

# Suppress R CMD check warnings for dplyr column names
utils::globalVariables(c(
  # Database column names
  "model_name", "parent_model", "status", "ofv", "delta_ofv", "rse_max",
  "step_description", "step_number", "covariate_tested", "submission_time",
  "completion_time", "runtime_minutes", "cov_to_test", "estimation_issue",
  "original_model", "best_model", "get_model_ofv",

  # Generated variables in dplyr chains
  "parent_display", "model_type", "changes", "status_display", "ofv_display",
  "delta_display", "param_display", "delta_ofv_significant", "rse_acceptable",
  "completed", "total", "best_delta_ofv", "retry_model", "exclusion_reason",

  # From extract_params function
  "line", "param", "transformation", "trans", "field1", "field3",

  # From get_param2 function
  "random_effect_sd", "random_effect_sdse", "parameter_names",
  "Parameter", "RSE", "SHRINKAGE", "fixed", "label", "model",
  "diag", "estimate", "stderr", "shrinkage", "comment",

  # From model_report function
  "comment_info", "group",

  # From submit_and_wait_for_step
  "model_num"
))


