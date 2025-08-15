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
#' @importFrom bbr read_model copy_model_from submit_model add_tags
#' @importFrom yaml read_yaml
#' @importFrom readr read_csv read_lines
#' @importFrom purrr map
#' @importFrom stringr str_detect str_extract
#' @importFrom stats median
#' @importFrom utils head tail
NULL


# In CovariateSearchr.R file, update the globalVariables section:

# Suppress R CMD check warnings for dplyr column names
utils::globalVariables(c(
  # Database column names
  "model_name", "parent_model", "status", "ofv", "delta_ofv", "rse_max",
  "step_description", "step_number", "covariate_tested", "submission_time",
  "completion_time", "runtime_minutes", "cov_to_test", "estimation_issue",
  "original_model","best_model",

  # Generated variables in dplyr chains
  "parent_display", "model_type", "changes", "status_display", "ofv_display",
  "delta_display", "param_display", "delta_ofv_significant", "rse_acceptable",
  "completed", "total", "best_delta_ofv", "retry_model", "exclusion_reason"
))
