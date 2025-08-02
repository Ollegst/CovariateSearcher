# =============================================================================
# PACKAGE IMPORTS - GLOBAL IMPORT STATEMENTS
# File: R/00-imports.R
# Part of CovariateSearcher Package
# =============================================================================

#' Package Imports
#'
#' @description Global imports for CovariateSearcher package
#' @importFrom dplyr %>% mutate arrange filter select group_by summarise bind_rows case_when count n desc
#' @importFrom tibble tibble
#' @importFrom bbr read_model copy_model_from submit_model add_tags
#' @importFrom yaml read_yaml
#' @importFrom readr read_csv read_lines
#' @importFrom purrr map
#' @importFrom stringr str_detect str_extract
#' @importFrom stats median
#' @importFrom utils head tail
#' @name imports
NULL

# Define %||% operator for NULL coalescing
#' NULL-coalescing operator
#'
#' @name null-coalesce
#' @rdname null-coalesce
#' @param x Left-hand side
#' @param y Right-hand side
#' @return x if not NULL, otherwise y
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x

# Suppress R CMD check warnings for dplyr column names
utils::globalVariables(c(
  # Database column names
  "model_name", "parent_model", "status", "ofv", "delta_ofv", "rse_max",
  "step_description", "step_number", "covariate_tested", "submission_time",
  "completion_time", "runtime_minutes", "cov_to_test",

  # Generated variables in dplyr chains
  "parent_display", "model_type", "changes", "status_display", "ofv_display",
  "delta_display", "param_display", "delta_ofv_significant", "rse_acceptable",
  "completed", "total", "best_delta_ofv"
))
