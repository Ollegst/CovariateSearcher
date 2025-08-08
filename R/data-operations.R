# =============================================================================
# DATA OPERATIONS
# File: R/data-operations.R
# Part of CovariateSearcher Package
# Data loading and validation operations
# =============================================================================



#' Load Search Data
#' @param data_file_path Character path to data file
#' @param covariate_search_path Character path to covariate search file
#' @return List with data components
#' @export
load_search_data <- function(data_file_path, covariate_search_path) {
  data_file <- readr::read_csv(data_file_path, show_col_types = FALSE)
  covariate_search <- readr::read_csv(covariate_search_path, show_col_types = FALSE)
  tags_path <- file.path(dirname(dirname(data_file_path)), "spec", "tags.yaml")
  tags <- if (file.exists(tags_path)) yaml::read_yaml(tags_path) else list()
  return(list(data_file = data_file, covariate_search = covariate_search, tags = tags))
}



#' Load Search Configuration
#' @return List with configuration
#' @export
load_search_config <- function() {
  list(forward_ofv_threshold = 3.84, max_rse_threshold = 50, threads = 4)
}



#' Validate Search Inputs
#' @param base_model_path Character base model path
#' @param data_file_path Character data file path
#' @param covariate_search_path Character covariate search path
#' @return Logical TRUE if valid
#' @export
validate_search_inputs <- function(base_model_path, data_file_path, covariate_search_path) {
  if (!file.exists(data_file_path)) stop("Data file not found: ", data_file_path)
  if (!file.exists(covariate_search_path)) stop("Covariate search file not found: ", covariate_search_path)
  return(TRUE)
}

