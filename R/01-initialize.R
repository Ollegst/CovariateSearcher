#' @title Initialize Functions
#' @description Package initialization and state creation functions
#' @name initialize-functions
NULL

#' Initialize CovariateSearcher State
#'
#' @param base_model_path Character. Path to base NONMEM model
#' @param data_file_path Character. Path to dataset file
#' @param covariate_search_path Character. Path to covariate search definition
#' @param models_folder Character. Directory for NONMEM models
#' @param threads Integer. Number of threads for parallel execution
#' @return List containing initialized search state
#' @export
initialize_covariate_search <- function(base_model_path, data_file_path, covariate_search_path,
                                       models_folder = "models", threads = 4) {

  # Validate inputs
  validate_search_inputs(base_model_path, data_file_path, covariate_search_path)

  # Load data
  search_data <- load_search_data(data_file_path, covariate_search_path)

  # Initialize database
  search_db <- initialize_search_database(models_folder)

  # Load configuration
  config <- load_search_config()

  # Create search state
  search_state <- list(
    base_model = base_model_path,
    data_file = search_data$data_file,
    covariate_search = search_data$covariate_search,
    tags = search_data$tags,
    search_database = search_db$database,
    search_config = config,
    model_counter = search_db$counter,
    models_folder = models_folder,
    threads = threads
  )

  class(search_state) <- c("covariate_search_state", "list")
  return(search_state)
}
