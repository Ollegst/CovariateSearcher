# =============================================================================
# NONMEM EXT FILE UTILITIES
# File: R/ext-file-utilities.R
# Part of CovariateSearcher Package
# Functions for reading and visualizing NONMEM .ext files
# =============================================================================

# Global variables declaration to avoid R CMD check notes
utils::globalVariables(c("ITERATION", "TYPE", "value", "variable"))


#' Read NONMEM Extended Output File
#'
#' @description Reads and formats a NONMEM extended output (.ext) file containing
#'   iteration history, objective function values, and parameter estimates.
#'   Handles multiple estimation steps and different iteration types (BURN, ITER, FINAL).
#' @param ext_file Character. Path to the .ext file
#' @return Data frame with columns:
#'   \itemize{
#'     \item ITERATION - Iteration number
#'     \item Parameter columns - THETA, OMEGA, SIGMA values
#'     \item OBJ - Objective function value
#'     \item EST.NO - Estimation step number
#'     \item EST.NAME - Estimation step name/description
#'     \item TYPE - Iteration type (ITER, BURN, FINAL, SE, EIGEN, CONDNUM)
#'     \item EVALUATION - Logical, TRUE if evaluation step
#'   }
#'   Returns empty data frame if file doesn't exist or is incomplete
#' @examples
#' \dontrun{
#' # Read ext file for run123
#' ext_data <- read_ext_iterations("models/run123/run123.ext")
#'
#' # Check final estimates
#' final_estimates <- ext_data[ext_data$TYPE == "FINAL", ]
#' }
#' @export
read_ext_iterations <- function(ext_file) {

  # Check file existence
  if (!file.exists(ext_file)) {
    return(data.frame())
  }

  # Read entire file
  file_content <- scan(ext_file, what = "character", sep = "\n", quiet = TRUE)

  # Find table markers
  table_rows <- grep("TABLE", file_content)
  if (length(table_rows) == 0) {
    # File is incomplete or has no tables
    return(data.frame())
  }

  # Define cut points for each table section
  cut_points <- c(table_rows, length(file_content) + 1)

  # Extract and clean headings
  headings <- file_content[table_rows]
  headings <- gsub("^TABLE NO.\\s+[0-9]+:\\s", "", headings)
  headings <- gsub(": Goal.*", "", headings)

  # Process each table section
  table_list <- lapply(seq_along(table_rows), function(i) {

    # Check if section has content
    if ((cut_points[i] + 1) > (cut_points[i + 1] - 1)) {
      return(data.frame())
    }

    # Extract section content
    section_data <- file_content[(cut_points[i] + 1):(cut_points[i + 1] - 1)]

    # Parse as table using temporary connection
    temp_con <- file()
    writeLines(section_data, temp_con)
    df <- utils::read.table(temp_con, header = TRUE)
    close(temp_con)

    # Add metadata
    df$EST.NO <- i
    df$EST.NAME <- headings[i]

    # Standardize OBJ column name
    obj_cols <- grepl("OBJ$", names(df))
    if (sum(obj_cols) > 1) {
      stop("Multiple OBJ columns detected. Please check ext file format.")
    }
    if (any(obj_cols)) {
      names(df)[obj_cols] <- "OBJ"
      df$OBJ <- as.numeric(as.character(df$OBJ))
    }

    # Classify iteration types based on ITERATION value
    df$TYPE <- NA_character_
    df$TYPE[df$ITERATION >= 0] <- "ITER"
    df$TYPE[df$ITERATION > -1000000000 & df$ITERATION < 0] <- "BURN"
    df$TYPE[df$ITERATION == -1000000000] <- "FINAL"
    df$TYPE[df$ITERATION == -1000000001] <- "SE"
    df$TYPE[df$ITERATION == -1000000002] <- "EIGEN"
    df$TYPE[df$ITERATION == -1000000003] <- "CONDNUM"

    # Flag evaluation steps
    df$EVALUATION <- grepl("Evaluation", df$EST.NAME)

    return(df)
  })

  # Combine all tables
  combined_data <- do.call(rbind, table_list)
  return(combined_data)
}


#' Plot NONMEM Iteration Data
#'
#' @description Creates a multi-panel plot showing the trajectory of objective function
#'   and parameter estimates across iterations. Useful for diagnosing estimation
#'   problems, convergence issues, and parameter stability.
#' @param model_name Character. Name of the model (e.g., "run123")
#' @param models_dir Character. Directory containing model files (default: "models")
#' @param transform Logical. Apply transformations to parameters (default: TRUE)
#' @param skip_iterations Integer. Number of initial iterations to skip (default: 0)
#' @param obj_var Character. Variable to plot (default: "OBJ" for objective function)
#' @param max_iterations Integer. Maximum number of iterations to display (default: 100)
#' @return ggplot2 object with faceted plots showing parameter trajectories
#' @details
#' The function:
#' \itemize{
#'   \item Reads the .ext file for the specified model
#'   \item Filters to iteration data (ITER and BURN types)
#'   \item Removes fixed parameters (those that don't change)
#'   \item Adjusts BURN iteration numbers for continuous display
#'   \item Creates faceted plots for each parameter and OBJ
#' }
#' @examples
#' \dontrun{
#' # Plot iteration data for run123
#' p <- plot_nonmem_iterations("run123")
#' print(p)
#'
#' # Skip first 10 iterations and limit to 50 iterations
#' p <- plot_nonmem_iterations("run123", skip_iterations = 10, max_iterations = 50)
#'
#' # Use custom models directory
#' p <- plot_nonmem_iterations("run123", models_dir = "path/to/models")
#' }
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
#' @export
plot_nonmem_iterations <- function(model_name,
                                   models_dir = "models",
                                   transform = TRUE,
                                   skip_iterations = 0,
                                   obj_var = "OBJ",
                                   max_iterations = 100) {

  # Validate inputs
  if (!is.character(model_name) || length(model_name) != 1) {
    stop("model_name must be a single character string")
  }

  # Construct ext file path
  ext_file_path <- file.path(models_dir, model_name, paste0(model_name, ".ext"))

  # Check file existence
  if (!file.exists(ext_file_path)) {
    stop(sprintf("EXT file not found: %s", ext_file_path))
  }

  # Read ext file
  ext_data <- read_ext_iterations(ext_file_path)

  if (nrow(ext_data) == 0) {
    stop("No data found in ext file or file is incomplete")
  }

  # Filter to iteration data only
  TYPE <- NULL  # To avoid R CMD check notes
  ext_data <- ext_data[ext_data$TYPE %in% c("ITER", "BURN"), ]

  if (nrow(ext_data) == 0) {
    stop("No iteration data found in ext file")
  }

  # Add formatted estimation step names
  ext_data$EST.NAME2 <- paste("$EST", ext_data$EST.NO, ":", ext_data$EST.NAME, sep = " ")

  # Limit iterations if specified
  ITERATION <- NULL  # To avoid R CMD check notes
  ext_data <- dplyr::filter(ext_data, ITERATION < max_iterations)

  # Identify estimation steps (excluding evaluation steps)
  if (length(unique(ext_data$EST.NO)) > 1) {
    est_no <- max(unique(ext_data$EST.NO[!grepl("Eval", ext_data$EST.NAME)]))
  } else {
    est_no <- max(unique(ext_data$EST.NO))
  }

  # Filter to selected estimation step
  ext_data <- ext_data[ext_data$EST.NO %in% est_no, ]

  # Identify parameter columns
  param_names <- names(ext_data)[2:(match("OBJ", names(ext_data)) - 1)]

  # Remove fixed parameters (those that don't change)
  for (param in param_names) {
    if (length(unique(ext_data[[param]])) == 1) {
      ext_data[[param]] <- NULL
    }
  }

  # Adjust BURN iteration numbers for continuous display
  ext_data <- by(ext_data, ext_data$EST.NO, function(df) {
    # Skip initial iterations if requested
    df <- df[df$ITERATION >= min(df$ITERATION) + skip_iterations, ]

    # Handle BURN iterations
    if (!"BURN" %in% df$TYPE || sum(df$TYPE == "BURN") <= 1) {
      return(df)
    }

    # Adjust BURN iteration numbers to appear before main iterations
    burn_iterations <- sort(df$ITERATION[df$TYPE == "BURN"], decreasing = TRUE)
    max_burn <- burn_iterations[1]
    max_burn2 <- burn_iterations[2]
    burn_interval <- max_burn - max_burn2

    df$ITERATION[df$TYPE == "BURN"] <- df$ITERATION[df$TYPE == "BURN"] - max_burn - burn_interval

    return(df)
  })

  # Recombine data
  ext_data <- do.call(rbind, ext_data)

  # Update parameter names list after removing fixed parameters
  param_names <- names(ext_data)[names(ext_data) %in% param_names]
  plot_vars <- c("OBJ", param_names)

  # Reshape data for plotting
  plot_data <- tidyr::pivot_longer(
    ext_data,
    cols = plot_vars,  # Changed from all_of(plot_vars)
    names_to = "variable",
    values_to = "value"
  )

  # Set factor levels to control plot order
  plot_data$variable <- factor(plot_data$variable, levels = plot_vars)

  # Create plot
  # Declare variables to avoid R CMD check notes
  value <- variable <- NULL

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = ITERATION, y = value)) +
    ggplot2::geom_line(ggplot2::aes(colour = TYPE)) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste("Iteration Trajectory:", unique(ext_data$EST.NAME))) +
    ggplot2::labs(
      x = "Iteration",
      y = "Parameter Value",
      colour = "Iteration Type"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.background = ggplot2::element_rect(fill = "grey90"),
      strip.text = ggplot2::element_text(face = "bold")
    )

  return(p)
}
