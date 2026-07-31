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


# Work out, from the control stream's own annotations, what each .ext column
# should be called and which ones are on the log scale. Both are best-effort: a
# model that annotates nothing keeps the .ext's names and values.
#
# Annotations are matched to columns BY POSITION, which is only sound when the
# counts agree - extract_params() drops a record line carrying no ';' comment,
# and one dropped line shifts every name after it onto the wrong parameter. Each
# record is therefore checked on its own and skipped whole on a mismatch, so a
# fully annotated $THETA still gets names when a BLOCK() $OMEGA cannot.
#
# Only THETAs are rescaled. A ';LOG' theta is stored as log(value) and is
# meaningless plotted raw; OMEGA and SIGMA are variances and are shown as
# NONMEM wrote them.
#' @keywords internal
#' @noRd
.listing_column_meta <- function(ext_cols, model_name, models_dir) {

  labels   <- stats::setNames(ext_cols, ext_cols)
  log_cols <- character(0)

  # suppressWarnings: extract_params() warns from an internal min() whenever the
  # control stream's last record is $THETA/$OMEGA/$SIGMA. Upstream noise, not
  # something the caller of a plot can act on.
  params <- tryCatch(
    suppressWarnings(extract_model_params(model_name, models_folder = models_dir)),
    error = function(e) NULL
  )
  if (is.null(params)) {
    warning(sprintf(
      paste("Control stream for '%s' could not be read; panels keep the .ext's",
            "own names and values."), model_name), call. = FALSE)
    return(list(labels = labels, log_cols = log_cols))
  }

  # NONMEM's own column order, not alphabetical: THETA10 follows THETA9.
  theta_pat  <- "^THETA0*(\\d+)$"
  theta_cols <- grep(theta_pat, ext_cols, value = TRUE)
  theta_cols <- theta_cols[order(as.integer(sub(theta_pat, "\\1", theta_cols)))]

  # read.table sanitises the .ext header, so OMEGA(1,1) arrives as OMEGA.1.1.
  # Only the diagonal has a $OMEGA/$SIGMA line to be named from.
  diagonal_of <- function(block) {
    pat <- sprintf("^%s[.(](\\d+)[,.](\\d+)[.)]$", block)
    hit <- grep(pat, ext_cols, value = TRUE)
    if (length(hit) == 0L) return(character(0))
    hit <- hit[sub(pat, "\\1", hit) == sub(pat, "\\2", hit)]
    hit[order(as.integer(sub(pat, "\\1", hit)))]
  }

  records <- list(
    list(cols = theta_cols,           tbl = params$THETAS, tag = "$THETA"),
    list(cols = diagonal_of("OMEGA"), tbl = params$OMEGAS, tag = "$OMEGA"),
    list(cols = diagonal_of("SIGMA"), tbl = params$SIGMA,  tag = "$SIGMA")
  )

  for (rec in records) {
    n_annotated <- if (is.null(rec$tbl)) 0L else nrow(rec$tbl)
    if (length(rec$cols) == 0L || n_annotated == 0L) next

    if (n_annotated != length(rec$cols)) {
      warning(sprintf(
        paste("'%s' has %d annotated %s line(s) for %d .ext column(s), so those",
              "panels keep their .ext names and values. Annotate every line as",
              "'; NAME ; UNIT ; TRANS' to name them."),
        model_name, n_annotated, rec$tag, length(rec$cols)), call. = FALSE)
      next
    }

    labels[rec$cols] <- rec$tbl$param
    if (identical(rec$tag, "$THETA")) {
      log_cols <- c(log_cols, rec$cols[!is.na(rec$tbl$trans) & rec$tbl$trans == "LOG"])
    }
  }

  # Two panels sharing a label would be collapsed into one by the facet, so any
  # name used more than once falls back to the .ext column it came from.
  repeated <- labels %in% labels[duplicated(labels)]
  labels[repeated] <- ext_cols[repeated]

  list(labels = labels, log_cols = log_cols)
}


#' Plot NONMEM Iteration Data
#'
#' @description Creates a multi-panel plot showing the trajectory of objective function
#'   and parameter estimates across iterations. Useful for diagnosing estimation
#'   problems, convergence issues, and parameter stability.
#' @param model_name Character. Name of the model (e.g., "run123")
#' @param models_dir Character. Directory containing model files (default: "models")
#' @param transform Logical. Use whatever the control stream declares about its
#'   own parameters (default: TRUE). Each panel is titled with the parameter's
#'   name from its \code{$THETA}/\code{$OMEGA}/\code{$SIGMA} comment, and a THETA
#'   annotated \code{;LOG} is exponentiated so it is plotted on the scale it is
#'   reported on. \code{;RATIO} and unannotated THETAs are plotted as estimated,
#'   and OMEGA/SIGMA are always shown as NONMEM wrote them. Anything the model
#'   does not declare falls back to the .ext, so an unannotated record keeps
#'   names like \code{THETA1} and \code{OMEGA.1.1.}. \code{FALSE} plots the .ext
#'   exactly as written - raw values and raw column names.
#' @param skip_iterations Integer. Number of initial iterations to skip (default: 0)
#' @param obj_var Character. Column plotted in the leading panel (default: "OBJ").
#'   Must name a column of the .ext file; \code{read_ext_iterations()} standardises
#'   any objective column to \code{"OBJ"}, so the default fits every estimation method.
#' @param max_iterations Integer. Upper bound on the iteration NUMBER displayed
#'   (keeps \code{ITERATION < max_iterations}), not a count of points (default: 100)
#' @return ggplot2 object with faceted plots showing parameter trajectories
#' @details
#' The function:
#' \itemize{
#'   \item Reads the .ext file for the specified model
#'   \item Filters to iteration data (ITER and BURN types)
#'   \item Keeps a single estimation step - the highest-numbered \code{$EST}
#'     that is not an evaluation step
#'   \item Removes fixed parameters (those that don't change)
#'   \item Adjusts BURN iteration numbers for continuous display
#'   \item Names and rescales parameters when \code{transform = TRUE}
#'   \item Creates faceted plots for each parameter and \code{obj_var}
#' }
#'
#' \code{transform = TRUE} matches the control stream's annotations to the .ext
#' columns by position, the same assumption \code{model_report()} makes. Every
#' line of a record therefore needs its \code{; NAME ; UNIT ; TRANS} comment: a
#' line without one is not parsed at all, and would shift every name after it
#' onto the wrong parameter. Each record is checked on its own and skipped whole
#' when the counts disagree, so a fully annotated \code{$THETA} is still named
#' when a \code{BLOCK()} \code{$OMEGA} cannot be.
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

  if (!is.character(obj_var) || length(obj_var) != 1 || !obj_var %in% names(ext_data)) {
    stop(sprintf(
      "obj_var '%s' is not a column of %s. Available columns: %s",
      paste(obj_var, collapse = ", "),
      basename(ext_file_path),
      paste(names(ext_data), collapse = ", ")
    ))
  }

  if (!is.logical(transform) || length(transform) != 1 || is.na(transform)) {
    stop("transform must be TRUE or FALSE")
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

  # Identify parameter columns. OBJ marks the boundary between the parameter
  # columns and the metadata the reader appends, whichever column obj_var plots.
  param_names <- names(ext_data)[2:(match("OBJ", names(ext_data)) - 1)]
  param_names <- setdiff(param_names, obj_var)

  # Captured from the whole .ext, before constant columns are dropped and
  # regardless of obj_var, so the per-record counts match what the control
  # stream declares rather than what survived to be plotted.
  ext_cols <- names(ext_data)

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
  plot_vars <- c(obj_var, param_names)

  var_labels <- stats::setNames(plot_vars, plot_vars)
  if (transform) {
    meta <- .listing_column_meta(ext_cols, model_name, models_dir)

    # Only columns that survived to be plotted; a THETA promoted to the leading
    # panel by obj_var is rescaled and named like any other.
    for (nm in intersect(meta$log_cols, plot_vars)) {
      ext_data[[nm]] <- exp(ext_data[[nm]])
    }
    var_labels[plot_vars] <- meta$labels[plot_vars]
  }

  # Reshape data for plotting
  plot_data <- tidyr::pivot_longer(
    ext_data,
    cols = plot_vars,  # Changed from all_of(plot_vars)
    names_to = "variable",
    values_to = "value"
  )

  # Set factor levels to control plot order; labels carry the applied scale
  plot_data$variable <- factor(
    plot_data$variable,
    levels = plot_vars,
    labels = unname(var_labels[plot_vars])
  )

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
