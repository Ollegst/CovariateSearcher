# =============================================================================
# VALIDATION
# File: R/validation.R
# Part of CovariateSearcher Package
# Model validation and quality assessment
# =============================================================================


#' Update Model Status from Files with Enhanced Error Detection and Force Flag
#'
#' @title Updates search database with results from NONMEM output files
#' @description Reads NONMEM output files and updates database. Includes force flag
#'   to control re-reading of completed models for efficiency.
#' @param search_state List. Current search state with database and configuration
#' @param model_name Character. Model name to update (e.g., "run11")
#' @param force Logical. If TRUE, forces re-reading even for completed models (default: FALSE)
#' @return List with updated search_state
#' @export
update_model_status_from_files <- function(search_state, model_name, force = FALSE) {

  # ===== SECTION 1: INPUT VALIDATION =====
  if (is.null(search_state) || is.null(model_name) || length(model_name) == 0) {
    cat("❌ Invalid input parameters\n")
    return(search_state)
  }

  # Validate search_state structure
  if (is.null(search_state$search_database) || is.null(search_state$models_folder)) {
    cat("❌ Invalid search_state structure\n")
    return(search_state)
  }

  # Validate required columns exist
  required_cols <- c("model_name", "status", "ofv", "estimation_issue",
                     "covariate_tested", "parent_model", "delta_ofv",
                     "submission_time", "completion_time", "step_number",
                     "excluded_from_step", "original_model", "rse_max")

  missing_cols <- setdiff(required_cols, names(search_state$search_database))
  if (length(missing_cols) > 0) {
    # Add missing columns with appropriate defaults using case_when logic
    for (col in missing_cols) {
      search_state$search_database[[col]] <- dplyr::case_when(
        col %in% c("ofv", "delta_ofv", "rse_max", "step_number") ~ NA_real_,
        col == "excluded_from_step" ~ FALSE,
        TRUE ~ NA_character_
      )
    }
  }

  # Find model in database
  db_idx <- which(search_state$search_database$model_name == model_name)
  if (length(db_idx) == 0) {
    cat(sprintf("⚠️  Model %s not found in database\n", model_name))
    return(search_state)
  }

  # ===== SECTION 2: CHECK IF UPDATE NEEDED =====
  current_status <- search_state$search_database$status[db_idx]
  current_ofv <- search_state$search_database$ofv[db_idx]

  # Skip re-reading if model is completed with OFV and force=FALSE
  if (!force &&
      !is.na(current_status) &&
      current_status == "completed" &&
      !is.na(current_ofv)) {
    # Model already complete with OFV, skip re-reading large files
    return(search_state)
  }

  # ===== SECTION 3: READ FILES AND EXTRACT INFORMATION =====
  model_path <- file.path(search_state$models_folder, model_name)

  # Helper function to read all files once
  read_all_model_files <- function() {
    results <- list(
      status = "unknown",
      ofv = NA_real_,
      rse_max = NA_real_,
      error_message = NA_character_,
      completion_time = NA
    )

    # Check LST file existence
    lst_file <- file.path(model_path, paste0(model_name, ".lst"))

    if (!file.exists(lst_file)) {
      # No LST file = still running
      results$status <- "in_progress"
      return(results)
    }

    # Read LST file - SIMPLIFIED: Only check Stop Time
    lst_info <- tryCatch({
      lst_content <- readLines(lst_file, warn = FALSE)

      # ONLY check if run has finished (Stop Time is standard NONMEM output)
      has_stop_time <- any(grepl("Stop Time:", lst_content))

      list(
        status = if (has_stop_time) "stopped" else "in_progress",
        lst_content = lst_content  # Keep for potential #TERM extraction
      )
    }, error = function(e) {
      list(status = "read_error", lst_content = NULL)
    })

    # If still running, return early
    if (lst_info$status == "in_progress") {
      results$status <- "in_progress"
      return(results)
    }

    # If LST read error, return early
    if (lst_info$status == "read_error") {
      results$status <- "failed"
      results$error_message <- "Cannot read LST file"
      return(results)
    }

    # Extract completion time (only if stopped)
    timestamps <- tryCatch({
      extract_nonmem_timestamps(model_name, search_state$models_folder)
    }, error = function(e) {
      list(stop_time = NA)
    })
    results$completion_time <- timestamps$stop_time

    # Extract OFV from EXT file - THIS DETERMINES FINAL STATUS
    ext_file <- file.path(model_path, paste0(model_name, ".ext"))

    if (!file.exists(ext_file)) {
      # No EXT file = failed
      results$status <- "failed"
      results$error_message <- "No EXT file found"
      results$ofv <- NA_real_
    } else {
      # Use read_nonmem_ext() instead of duplicating logic
      ext_results <- read_nonmem_ext(model_path)

      if (!ext_results$found) {
        # EXT file exists but couldn't be read
        results$status <- "failed"
        results$error_message <- ext_results$error
        results$ofv <- NA_real_
      } else {
        # Successfully read OFV
        ofv_value <- ext_results$ofv
        results$ofv <- ofv_value

        # SIMPLE STATUS DETERMINATION: Valid OFV = success, Invalid OFV = failed
        is_valid_ofv <- !is.na(ofv_value) && is.finite(ofv_value) && abs(ofv_value) <= 1e10

        # NEW: Check for parameters at boundary limits
        if (is_valid_ofv && !is.null(ext_results$parameters)) {
          # Check if any parameter is at boundary (±8.99990E+05)
          boundary_limit <- 8.99990e5
          param_values <- ext_results$parameters

          if (any(abs(param_values) >= boundary_limit, na.rm = TRUE)) {
            is_valid_ofv <- FALSE

            # Find which parameters are at boundary
            problem_indices <- which(abs(param_values) >= boundary_limit)
            if (length(problem_indices) > 0) {
              results$error_message <- paste("Parameters at boundary limits (positions:",
                                             paste(problem_indices, collapse = ", "), ")")
            } else {
              results$error_message <- "Parameters at boundary limits"
            }
          }
        }

        results$status <- if (is_valid_ofv) "completed" else "failed"

        # Set error message for failed runs
        if (!is_valid_ofv) {
          results$error_message <- dplyr::case_when(
            is.na(ofv_value) ~ "No OFV found in EXT file",
            is.infinite(ofv_value) ~ "Infinite OFV",
            is.nan(ofv_value) ~ "NaN OFV",
            abs(ofv_value) > 1e10 ~ sprintf("OFV > 10^10: %.2e", ofv_value),
            TRUE ~ "Invalid OFV"
          )
        }
      }
    }

    # For failed runs, extract #TERM message for diagnostics
    if (results$status == "failed" && !is.null(lst_info$lst_content)) {
      term_idx <- grep("#TERM:", lst_info$lst_content)
      if (length(term_idx) > 0) {
        # Get up to 3 lines after #TERM:
        term_start <- term_idx[1]
        term_end <- min(term_start + 3, length(lst_info$lst_content))
        if (term_end > term_start) {
          term_lines <- lst_info$lst_content[(term_start + 1):term_end]
          term_lines <- term_lines[nchar(trimws(term_lines)) > 0]
          if (length(term_lines) > 0) {
            term_message <- paste(term_lines, collapse = "; ")
            results$error_message <- paste(results$error_message,
                                           paste0("[#TERM: ", term_message, "]"),
                                           sep = " ")
          }
        }
      }
    }

    # Extract RSE (only for successful runs)
    if (results$status == "completed") {
      results$rse_max <- tryCatch({
        params <- get_param2(
          model_number = model_name,
          count_model = 1,
          models_folder = search_state$models_folder
        )

        rse_values <- params$RSE[!is.na(params$RSE) & is.finite(params$RSE)]
        if (length(rse_values) > 0) max(rse_values) else NA_real_
      }, error = function(e) {
        NA_real_
      })
    }

    return(results)
  }

  # Read all files once
  file_results <- read_all_model_files()

  # ===== SECTION 4: UPDATE DATABASE =====
  # ALWAYS update when force=TRUE or when new information is available
  if (force ||
      is.na(current_status) ||
      current_status != file_results$status ||
      (file_results$status == "completed" && is.na(current_ofv))) {

    # Update all fields
    search_state$search_database$status[db_idx] <- file_results$status
    search_state$search_database$ofv[db_idx] <- file_results$ofv
    search_state$search_database$rse_max[db_idx] <- file_results$rse_max
    search_state$search_database$estimation_issue[db_idx] <- file_results$error_message

    if (!is.na(file_results$completion_time)) {
      search_state$search_database$completion_time[db_idx] <- file_results$completion_time
    }
  }

  # ===== SECTION 5: CALCULATE DELTA OFV =====
  if (file_results$status == "completed" && !is.na(file_results$ofv)) {
    parent_model <- search_state$search_database$parent_model[db_idx]

    if (!is.na(parent_model) && nchar(parent_model) > 0) {
      parent_idx <- which(search_state$search_database$model_name == parent_model)

      if (length(parent_idx) > 0) {
        parent_ofv <- search_state$search_database$ofv[parent_idx[1]]

        if (!is.na(parent_ofv)) {
          # Get the action type to determine delta OFV sign convention
          action_type <- search_state$search_database$action[db_idx]

          # Calculate delta OFV with correct sign convention:
          # - For FORWARD (add_covariate): delta = parent - child
          #   Positive delta = child OFV is lower (improvement)
          # - For BACKWARD (remove_covariate): delta = child - parent
          #   Positive delta = child OFV is higher (removal worsened model = keep covariate)
          if (!is.na(action_type) &&
              (action_type == "remove_single_covariate" ||
               action_type == "remove_covariate" ||
               grepl("remove", action_type, ignore.case = TRUE))) {
            # BACKWARD: child - parent (positive = OFV increased = bad removal)
            delta_ofv <- file_results$ofv - parent_ofv
          } else {
            # FORWARD: parent - child (positive = OFV decreased = good addition)
            delta_ofv <- parent_ofv - file_results$ofv
          }

          search_state$search_database$delta_ofv[db_idx] <- delta_ofv
        }
      }
    }
  }

  # ===== SECTION 6: CONSOLE OUTPUT =====
  # Get step and covariate info for display
  step_num <- search_state$search_database$step_number[db_idx]
  covariate <- search_state$search_database$covariate_tested[db_idx]
  parent_model <- search_state$search_database$parent_model[db_idx]
  delta_ofv <- search_state$search_database$delta_ofv[db_idx]

  # Format step display
  step_display <- if (!is.na(step_num)) sprintf("[Step %d]", step_num) else ""

  # Format covariate display
  cov_display <- if (!is.na(covariate) && nchar(covariate) > 0) {
    sprintf(" (%s)", covariate)
  } else {
    ""
  }

  # Status icon using case_when
  status_icon <- dplyr::case_when(
    file_results$status == "completed" ~ "✅",
    file_results$status == "failed" ~ "❌",
    file_results$status == "in_progress" ~ "⏳",
    TRUE ~ "⚠️"
  )

  # Build status message
  if (file_results$status == "completed" && !is.na(file_results$ofv)) {
    if (!is.na(parent_model) && !is.na(delta_ofv)) {
      parent_ofv <- search_state$search_database$ofv[search_state$search_database$model_name == parent_model][1]

      cat(sprintf("%s %s Model %s%s completed: OFV %.2f → %.2f (ΔOFV: %.2f)\n",
                  step_display, status_icon, model_name, cov_display,
                  parent_ofv, file_results$ofv, delta_ofv))

      # Add RSE warning if high
      if (!is.na(file_results$rse_max) && file_results$rse_max > 50) {
        cat(sprintf("    ⚠️  High RSE detected: %.1f%%\n", file_results$rse_max))
      }
    } else {
      cat(sprintf("%s %s Model %s%s completed: OFV = %.2f\n",
                  step_display, status_icon, model_name, cov_display, file_results$ofv))
    }
  } else if (file_results$status == "failed") {
    cat(sprintf("%s %s Model %s%s failed: %s\n",
                step_display, status_icon, model_name, cov_display,
                file_results$error_message))
  } else if (file_results$status == "in_progress") {
    cat(sprintf("%s %s Model %s%s still running...\n",
                step_display, status_icon, model_name, cov_display))
  }

  return(search_state)
}

#' Force Update Models with Fresh File Read
#'
#' @title Force update one or more models with fresh file reads
#' @description Forces a complete re-read of model files and updates database.
#'   Useful for correcting database values that may be incorrect.
#' @param search_state List containing search state
#' @param model_names Character vector. Model(s) to force update (can be single or multiple)
#' @return Updated search_state
#' @export
force_update_models <- function(search_state, model_names) {
  n_models <- length(model_names)

  if (n_models == 1) {
    cat(sprintf("\n🔄 Force updating %s from files...\n", model_names))
  } else {
    cat(sprintf("\n🔄 Force updating %d models from files...\n", n_models))
  }

  for (model_name in model_names) {
    search_state <- update_model_status_from_files(search_state, model_name, force = TRUE)
  }

  cat("✅ Force update complete\n")
  return(search_state)
}

#' Update All Model Statuses (FIXED)
#'
#' @title Updates all models with robust error handling and concise progress reporting
#' @description Enhanced version with comprehensive error handling and validation
#' @param search_state List. Current search state
#' @param show_progress Logical. Whether to show progress summary (default: TRUE)
#' @return List with updated search_state
#' @export
update_all_model_statuses <- function(search_state, show_progress = TRUE) {
  # FIXED: Comprehensive input validation
  if (is.null(search_state) || is.null(search_state$search_database)) {
    cat("❌ Invalid search_state or missing database\n")
    return(search_state)
  }

  if (show_progress) {
    cat("📊 Updating model statuses...\n")
  }

  # FIXED: Safer extraction with validation
  models_to_update <- tryCatch({
    if (is.data.frame(search_state$search_database) &&
        "model_name" %in% names(search_state$search_database) &&
        "status" %in% names(search_state$search_database)) {

      # Get all models
      all_models <- search_state$search_database$model_name
      all_statuses <- search_state$search_database$status

      # Filter OUT models with terminal statuses
      terminal_statuses <- c("completed", "failed", "estimation_error")
      needs_update <- !(all_statuses %in% terminal_statuses)

      # Get models that need updating
      models_needing_update <- all_models[needs_update]

      if (show_progress && length(all_models) > 0) {
        skipped_count <- sum(!needs_update)
        if (skipped_count > 0) {
          cat(sprintf("  ⏩ Skipping %d finished models, checking %d active models\n",
                      skipped_count, length(models_needing_update)))
        }
      }

      models_needing_update
    } else {
      character(0)
    }
  }, error = function(e) {
    character(0)
  })

  # FIXED: Handle empty database
  if (length(models_to_update) == 0) {
    if (show_progress) {
      cat("  ✅ No active models to update\n")
    }
    return(search_state)
  }

  # Track changes for summary
  status_changes <- list(newly_completed = character(0), newly_failed = character(0))

  for (model_name in models_to_update) {
    # FIXED: Comprehensive validation for model_name
    if (is.na(model_name) || is.null(model_name) ||
        length(model_name) == 0 || nchar(as.character(model_name)) == 0) {
      next
    }

    # FIXED: Safer status extraction with validation
    old_status_idx <- which(search_state$search_database$model_name == model_name)
    if (length(old_status_idx) == 0) {
      next
    }

    old_status <- tryCatch({
      if ("status" %in% names(search_state$search_database)) {
        search_state$search_database$status[old_status_idx[1]]
      } else {
        NA_character_
      }
    }, error = function(e) {
      NA_character_
    })

    # FIXED: Comprehensive error handling for update function
    search_state <- tryCatch({
      # Validate that the update function exists
      if (exists("update_model_status_from_files") &&
          is.function(update_model_status_from_files)) {
        update_model_status_from_files(search_state, model_name)
      } else {
        cat(sprintf("❌ update_model_status_from_files function not available for %s\n", model_name))
        search_state
      }
    }, error = function(e) {
      cat(sprintf("❌ Error updating %s: %s\n", model_name, e$message))
      search_state  # Return unchanged state on error
    })

    # FIXED: Safer new status extraction
    new_status_idx <- which(search_state$search_database$model_name == model_name)
    if (length(new_status_idx) == 0) {
      next
    }

    new_status <- tryCatch({
      if ("status" %in% names(search_state$search_database)) {
        search_state$search_database$status[new_status_idx[1]]
      } else {
        NA_character_
      }
    }, error = function(e) {
      NA_character_
    })

    # FIXED: Comprehensive status comparison with validation
    if (!is.na(old_status) && !is.na(new_status) &&
        !is.null(old_status) && !is.null(new_status) &&
        nchar(as.character(old_status)) > 0 &&
        nchar(as.character(new_status)) > 0 &&
        as.character(old_status) != as.character(new_status)) {

      if (as.character(new_status) == "completed" && as.character(old_status) != "completed") {
        status_changes$newly_completed <- c(status_changes$newly_completed, model_name)
      } else if (as.character(new_status) == "failed" && as.character(old_status) != "failed") {
        status_changes$newly_failed <- c(status_changes$newly_failed, model_name)
      }
    }
  }

  if (show_progress) {
    # FIXED: Safer summary statistics with comprehensive validation
    total_completed <- tryCatch({
      if ("status" %in% names(search_state$search_database)) {
        sum(search_state$search_database$status == "completed" &
              !is.na(search_state$search_database$ofv) &
              is.finite(search_state$search_database$ofv), na.rm = TRUE)
      } else {
        0
      }
    }, error = function(e) {
      0
    })

    total_failed <- tryCatch({
      if ("status" %in% names(search_state$search_database)) {
        sum(search_state$search_database$status == "failed", na.rm = TRUE)
      } else {
        0
      }
    }, error = function(e) {
      0
    })

    cat(sprintf("✅ Status update complete: %d completed, %d failed total\n",
                total_completed, total_failed))

    # FIXED: Safer significance reporting with comprehensive validation
    if (length(status_changes$newly_completed) > 0) {
      tryCatch({
        # FIXED: Comprehensive column validation before dplyr operations
        required_cols_for_summary <- c("model_name", "delta_ofv", "covariate_tested")
        if (all(required_cols_for_summary %in% names(search_state$search_database))) {

          # FIXED: Safe filtering without dplyr pipe operations
          db_filtered <- search_state$search_database[
            search_state$search_database$model_name %in% status_changes$newly_completed &
              !is.na(search_state$search_database$delta_ofv) &
              search_state$search_database$delta_ofv > 3.84, ]

          if (nrow(db_filtered) > 0) {
            # FIXED: Safe ordering without dplyr
            order_idx <- order(db_filtered$delta_ofv, decreasing = TRUE)
            significant_new <- db_filtered[order_idx, ]

            cat("⭐ New significant improvements:\n")
            for (i in 1:nrow(significant_new)) {
              row <- significant_new[i, ]
              # FIXED: Comprehensive validation for covariate display
              covariate_display <- tryCatch({
                cov_val <- row$covariate_tested
                if (is.na(cov_val) || is.null(cov_val) ||
                    nchar(as.character(cov_val)) == 0) {
                  "Unknown"
                } else {
                  as.character(cov_val)
                }
              }, error = function(e) {
                "Unknown"
              })

              # FIXED: Safe sprintf with validation
              delta_val <- tryCatch({
                if (is.numeric(row$delta_ofv) && !is.na(row$delta_ofv)) {
                  sprintf("%.2f", row$delta_ofv)
                } else {
                  "Unknown"
                }
              }, error = function(e) {
                "Unknown"
              })

              cat(sprintf("   %s (%s): ΔOFV = %s\n",
                          as.character(row$model_name), covariate_display, delta_val))
            }
          }
        }
      }, error = function(e) {
        cat("⚠️  Could not generate significance summary\n")
      })
    }
  }

  return(search_state)
}

#' Get Maximum RSE Using Existing get_param2
#'
#' @title Extract maximum RSE from model using existing functionality
#' @description Wrapper around get_param2 to extract max RSE value
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Numeric. Maximum RSE value or NA
#' @export
get_model_max_rse <- function(search_state, model_name) {
  tryCatch({
    # Use existing get_param2 function
    params <- get_param2(
      model_number = model_name,
      count_model = 1,
      models_folder = search_state$models_folder
    )

    # Extract RSE values
    rse_values <- params$RSE[!is.na(params$RSE) & is.finite(params$RSE)]

    if (length(rse_values) > 0) {
      return(max(rse_values))
    } else {
      return(NA_real_)
    }
  }, error = function(e) {
    return(NA_real_)
  })
}


#' Validate NONMEM Parameter Block Formatting
#'
#' @title Check THETA, OMEGA, and SIGMA block formatting for SCM compatibility
#' @description Validates that parameter blocks follow the required format:
#'   - OMEGA BLOCK: One value per line
#'   - All blocks: Proper comment structure (value ; name ; units ; transform)
#'
#'   Required format for all parameter lines:
#'   number ; PARAM_NAME ; [units] ; RATIO|LOG
#'
#'   Examples:
#'   $THETA
#'   0.5 ; CL ; L/h ; LOG
#'   10  ; V  ; L   ; LOG
#'
#'   $OMEGA BLOCK(3)
#'   0.1 ; IIV_CL    ; ; RATIO
#'   0.1 ; IIV_CL_V2 ; ; RATIO
#'   0.1 ; IIV_V2    ; ; RATIO
#'
#'   $SIGMA
#'   0.1 ; RUV_PROP ; ; RATIO
#'
#' @param model_file Character. Path to .ctl or .mod file
#' @param check_omega_structure Logical. Check OMEGA BLOCK has one value per line (default: TRUE)
#' @param check_comments Logical. Require proper comment structure (default: TRUE)
#' @param allow_empty_units Logical. Allow empty units field (default: TRUE)
#' @return List with validation results
#' @export
validate_parameter_blocks <- function(model_file,
                                      check_omega_structure = TRUE,
                                      check_comments = TRUE,
                                      allow_empty_units = TRUE) {

  # ===================================================================
  # SECTION 1: READ MODEL FILE
  # ===================================================================

  if (!file.exists(model_file)) {
    return(list(
      valid = FALSE,
      issues = paste("Model file not found:", model_file),
      model_file = model_file
    ))
  }

  model_lines <- tryCatch({
    readLines(model_file, warn = FALSE)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(model_lines)) {
    return(list(
      valid = FALSE,
      issues = "Cannot read model file",
      model_file = model_file
    ))
  }

  # ===================================================================
  # SECTION 2: IDENTIFY PARAMETER BLOCKS
  # ===================================================================

  issues <- character(0)
  warnings <- character(0)

  # Find all parameter block starts
  theta_lines <- grep("^\\s*\\$THETA", model_lines, ignore.case = TRUE)
  omega_lines <- grep("^\\s*\\$OMEGA", model_lines, ignore.case = TRUE)
  sigma_lines <- grep("^\\s*\\$SIGMA", model_lines, ignore.case = TRUE)

  all_blocks <- list(
    THETA = theta_lines,
    OMEGA = omega_lines,
    SIGMA = sigma_lines
  )

  # ===================================================================
  # SECTION 3: VALIDATE EACH PARAMETER BLOCK
  # ===================================================================

  for (block_type in names(all_blocks)) {
    block_starts <- all_blocks[[block_type]]

    if (length(block_starts) == 0) next

    for (start_line in block_starts) {
      block_header <- model_lines[start_line]

      # Check if this is an OMEGA BLOCK
      is_omega_block <- grepl("BLOCK\\s*\\(", block_header, ignore.case = TRUE)

      # Extract block size for OMEGA BLOCK
      block_size <- NA
      if (is_omega_block) {
        block_size_match <- regmatches(block_header,
                                       regexec("BLOCK\\s*\\(\\s*(\\d+)\\s*\\)",
                                               block_header, ignore.case = TRUE))
        if (length(block_size_match[[1]]) > 1) {
          block_size <- as.integer(block_size_match[[1]][2])
        }
      }

      # Find end of this block (next $ section)
      next_section_idx <- grep("^\\s*\\$",
                               model_lines[(start_line + 1):length(model_lines)])
      if (length(next_section_idx) > 0) {
        end_line <- start_line + next_section_idx[1] - 1
      } else {
        end_line <- length(model_lines)
      }

      # Get block content (skip header)
      block_lines <- model_lines[(start_line + 1):end_line]

      # Remove empty lines and pure comment lines
      block_lines_idx <- (start_line + 1):end_line
      valid_idx <- !grepl("^\\s*$", block_lines) & !grepl("^\\s*;", block_lines)
      block_lines <- block_lines[valid_idx]
      block_line_numbers <- block_lines_idx[valid_idx]

      if (length(block_lines) == 0) next

      # ===================================================================
      # SECTION 4: VALIDATE OMEGA BLOCK STRUCTURE
      # ===================================================================

      if (check_omega_structure && is_omega_block && !is.na(block_size)) {
        # Check: Should have exactly block_size lines (one per parameter)
        if (length(block_lines) != block_size) {
          issues <- c(issues, sprintf(
            "%s BLOCK(%d) at line %d: Expected %d lines, found %d lines",
            block_type, block_size, start_line, block_size, length(block_lines)
          ))
        }

        # Check: Each line should have only ONE numeric value
        for (i in seq_along(block_lines)) {
          line <- block_lines[i]
          line_num <- block_line_numbers[i]

          # Split by semicolon
          parts <- strsplit(line, ";")[[1]]

          if (length(parts) >= 1) {
            value_part <- trimws(parts[1])

            # Count numeric values (exclude FIX keyword)
            value_part_clean <- gsub("FIX", "", value_part, ignore.case = TRUE)
            numbers <- gregexpr("-?\\d+\\.?\\d*([eE][+-]?\\d+)?", value_part_clean)
            num_values <- length(unlist(regmatches(value_part_clean, numbers)))

            if (num_values > 1) {
              issues <- c(issues, sprintf(
                "Line %d (%s BLOCK line %d): Multiple values (%d) on one line - should have ONE value per line",
                line_num, block_type, i, num_values
              ))
            } else if (num_values == 0) {
              issues <- c(issues, sprintf(
                "Line %d (%s BLOCK line %d): No numeric value found",
                line_num, block_type, i
              ))
            }
          }
        }
      }

      # ===================================================================
      # SECTION 5: VALIDATE COMMENT STRUCTURE
      # ===================================================================

      if (check_comments) {
        for (i in seq_along(block_lines)) {
          line <- block_lines[i]
          line_num <- block_line_numbers[i]

          # Expected format: number ; NAME ; units ; RATIO|LOG
          # Split by semicolon
          parts <- strsplit(line, ";")[[1]]
          parts <- trimws(parts)

          # Check for 3 semicolons (units field can be empty)
          semicolon_count <- length(gregexpr(";", line)[[1]])
          if (semicolon_count < 3) {
            issues <- c(issues, sprintf(
              "Line %d (%s): Incorrect comment format - expected 'value ; NAME ; units ; RATIO|LOG', found %d semicolon(s)",
              line_num, block_type, semicolon_count
            ))
            next
          }

          value_part <- parts[1]
          name_part <- parts[2]
          units_part <- parts[3]
          transform_part <- parts[4]

          # Check 1: Value part should have a number
          has_number <- grepl("\\d", value_part)
          if (!has_number) {
            issues <- c(issues, sprintf(
              "Line %d (%s): No numeric value found in first field",
              line_num, block_type
            ))
          }

          # Check 2: Name should not be empty
          if (nchar(name_part) == 0) {
            issues <- c(issues, sprintf(
              "Line %d (%s): Parameter name (field 2) is empty",
              line_num, block_type
            ))
          }

          # Check 3: Units can be empty if allowed
          if (!allow_empty_units && nchar(units_part) == 0) {
            warnings <- c(warnings, sprintf(
              "Line %d (%s): Units field (field 3) is empty",
              line_num, block_type
            ))
          }

          # Check 4: Transform should be RATIO or LOG
          transform_upper <- toupper(trimws(transform_part))
          valid_transforms <- c("RATIO", "LOG")
          if (!(transform_upper %in% valid_transforms)) {
            issues <- c(issues, sprintf(
              "Line %d (%s): Transform (field 4) should be 'RATIO' or 'LOG', found '%s'",
              line_num, block_type, transform_part
            ))
          }

          # Check 5: For THETA and SIGMA, should not have multiple values per line
          if (block_type %in% c("THETA", "SIGMA")) {
            value_part_clean <- gsub("FIX", "", value_part, ignore.case = TRUE)
            # Remove bounds syntax like (0, x, 100)
            value_part_clean <- gsub("\\([^)]+\\)", "", value_part_clean)
            numbers <- gregexpr("-?\\d+\\.?\\d*([eE][+-]?\\d+)?", value_part_clean)
            num_values <- length(unlist(regmatches(value_part_clean, numbers)))

            if (num_values > 1) {
              issues <- c(issues, sprintf(
                "Line %d (%s): Multiple values (%d) on one line - should have ONE value per line",
                line_num, block_type, num_values
              ))
            }
          }
        }
      }
    }
  }

  # ===================================================================
  # SECTION 6: GENERATE RESULTS
  # ===================================================================

  valid <- length(issues) == 0

  result <- list(
    valid = valid,
    issues = issues,
    warnings = warnings,
    model_file = model_file,
    blocks_checked = list(
      THETA = length(all_blocks$THETA) > 0,
      OMEGA = length(all_blocks$OMEGA) > 0,
      SIGMA = length(all_blocks$SIGMA) > 0
    )
  )

  return(result)
}


#' Print Parameter Block Validation Results
#'
#' @title Display parameter validation results in readable format
#' @param validation_result List. Output from validate_parameter_blocks()
#' @param verbose Logical. Show detailed information (default: TRUE)
#' @export
print_parameter_validation <- function(validation_result, verbose = TRUE) {

  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("PARAMETER BLOCK VALIDATION RESULTS\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  cat(sprintf("Model file: %s\n\n", validation_result$model_file))

  # Show which blocks were found
  blocks_found <- names(validation_result$blocks_checked)[
    unlist(validation_result$blocks_checked)]
  if (length(blocks_found) > 0) {
    cat(sprintf("Blocks found: %s\n\n", paste(blocks_found, collapse = ", ")))
  } else {
    cat("No parameter blocks found\n\n")
  }

  # Validation status
  if (validation_result$valid) {
    cat("✅ VALIDATION PASSED\n")
    cat("All parameter blocks are correctly formatted.\n")

    if (length(validation_result$warnings) > 0 && verbose) {
      cat(sprintf("\n⚠️  %d warning(s):\n", length(validation_result$warnings)))
      for (warning in validation_result$warnings) {
        cat(sprintf("  • %s\n", warning))
      }
    }

  } else {
    cat("❌ VALIDATION FAILED\n\n")
    cat(sprintf("Found %d issue(s):\n\n", length(validation_result$issues)))

    for (issue in validation_result$issues) {
      cat(sprintf("  ❌ %s\n", issue))
    }

    if (length(validation_result$warnings) > 0 && verbose) {
      cat(sprintf("\n⚠️  %d warning(s):\n", length(validation_result$warnings)))
      for (warning in validation_result$warnings) {
        cat(sprintf("  • %s\n", warning))
      }
    }

    # Provide fix guidance
    cat("\n")
    cat(paste(rep("-", 70), collapse = ""), "\n")
    cat("REQUIRED FORMAT:\n")
    cat(paste(rep("-", 70), collapse = ""), "\n")
    cat("Each parameter line must follow this structure:\n")
    cat("  value ; PARAM_NAME ; units ; RATIO|LOG\n\n")
    cat("Examples:\n")
    cat("  $THETA\n")
    cat("  0.5 ; TVCL ; L/h ; LOG\n")
    cat("  10  ; TVV  ; L   ; LOG\n\n")
    cat("  $OMEGA BLOCK(3)\n")
    cat("  0.1 ; IIV_CL    ; ; RATIO\n")
    cat("  0.1 ; IIV_CL_V2 ; ; RATIO\n")
    cat("  0.1 ; IIV_V2    ; ; RATIO\n\n")
    cat("  $SIGMA\n")
    cat("  0.1 ; RUV_PROP ; ; RATIO\n")
  }

  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  invisible(validation_result)
}


#' Validate Base Model Parameter Structure (For Initialization)
#'
#' @title Check base model parameter formatting during SCM initialization
#' @description Called during initialize_covariate_search() to ensure base model
#'   has properly formatted THETA, OMEGA, and SIGMA blocks for SCM operations.
#'   This prevents issues during covariate addition/removal.
#'
#' @param base_model_path Character. Path to base model directory or .ctl file
#' @param strict Logical. If TRUE, stops on validation failure (default: TRUE)
#' @param models_folder Character. Directory containing models (default: NULL)
#' @param check_omega_structure Logical. Validate OMEGA BLOCK structure (default: TRUE)
#' @param check_comments Logical. Validate comment structure (default: TRUE)
#' @return List with validation results. Stops execution if strict=TRUE and validation fails.
#' @export
validate_base_model_parameters <- function(base_model_path,
                                           models_folder = NULL,
                                           strict = TRUE,
                                           check_omega_structure = TRUE,
                                           check_comments = TRUE) {

  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("VALIDATING BASE MODEL PARAMETER STRUCTURE\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")

  # Construct full path if models_folder provided
  if (!is.null(models_folder)) {
    full_path <- file.path(models_folder, base_model_path)
  } else {
    full_path <- base_model_path
  }

  ctl_path <- paste0(full_path, ".ctl")
  mod_path <- paste0(full_path, ".mod")

  if (file.exists(ctl_path)) {
    model_file <- ctl_path
  } else if (file.exists(mod_path)) {
    model_file <- mod_path
  } else if (file.exists(full_path)) {
    # Full path already includes extension
    model_file <- full_path
  } else if (dir.exists(full_path)) {
    # Look inside directory
    ctl_files <- list.files(full_path, pattern = "\\.(ctl|mod)$",
                            full.names = TRUE, ignore.case = TRUE)
    if (length(ctl_files) == 0) {
      stop("No .ctl or .mod file found in directory: ", full_path)
    }
    model_file <- ctl_files[1]
  } else {
    stop("Base model path not found: ", full_path)
  }

  cat(sprintf("\nChecking: %s\n", model_file))

  # Run validation
  validation <- validate_parameter_blocks(
    model_file = model_file,
    check_omega_structure = check_omega_structure,
    check_comments = check_comments,
    allow_empty_units = TRUE
  )

  # Print results
  print_parameter_validation(validation, verbose = TRUE)

  # Handle validation failure
  if (!validation$valid) {
    if (strict) {
      stop("\n❌ Base model validation FAILED. Please fix parameter block formatting before initializing SCM.")
    } else {
      warning("\n⚠️  Base model has formatting issues. SCM may encounter problems.")
    }
  } else {
    cat("✅ Base model parameter structure is valid for SCM\n\n")
  }

  return(validation)
}



