# =============================================================================
# VALIDATION
# File: R/validation.R
# Part of CovariateSearcher Package
# Model validation and quality assessment
# =============================================================================







#' Update Model Status from Files with Enhanced Error Detection (FIXED)
#'
#' @title Updates search database with enhanced results from NONMEM output
#' @description Enhanced version with concise error reporting and comprehensive error handling
#' @param search_state List. Current search state
#' @param model_name Character. Model name to update
#' @return List with updated search_state
#' @export
update_model_status_from_files <- function(search_state, model_name) {
  # FIXED: Add comprehensive input validation
  if (is.null(search_state) || is.null(model_name) || length(model_name) == 0) {
    cat("❌ Invalid input parameters\n")
    return(search_state)
  }

  # FIXED: Validate search_state structure
  if (is.null(search_state$search_database) || is.null(search_state$models_folder)) {
    cat("❌ Invalid search_state structure\n")
    return(search_state)
  }

  # FIXED: Validate required columns exist with more comprehensive check
  required_cols <- c("model_name", "status", "ofv", "estimation_issue",
                     "covariate_tested", "parent_model", "delta_ofv",
                     "submission_time", "completion_time", "step_number",
                     "excluded_from_step", "original_model")
  missing_cols <- setdiff(required_cols, names(search_state$search_database))
  if (length(missing_cols) > 0) {
    cat(sprintf("❌ Missing database columns: %s\n", paste(missing_cols, collapse = ", ")))
    return(search_state)
  }

  model_path <- file.path(search_state$models_folder, model_name)
  db_idx <- which(search_state$search_database$model_name == model_name)
  if (length(db_idx) == 0) {
    cat(sprintf("⚠️  Model %s not found in database\n", model_name))
    return(search_state)
  }

  # Get current status from database (removed, not needed)

  # Check if LST file exists
  lst_file <- file.path(model_path, paste0(model_name, ".lst"))
  lst_exists <- file.exists(lst_file)

  # Simple status determination
  if (!lst_exists) {
    # No LST file = still running
    actual_status <- "in_progress"
    lst_info <- list(status = "in_progress", error_message = NA, has_issues = FALSE)
  } else {
    # LST file exists - read it to determine status
    lst_info <- tryCatch({
      if (exists("read_nonmem_lst") && is.function(read_nonmem_lst)) {
        result <- read_nonmem_lst(model_path)

        # IMPORTANT: Check if "Model run incomplete" means still running
        if (!is.null(result$error_message) &&
            grepl("Model run incomplete|not yet completed", result$error_message, ignore.case = TRUE)) {
          # Model is still running, not failed
          list(status = "in_progress", error_message = NA, has_issues = FALSE)
        } else {
          result
        }
      } else {
        # Fallback: check if LST contains key phrases
        lst_content <- readLines(lst_file, warn = FALSE)

        # Check for completion markers FIRST
        if (any(grepl("COMPLETED|SUCCESSFUL", lst_content))) {
          list(status = "completed", error_message = NA, has_issues = FALSE)
        } else if (any(grepl("TERMINATED", lst_content)) &&
                   any(grepl("ERROR", lst_content))) {
          # Only failed if we have BOTH termination AND error
          error_line <- lst_content[grep("ERROR", lst_content)[1]]
          list(status = "failed", error_message = error_line, has_issues = TRUE)
        } else if (any(grepl("Stop Time:", lst_content))) {
          # Has stop time but no success message = failed
          list(status = "failed", error_message = "Run completed without success", has_issues = TRUE)
        } else {
          # No completion markers = still running
          list(status = "in_progress", error_message = NA, has_issues = FALSE)
        }
      }
    }, error = function(e) {
      list(status = "read_error", error_message = paste("LST read error:", e$message), has_issues = TRUE)
    })

    # Determine actual status based on LST content
    if (lst_info$status == "completed") {
      actual_status <- "completed"
    } else if (lst_info$status == "failed" &&
               !grepl("incomplete|not yet completed", lst_info$error_message, ignore.case = TRUE)) {
      # Only mark as failed if it's truly failed, not just incomplete
      actual_status <- "failed"
    } else if (lst_info$status == "in_progress") {
      actual_status <- "in_progress"
    } else if (lst_info$has_issues &&
               !grepl("incomplete|not yet completed", lst_info$error_message, ignore.case = TRUE)) {
      actual_status <- "failed"
    } else {
      # Default to running if uncertain
      actual_status <- "in_progress"
    }
  }

  # Extract results if completed
  results <- list(ofv = NA_real_, rse_max = NA_real_, status = actual_status)
  if (actual_status == "completed") {
    results <- tryCatch({
      # Primary path: use extract_model_results
      base_results <- extract_model_results(search_state, model_name)

      # Extract RSE using get_param2
      rse_max <- tryCatch({
        params <- get_param2(
          model_number = model_name,
          count_model = 1,
          models_folder = search_state$models_folder
        )

        # Get maximum RSE from the params
        rse_values <- params$RSE[!is.na(params$RSE) & is.finite(params$RSE)]
        if (length(rse_values) > 0) {
          max(rse_values)
        } else {
          NA_real_
        }
      }, error = function(e) {
        # RSE extraction failed, keep as NA
        NA_real_
      })

      # Add RSE to results
      base_results$rse_max <- rse_max
      base_results

    }, error = function(e) {
      # If extract_model_results fails, fall back to reading EXT file directly
      ext_file <- file.path(model_path, paste0(model_name, ".ext"))
      if (file.exists(ext_file)) {
        ext_lines <- readLines(ext_file, warn = FALSE)
        data_lines <- ext_lines[!grepl("^TABLE|^\\s*$", ext_lines)]

        # Extract OFV
        ofv_value <- NA_real_
        if (length(data_lines) > 0) {
          # Find lines with -1000000000 (final estimates)
          final_lines_idx <- grep("^\\s*-1000000000", data_lines)
          if (length(final_lines_idx) > 0) {
            # Get the line BEFORE the last -1000000000 line
            last_final_idx <- tail(final_lines_idx, 1)
            if (last_final_idx > 1) {
              ofv_line <- data_lines[last_final_idx - 1]
              values <- as.numeric(strsplit(trimws(ofv_line), "\\s+")[[1]])
              ofv_value <- tail(values, 1)
            }
          } else {
            # No -1000000000 line found, use last data line
            last_line <- tail(data_lines, 1)
            values <- as.numeric(strsplit(trimws(last_line), "\\s+")[[1]])
            ofv_value <- tail(values, 1)
          }
        }

        # Extract RSE
        rse_max <- tryCatch({
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

        list(ofv = ofv_value, rse_max = rse_max, status = "completed")
      } else {
        list(ofv = NA_real_, rse_max = NA_real_, status = "completed")
      }
    })
  }

  # FIXED: Add comprehensive error handling for timestamp extraction
  timestamps <- tryCatch({
    if (exists("extract_nonmem_timestamps") && is.function(extract_nonmem_timestamps)) {
      extract_nonmem_timestamps(model_name, search_state$models_folder)
    } else {
      list(start_time = NA, stop_time = NA)
    }
  }, error = function(e) {
    list(start_time = NA, stop_time = NA)
  })

  # Update database
  tryCatch({
    search_state$search_database$status[db_idx] <- actual_status
    search_state$search_database$ofv[db_idx] <- results$ofv

    # Add RSE to database if available
    if (!is.null(results$rse_max) && !is.na(results$rse_max)) {
      search_state$search_database$rse_max[db_idx] <- results$rse_max
    }

    if (!is.null(lst_info$error_message) && !is.na(lst_info$error_message)) {
      search_state$search_database$estimation_issue[db_idx] <- lst_info$error_message
    }
  }, error = function(e) {
    cat(sprintf("❌ Error updating database for %s: %s\n", model_name, e$message))
    return(search_state)
  })

  # Update timestamps with validation
  if (!is.na(timestamps$start_time) && !is.null(timestamps$start_time)) {
    search_state$search_database$submission_time[db_idx] <- timestamps$start_time
  }

  if (!is.na(timestamps$stop_time) && !is.null(timestamps$stop_time)) {
    search_state$search_database$completion_time[db_idx] <- timestamps$stop_time
  } else if (actual_status %in% c("completed", "failed")) {
    search_state$search_database$completion_time[db_idx] <- Sys.time()
  }

  # FIXED: Safer model information extraction with validation
  model_row <- search_state$search_database[db_idx, , drop = FALSE]
  covariate <- if (nrow(model_row) > 0) model_row$covariate_tested[1] else NA
  parent_model <- if (nrow(model_row) > 0) model_row$parent_model[1] else NA

  # NEW: Extract step information
  step_number <- if (nrow(model_row) > 0) model_row$step_number[1] else NA
  step_prefix <- if (!is.na(step_number)) sprintf("[Step %d] ", step_number) else ""

  # FIXED: Robust covariate display logic
  display_covariate <- if (is.na(covariate) || is.null(covariate) ||
                           nchar(as.character(covariate)) == 0 ||
                           as.character(covariate) == "") {
    "Unknown"
  } else {
    as.character(covariate)
  }

  # Status reporting
  if (actual_status == "failed") {
    error_msg <- if (is.null(lst_info$error_message) || is.na(lst_info$error_message)) {
      "Unknown error"
    } else {
      as.character(lst_info$error_message)
    }
    cat(sprintf("%s❌ Model %s (%s) FAILED: %s\n", step_prefix, model_name, display_covariate, error_msg))

  } else if (actual_status == "in_progress") {
    # Show running status
    cat(sprintf("%s🔄 Model %s (%s) running\n", step_prefix, model_name, display_covariate))

  } else if (actual_status == "completed") {
    # Only show completed when we truly have results
    if (!is.na(parent_model) && !is.null(parent_model) &&
        nchar(as.character(parent_model)) > 0 && as.character(parent_model) != "") {

      parent_idx <- which(search_state$search_database$model_name == parent_model)
      if (length(parent_idx) > 0) {
        parent_ofv <- search_state$search_database$ofv[parent_idx[1]]
        parent_status <- search_state$search_database$status[parent_idx[1]]
        current_ofv <- results$ofv

        if (!is.na(parent_status) && !is.na(actual_status) &&
            parent_status == "completed" && actual_status == "completed" &&
            !is.na(parent_ofv) && !is.na(current_ofv) &&
            is.finite(parent_ofv) && is.finite(current_ofv)) {

          delta_ofv <- parent_ofv - current_ofv
          search_state$search_database$delta_ofv[db_idx] <- delta_ofv

          cat(sprintf("%s✅ Model %s (%s) completed: OFV %.2f → %.2f (ΔOFV: %+.2f)\n",
                      step_prefix, model_name, display_covariate, parent_ofv, current_ofv, delta_ofv))

        } else if (parent_status == "failed" && actual_status == "completed") {
          search_state$search_database$delta_ofv[db_idx] <- 999999
          if (!is.na(current_ofv) && is.numeric(current_ofv)) {
            cat(sprintf("%s🎉 Model %s (%s) FIXED failed parent %s! OFV: %.2f\n",
                        step_prefix, model_name, display_covariate, parent_model, current_ofv))
          } else {
            cat(sprintf("%s🎉 Model %s (%s) FIXED failed parent %s!\n",
                        step_prefix, model_name, display_covariate, parent_model))
          }
        } else {
          search_state$search_database$delta_ofv[db_idx] <- NA_real_
          if (!is.na(current_ofv) && is.numeric(current_ofv)) {
            cat(sprintf("%s✅ Model %s (%s) completed: OFV %.2f\n",
                        step_prefix, model_name, display_covariate, current_ofv))
          } else {
            cat(sprintf("%s✅ Model %s (%s) completed\n",
                        step_prefix, model_name, display_covariate))
          }
        }
      } else {
        # No parent found in database
        search_state$search_database$delta_ofv[db_idx] <- NA_real_
        if (!is.na(results$ofv) && is.numeric(results$ofv)) {
          cat(sprintf("%s✅ Model %s (%s) completed: OFV %.2f\n",
                      step_prefix, model_name, display_covariate, results$ofv))
        } else {
          cat(sprintf("%s✅ Model %s (%s) completed\n",
                      step_prefix, model_name, display_covariate))
        }
      }
    } else {
      # Base model or no parent
      search_state$search_database$delta_ofv[db_idx] <- ifelse(
        model_name == search_state$base_model, 0.0, NA_real_)

      if (!is.na(results$ofv) && is.numeric(results$ofv)) {
        cat(sprintf("%s✅ Model %s (%s) completed: OFV %.2f\n",
                    step_prefix, model_name, display_covariate, results$ofv))
      } else {
        cat(sprintf("%s✅ Model %s (%s) completed\n",
                    step_prefix, model_name, display_covariate))
      }
    }
  } else {
    # Any other status
    cat(sprintf("%s❓ Model %s (%s) status: %s\n",
                step_prefix, model_name, display_covariate, actual_status))
  }

  # Set excluded_from_step based on model completion status
  if (actual_status == "completed") {
    search_state$search_database$excluded_from_step[db_idx] <- FALSE
  } else if (actual_status == "failed") {
    search_state$search_database$excluded_from_step[db_idx] <- TRUE
  }

  # Handle retry model success - reset original model exclusion
  has_original_model <- !is.na(model_row$original_model) &&
    nchar(as.character(model_row$original_model)) > 0

  if (has_original_model && actual_status == "completed") {
    original_model_name <- as.character(model_row$original_model)
    original_idx <- which(search_state$search_database$model_name == original_model_name)
    if (length(original_idx) > 0) {
      search_state$search_database$excluded_from_step[original_idx] <- FALSE
      cat(sprintf("%s🔄 Reset exclusion for original model %s (retry succeeded)\n",
                  step_prefix, original_model_name))
    }
  }

  # Fix retry models with missing step_description and phase
  if (grepl("\\d{3}$", model_name)) {  # This is a retry model
    current_row <- search_state$search_database[db_idx, ]

    if (is.na(current_row$step_description) || is.na(current_row$phase)) {
      covariate_name <- current_row$covariate_tested[1]

      if (!is.na(covariate_name) && covariate_name != "") {
        search_state$search_database$step_description[db_idx] <- sprintf("Retry %s", covariate_name)
        search_state$search_database$phase[db_idx] <- "retry"

        cat(sprintf("  🔧 Fixed retry model metadata for %s\n", model_name))
      }
    }
  }

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
