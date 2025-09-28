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
          # Calculate delta OFV (parent - child, positive = improvement)
          delta_ofv <- parent_ofv - file_results$ofv
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
