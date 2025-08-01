#' @title Recovery Functions
#' @description Automatic recovery system for failed NONMEM models
#' @name recovery-functions
NULL

#' Read NONMEM EXT File with Issue Detection
#'
#' Enhanced .ext file reader that detects estimation problems
#'
#' @param model_name Character. Model name (e.g., "run25")
#' @param models_folder Character. Models folder path
#' @return List with detailed status and issue detection
#' @export
read_ext_file_with_issues <- function(model_name, models_folder = "models") {

  ext_file <- file.path(models_folder, model_name, paste0(model_name, ".ext"))
  
  if (!file.exists(ext_file)) {
    return(list(
      status = "not_started",
      current_ofv = NA,
      iterations = 0,
      last_iteration_time = NA,
      has_estimation_issues = FALSE,
      issue_type = NA,
      estimation_method = NA
    ))
  }
  
  tryCatch({
    ext_lines <- readLines(ext_file, warn = FALSE)
    
    if (length(ext_lines) == 0) {
      return(list(
        status = "empty_file",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = FALSE,
        issue_type = NA,
        estimation_method = NA
      ))
    }
    
    # Find TABLE headers
    table_lines <- which(grepl("^TABLE", ext_lines))
    if (length(table_lines) == 0) table_lines <- 1
    
    # Process final table
    final_table_data <- NULL
    final_method <- "Unknown"
    
    for (i in seq_along(table_lines)) {
      table_start <- table_lines[i]
      table_end <- if (i < length(table_lines)) table_lines[i + 1] - 1 else length(ext_lines)
      
      # Extract method name
      if (table_start <= length(ext_lines)) {
        method_line <- ext_lines[table_start]
        if (grepl("First Order", method_line)) {
          method_name <- "FOCE"
        } else if (grepl("Importance", method_line)) {
          method_name <- "IMP"
        } else if (grepl("Stochastic", method_line)) {
          method_name <- "SAEM"
        } else if (grepl("Laplacian", method_line)) {
          method_name <- "LAPLACE"
        } else {
          method_name <- paste("Method", i)
        }
      } else {
        method_name <- "Unknown"
      }
      
      # Find data lines
      table_section <- ext_lines[table_start:table_end]
      data_lines <- table_section[!grepl("^#|^TABLE|^\\s*ITERATION\\s+", table_section) & 
                                    nchar(trimws(table_section)) > 0]
      
      if (length(data_lines) > 0) {
        final_table_data <- data_lines
        final_method <- method_name
      }
    }
    
    if (is.null(final_table_data) || length(final_table_data) == 0) {
      return(list(
        status = "no_data",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = FALSE,
        issue_type = NA,
        estimation_method = final_method
      ))
    }
    
    # Parse last data line
    last_line <- tail(final_table_data, 1)
    values <- as.numeric(strsplit(trimws(last_line), "\\s+")[[1]])
    
    if (length(values) < 2) {
      return(list(
        status = "parse_error",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = TRUE,
        issue_type = "parse_error",
        estimation_method = final_method
      ))
    }
    
    # Extract iteration and OFV
    current_iteration <- values[1]
    current_ofv <- values[length(values)]
    
    # Detect estimation issues
    has_issues <- FALSE
    issue_type <- NA
    
    if (!is.na(current_ofv)) {
      if (is.infinite(current_ofv)) {
        has_issues <- TRUE
        issue_type <- "infinite_ofv"
      } else if (is.nan(current_ofv)) {
        has_issues <- TRUE
        issue_type <- "nan_ofv"
      } else if (abs(current_ofv) > 1e10) {
        has_issues <- TRUE
        issue_type <- "high_ofv"
      }
    } else {
      has_issues <- TRUE
      issue_type <- "missing_ofv"
    }
    
    # Check for problematic values
    if (!has_issues) {
      problematic_values <- values[is.infinite(values) | is.nan(values) | abs(values) > 1e10]
      if (length(problematic_values) > 0) {
        has_issues <- TRUE
        issue_type <- "problematic_parameters"
      }
    }
    
    # Determine status
    model_status <- if (current_iteration < 0) "completed" else "progressing"
    
    return(list(
      status = model_status,
      current_ofv = current_ofv,
      iterations = abs(current_iteration),
      last_iteration_time = file.info(ext_file)$mtime,
      has_estimation_issues = has_issues,
      issue_type = issue_type,
      estimation_method = final_method
    ))
    
  }, error = function(e) {
    return(list(
      status = "read_error",
      current_ofv = NA,
      iterations = 0,
      last_iteration_time = file.info(ext_file)$mtime,
      has_estimation_issues = TRUE,
      issue_type = paste("read_error:", e$message),
      estimation_method = "Unknown"
    ))
  })
}

#' Detect Estimation Problems in Multiple Models
#'
#' @param model_names Character vector. Model names to monitor
#' @param search_state List. Current search state
#' @param check_interval_minutes Numeric. Check interval (default 30)
#' @return List of models with issues
#' @export
detect_estimation_problems <- function(model_names, search_state, check_interval_minutes = 30) {

  cat(sprintf("🔍 Monitoring %d models for estimation issues\n", length(model_names)))
  
  models_with_issues <- list()
  
  for (model_name in model_names) {
    cat(sprintf("  Checking %s... ", model_name))
    
    ext_data <- read_ext_file_with_issues(model_name, search_state$models_folder)
    
    if (ext_data$has_estimation_issues) {
      cat(sprintf("⚠️  Issue: %s (OFV: %s)\n", ext_data$issue_type, 
                  ifelse(is.na(ext_data$current_ofv), "NA", 
                         sprintf("%.2e", ext_data$current_ofv))))
      
      models_with_issues[[model_name]] <- list(
        model_name = model_name,
        issue_type = ext_data$issue_type,
        current_ofv = ext_data$current_ofv,
        iterations = ext_data$iterations,
        detection_time = Sys.time()
      )
      
    } else {
      cat(sprintf("✅ OK (OFV: %s, Iter: %d)\n", 
                  ifelse(is.na(ext_data$current_ofv), "NA", 
                         sprintf("%.2f", ext_data$current_ofv)),
                  ext_data$iterations))
    }
  }
  
  if (length(models_with_issues) > 0) {
    cat(sprintf("\n⚠️  Found %d models with estimation issues\n", length(models_with_issues)))
  } else {
    cat("\n✅ All models progressing normally\n")
  }
  
  return(models_with_issues)
}

#' Create Retry Model with THETA Adjustments
#'
#' @param original_model_name Character. Name of problematic model
#' @param search_state List. Current search state
#' @param issue_type Character. Type of estimation issue
#' @return Updated search state and retry model info
#' @export
create_retry_model <- function(original_model_name, search_state, issue_type = "estimation_error") {

  cat(sprintf("🔧 Creating retry model for %s (issue: %s)\n", original_model_name, issue_type))
  
  # Generate retry model name
  model_number <- gsub("run", "", original_model_name)
  retry_model_name <- paste0("run", model_number, "001")
  
  cat(sprintf("  Original: %s → Retry: %s\n", original_model_name, retry_model_name))
  
  tryCatch({
    # Get original model information
    original_row <- search_state$search_database[search_state$search_database$model_name == original_model_name, ]
    
    if (nrow(original_row) == 0) {
      stop("Original model not found in database: ", original_model_name)
    }
    
    cat("  ✓ Original model found in database\n")
    
    # Create BBR model copy
    cat("  Creating BBR model copy...")
    original_model_path <- file.path(search_state$models_folder, original_model_name)
    
    # Use bbr to copy model
    retry_mod <- bbr::copy_model_from(
      .parent_mod = bbr::read_model(original_model_path),
      .new_model = retry_model_name,
      .inherit_tags = TRUE,
      .overwrite = TRUE
    )
    
    cat(" ✓\n")
    
    # Add retry model to database
    cat("  Adding to database...")
    
    new_row <- tibble::tibble(
      model_name = retry_model_name,
      step_description = paste(original_row$step_description[1], "- Retry"),
      phase = original_row$phase[1],
      step_number = original_row$step_number[1],
      parent_model = original_row$parent_model[1],
      covariate_tested = original_row$covariate_tested[1],
      action = "retry_with_theta_adjustment",
      ofv = NA_real_,
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      status = "created",
      tags = original_row$tags,
      submission_time = as.POSIXct(NA),
      completion_time = as.POSIXct(NA),
      retry_attempt = 1L,
      original_model = original_model_name,
      estimation_issue = issue_type,
      excluded_from_step = FALSE
    )
    
    search_state$search_database <- dplyr::bind_rows(search_state$search_database, new_row)
    
    # Update original model status
    orig_idx <- which(search_state$search_database$model_name == original_model_name)
    if (length(orig_idx) > 0) {
      search_state$search_database$status[orig_idx] <- "error"
      search_state$search_database$estimation_issue[orig_idx] <- issue_type
    }
    
    cat(" ✓\n")
    cat(sprintf("✅ Retry model %s created successfully\n", retry_model_name))
    
    return(list(
      search_state = search_state,
      retry_model_name = retry_model_name,
      original_model_name = original_model_name,
      issue_type = issue_type,
      status = "created"
    ))
    
  }, error = function(e) {
    cat(sprintf("❌ Failed to create retry model: %s\n", e$message))
    
    return(list(
      search_state = search_state,
      retry_model_name = NULL,
      original_model_name = original_model_name,
      issue_type = issue_type,
      status = "failed",
      error = e$message
    ))
  })
}

#' Get Excluded Covariates
#'
#' @param search_state List. Current search state
#' @return Character vector of excluded covariate names
#' @export
get_excluded_covariates <- function(search_state) {

  excluded_models <- search_state$search_database[search_state$search_database$excluded_from_step == TRUE, ]
  
  if (nrow(excluded_models) > 0) {
    excluded_covs <- unique(excluded_models$covariate_tested)
    excluded_covs <- excluded_covs[!is.na(excluded_covs) & excluded_covs != ""]
    return(excluded_covs)
  } else {
    return(character(0))
  }
}

#' Generate Recovery Report
#'
#' @param search_state List. Current search state
#' @return List with recovery statistics
#' @export
generate_recovery_report <- function(search_state) {

  # Overall statistics
  status_stats <- search_state$search_database %>%
    dplyr::count(status, .drop = FALSE) %>%
    dplyr::mutate(percentage = n * 100.0 / nrow(search_state$search_database))
  
  # Retry statistics
  retry_models <- search_state$search_database[grepl("\\d{3}$", search_state$search_database$model_name), ]
  retry_stats <- list(
    total_retries = nrow(retry_models),
    successful_retries = sum(retry_models$status == "completed", na.rm = TRUE),
    failed_retries = sum(retry_models$status %in% c("failed", "retry_failed"), na.rm = TRUE)
  )
  
  # Excluded covariates
  excluded_covs <- get_excluded_covariates(search_state)
  
  return(list(
    timestamp = Sys.time(),
    status_distribution = status_stats,
    retry_statistics = retry_stats,
    excluded_covariates = excluded_covs,
    recovery_success_rate = if(retry_stats$total_retries > 0) {
      retry_stats$successful_retries / retry_stats$total_retries * 100
    } else { 0 }
  ))
}
