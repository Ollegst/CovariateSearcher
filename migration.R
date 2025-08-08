# =============================================================================
# COVARIATESEARCHER: AUTOMATED FILE MIGRATION SCRIPT
# =============================================================================
# This script reorganizes the package from numbered files to module-based files
# It extracts functions accurately and creates new organized structure

#' Migrate CovariateSearcher Package Structure
#'
#' @description Reorganizes package from numbered files to module-based organization
#' @param package_root Character. Path to package root directory
#' @param dry_run Logical. If TRUE, only shows what would be done (default: TRUE)
#' @param backup Logical. If TRUE, creates backup of original files (default: TRUE)
#' @return List with migration results
#' @export
migrate_package_structure <- function(package_root = ".", dry_run = TRUE, backup = TRUE) {

  cat("🚀 CovariateSearcher Package Migration\n")
  cat("======================================\n")

  # Define R directory
  r_dir <- file.path(package_root, "R")

  if (!dir.exists(r_dir)) {
    stop("R directory not found at: ", r_dir)
  }

  # Create backup if requested
  if (backup && !dry_run) {
    backup_dir <- file.path(package_root, "R_backup")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir)
      file.copy(list.files(r_dir, full.names = TRUE), backup_dir)
      cat("✅ Backup created at:", backup_dir, "\n")
    }
  }

  # Define migration mapping
  migration_plan <- get_migration_plan()

  # Extract functions from source files
  extracted_functions <- extract_all_functions(r_dir)

  # Create new module files
  migration_results <- create_new_modules(r_dir, migration_plan, extracted_functions, dry_run)

  # Generate summary report
  generate_migration_report(migration_results, dry_run)

  return(migration_results)
}

#' Get Migration Plan
#'
#' @description Defines the complete migration mapping
#' @return List with migration plan
get_migration_plan <- function() {
  list(
    # Module: imports.R
    "imports.R" = list(
      sources = c("00_imports.R"),
      functions = c("%||%", "globalVariables"),
      description = "Package imports and global definitions"
    ),

    # Module: initialization.R
    "initialization.R" = list(
      sources = c("01-core-functions.R"),
      functions = c("initialize_covariate_search", "load_tags", "validate_setup", "initialize_search_config"),
      description = "Package initialization and setup"
    ),

    # Module: data-operations.R
    "data-operations.R" = list(
      sources = c("02-data.R"),
      functions = c("load_search_data", "load_search_config", "validate_search_inputs"),
      description = "Data loading and validation operations"
    ),

    # Module: database-management.R
    "database-management.R" = list(
      sources = c("01-core-functions.R", "03-database.R"),
      functions = c("initialize_search_database_core", "get_model_status", "get_model_ofv",
                    "get_model_covariates", "get_model_covariates_from_db", "update_model_counter",
                    "initialize_search_database", "create_comprehensive_table", "view_comprehensive_table"),
      description = "Search database operations and management"
    ),

    # Module: model-modification.R
    "model-modification.R" = list(
      sources = c("01-core-functions.R", "02-model-utilities.R"),
      functions = c("add_covariate_to_model", "model_add_cov", "fix_theta_renumbering",
                    "remove_covariate_from_model", "add_covariate_with_detailed_logging"),
      description = "Model modification operations (add/remove covariates)"
    ),

    # Module: model-discovery.R
    "model-discovery.R" = list(
      sources = c("01-core-functions.R", "03-database.R", "09-utils.R"),
      functions = c("discover_existing_models", "discover_existing_models_simple", "analyze_existing_models_yaml"),
      description = "Existing model discovery and analysis"
    ),

    # Module: file-io.R
    "file-io.R" = list(
      sources = c("02-model-utilities.R", "05-file-io.R", "01-core-functions.R"),
      functions = c("extract_model_results", "read_model_file", "write_model_file",
                    "read_nonmem_ext", "read_nonmem_lst", "get_model_status_from_files",
                    "get_model_ofv_from_files", "get_model_covariates_from_files"),
      description = "NONMEM file I/O operations"
    ),

    # Module: scm-algorithm.R
    "scm-algorithm.R" = list(
      sources = c("06-scm-core.R", "07-scm-steps.R"),
      functions = c("get_remaining_covariates", "get_dropped_covariates", "get_excluded_covariates",
                    "run_stepwise_covariate_modeling"),
      description = "Core SCM algorithm and workflow"
    ),

    # Module: scm-execution.R
    "scm-execution.R" = list(
      sources = c("06-scm-core.R", "07-scm-steps.R"),
      functions = c("run_univariate_step", "submit_and_wait_for_step"),
      description = "SCM step execution and model submission"
    ),

    # Module: scm-evaluation.R
    "scm-evaluation.R" = list(
      sources = c("06-scm-core.R", "07-scm-steps.R"),
      functions = c("select_best_model"),
      description = "Statistical model evaluation and selection"
    ),

    # Module: monitoring-progress.R
    "monitoring-progress.R" = list(
      sources = c("08-monitoring.R"),
      functions = c("get_scm_progress_summary", "print_scm_progress_report", "monitor_scm_progress",
                    "get_model_status_dashboard", "print_model_status_dashboard"),
      description = "Progress monitoring and reporting"
    ),

    # Module: monitoring-files.R
    "monitoring-files.R" = list(
      sources = c("09-recovery.R"),
      functions = c("read_ext_file", "read_ext_file_with_issues"),
      description = "File-based model monitoring"
    ),

    # Module: recovery-detection.R
    "recovery-detection.R" = list(
      sources = c("09-recovery.R"),
      functions = c("detect_estimation_problems", "generate_recovery_report"),
      description = "Estimation problem detection"
    ),

    # Module: recovery-actions.R
    "recovery-actions.R" = list(
      sources = c("09-recovery.R"),
      functions = c("create_retry_model", "adjust_theta_for_covariate",
                    "process_estimation_issues", "handle_failed_retry"),
      description = "Recovery actions and retry model creation"
    ),

    # Module: validation.R
    "validation.R" = list(
      sources = c("05-validation.R", "01-core-functions.R"),
      functions = c("validate_model_quality", "calculate_delta_ofv", "update_model_status_from_files",
                    "update_all_model_statuses"),
      description = "Model validation and quality assessment"
    ),

    # Module: utilities.R
    "utilities.R" = list(
      sources = c("09-utils.R"),
      functions = c("read_model_yaml", "analyze_model_covariates_yaml", "get_model_parent_yaml", "analyze_model_from_logic"),
      description = "Utility functions and helpers"
    )
  )
}

#' Extract All Functions from Source Files
#'
#' @description Parses R files and extracts function definitions with their complete code
#' @param r_dir Character. Path to R directory
#' @return List with function definitions by source file
extract_all_functions <- function(r_dir) {

  cat("📖 Extracting functions from source files...\n")

  # Get all R files
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)

  extracted <- list()

  for (file_path in r_files) {
    file_name <- basename(file_path)
    cat(sprintf("  Processing %s...", file_name))

    tryCatch({
      file_content <- readLines(file_path, warn = FALSE)
      functions <- extract_functions_from_file(file_content, file_name)
      extracted[[file_name]] <- functions
      cat(sprintf(" ✅ Found %d functions\n", length(functions)))
    }, error = function(e) {
      cat(sprintf(" ❌ Error: %s\n", e$message))
      extracted[[file_name]] <- list()
    })
  }

  return(extracted)
}

#' Extract Functions from Single File
#'
#' @description Parses a single R file and extracts function definitions
#' @param file_content Character vector. Lines from R file
#' @param file_name Character. Name of source file
#' @return List with function definitions
extract_functions_from_file <- function(file_content, file_name) {

  functions <- list()

  # Find function definitions (including assignment operators)
  function_pattern <- "^\\s*([a-zA-Z_][a-zA-Z0-9_\\.]*|`[^`]+`)\\s*(<-|=)\\s*function\\s*\\("

  i <- 1
  while (i <= length(file_content)) {
    line <- file_content[i]

    # Check if this line starts a function definition
    if (grepl(function_pattern, line)) {
      func_match <- regexpr(function_pattern, line)
      func_name <- gsub("\\s*(<-|=).*", "",
                        gsub("^\\s*", "",
                             substr(line, func_match, func_match + attr(func_match, "match.length") - 1)))
      func_name <- gsub("`", "", func_name)  # Remove backticks if present

      # Extract complete function definition
      func_definition <- extract_complete_function(file_content, i)

      if (!is.null(func_definition)) {
        functions[[func_name]] <- list(
          name = func_name,
          start_line = i,
          end_line = func_definition$end_line,
          code = func_definition$code,
          source_file = file_name
        )
        i <- func_definition$end_line + 1
      } else {
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }

  return(functions)
}

#' Extract Complete Function Definition
#'
#' @description Extracts complete function including roxygen comments and body
#' @param file_content Character vector. File lines
#' @param start_line Integer. Starting line number
#' @return List with function code and end line
extract_complete_function <- function(file_content, start_line) {

  # Find roxygen comments before function
  roxygen_start <- start_line
  for (i in (start_line - 1):1) {
    if (i < 1) break
    line <- trimws(file_content[i])
    if (grepl("^#'", line) || line == "") {
      roxygen_start <- i
    } else {
      break
    }
  }

  # Find function end by tracking braces
  brace_count <- 0
  in_function <- FALSE
  end_line <- start_line

  for (i in start_line:length(file_content)) {
    line <- file_content[i]

    # Start counting braces after we see 'function('
    if (grepl("function\\s*\\(", line)) {
      in_function <- TRUE
    }

    if (in_function) {
      # Count opening and closing braces
      open_braces <- lengths(regmatches(line, gregexpr("\\{", line)))
      close_braces <- lengths(regmatches(line, gregexpr("\\}", line)))
      brace_count <- brace_count + open_braces - close_braces

      # Function ends when brace count returns to 0
      if (brace_count == 0 && open_braces > 0) {
        end_line <- i
        break
      }
    }
  }

  # Extract complete function code
  func_code <- file_content[roxygen_start:end_line]

  return(list(
    code = func_code,
    end_line = end_line
  ))
}

#' Create New Module Files
#'
#' @description Creates new module files with migrated functions
#' @param r_dir Character. R directory path
#' @param migration_plan List. Migration plan
#' @param extracted_functions List. Extracted function definitions
#' @param dry_run Logical. Whether this is a dry run
#' @return List with migration results
create_new_modules <- function(r_dir, migration_plan, extracted_functions, dry_run) {

  cat("\n🔧 Creating new module files...\n")

  results <- list()

  for (module_name in names(migration_plan)) {
    module_info <- migration_plan[[module_name]]

    cat(sprintf("\n📝 Creating %s...\n", module_name))
    cat(sprintf("   Description: %s\n", module_info$description))

    # Create module content
    module_content <- create_module_content(module_name, module_info, extracted_functions)

    if (!dry_run) {
      # Write new module file
      module_path <- file.path(r_dir, module_name)
      writeLines(module_content, module_path)
      cat(sprintf("   ✅ Written to: %s\n", module_path))
    } else {
      cat(sprintf("   📋 Would create: %s (%d lines)\n", module_name, length(module_content)))
    }

    results[[module_name]] <- list(
      module_name = module_name,
      functions_migrated = module_info$functions,
      source_files = module_info$sources,
      lines_created = length(module_content),
      status = if (dry_run) "dry_run" else "created"
    )
  }

  return(results)
}

#' Create Module Content
#'
#' @description Creates content for a new module file
#' @param module_name Character. Module name
#' @param module_info List. Module information from migration plan
#' @param extracted_functions List. All extracted functions
#' @return Character vector with module content
create_module_content <- function(module_name, module_info, extracted_functions) {

  # Create module header
  header_comment <- sprintf("# =============================================================================")
  module_title <- sprintf("# %s", toupper(gsub("-", " ", gsub("\\.R$", "", module_name))))
  file_comment <- sprintf("# File: R/%s", module_name)
  package_comment <- "# Part of CovariateSearcher Package"
  description_comment <- sprintf("# %s", module_info$description)
  end_comment <- "# ============================================================================="

  content <- c(
    header_comment,
    module_title,
    file_comment,
    package_comment,
    description_comment,
    end_comment,
    ""
  )

  # Add functions
  for (func_name in module_info$functions) {
    func_found <- FALSE

    # Search for function in source files
    for (source_file in module_info$sources) {
      if (source_file %in% names(extracted_functions)) {
        if (func_name %in% names(extracted_functions[[source_file]])) {
          func_def <- extracted_functions[[source_file]][[func_name]]

          content <- c(content, "", func_def$code, "")
          func_found <- TRUE
          cat(sprintf("     ✅ Added %s() from %s\n", func_name, source_file))
          break
        }
      }
    }

    if (!func_found) {
      cat(sprintf("     ❌ Function %s() not found in sources: %s\n",
                  func_name, paste(module_info$sources, collapse = ", ")))
    }
  }

  return(content)
}

#' Generate Migration Report
#'
#' @description Generates summary report of migration results
#' @param migration_results List. Results from migration
#' @param dry_run Logical. Whether this was a dry run
generate_migration_report <- function(migration_results, dry_run) {

  cat("\n📊 MIGRATION SUMMARY\n")
  cat("====================\n")

  total_modules <- length(migration_results)
  total_functions <- sum(sapply(migration_results, function(x) length(x$functions_migrated)))
  total_lines <- sum(sapply(migration_results, function(x) x$lines_created))

  cat(sprintf("Modules created: %d\n", total_modules))
  cat(sprintf("Functions migrated: %d\n", total_functions))
  cat(sprintf("Total lines: %d\n", total_lines))

  if (dry_run) {
    cat("\n⚠️  This was a DRY RUN - no files were actually created\n")
    cat("Run with dry_run = FALSE to perform actual migration\n")
  } else {
    cat("\n✅ Migration completed successfully!\n")
    cat("Next steps:\n")
    cat("1. Update package NAMESPACE\n")
    cat("2. Test package loading\n")
    cat("3. Run package checks\n")
    cat("4. Remove old numbered files after validation\n")
  }

  cat("\n📋 Module Details:\n")
  for (module_name in names(migration_results)) {
    result <- migration_results[[module_name]]
    cat(sprintf("  %s: %d functions, %d lines\n",
                module_name, length(result$functions_migrated), result$lines_created))
  }
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

#' Example usage:
#'
#' # Dry run first (shows what would be done)
#' results <- migrate_package_structure(dry_run = TRUE)
#'
#' # Actual migration
#' results <- migrate_package_structure(dry_run = FALSE, backup = TRUE)
#'
#' # From specific directory
#' results <- migrate_package_structure("/path/to/CovariateSearcher", dry_run = FALSE)
