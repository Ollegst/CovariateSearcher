# =============================================================================
# MODEL MODIFICATION
# File: R/model-modification.R
# Part of CovariateSearcher Package
# Model modification operations (add/remove covariates)
# =============================================================================



#' Add Covariate to Model (FIXED - WITH PROPER LOG FILE SAVING)
#' @param search_state List. Current search state from initialize_covariate_search()
#' @param base_model_id Character. Base model identifier (e.g., "run1")
#' @param covariate_tag Character. Covariate tag to add (e.g., "beta_cl_wt")
#' @param step_number Integer. Optional step number (NULL for auto-calculation)
#' @return List with updated search_state and new model information
#' @export
add_covariate_to_model <- function(search_state, base_model_id, covariate_tag,
                                   step_number = NULL) {
  cat(sprintf("[+] Adding covariate %s to model %s\n", covariate_tag, base_model_id))

  # STEP 1: Validate inputs and calculate step number FIRST
  if (!covariate_tag %in% names(search_state$tags)) {
    stop("Covariate tag not found: ", covariate_tag)
  }

  covariate_name <- search_state$tags[[covariate_tag]]
  cat(sprintf("  Covariate: %s\n", covariate_name))

  # Calculate step number BEFORE incrementing counter
  final_step_number <- if (!is.null(step_number)) {
    step_number  # Use provided step_number
  } else {
    # Auto-calculate for manual additions
    if (is.null(search_state$search_database) || nrow(search_state$search_database) == 0) {
      1L
    } else {
      current_max <- max(search_state$search_database$step_number, na.rm = TRUE)
      if (is.na(current_max) || is.infinite(current_max)) 1L else current_max + 1L
    }
  }
  cat(sprintf("  Step number: %d\n", final_step_number))

  # Find covariate definition
  matching_cov <- search_state$covariate_search[
    grepl(paste0("_", covariate_name, "$"), search_state$covariate_search$cov_to_test), ]

  if (nrow(matching_cov) == 0) {
    stop("No matching covariate definition found for: ", covariate_name)
  }

  # STEP 2: Calculate model name and prepare for creation
  original_counter <- search_state$model_counter
  search_state$model_counter <- search_state$model_counter + 1
  new_model_name <- sprintf("run%d", search_state$model_counter)
  cat(sprintf("  New model: %s\n", new_model_name))

  # STEP 3: Comprehensive error handling wrapper
  tryCatch({
    # Sub-step 3a: Create BBR model
    cat("  Creating BBR model...\n")
    parent_path <- file.path(search_state$models_folder, base_model_id)

    if (!file.exists(paste0(parent_path, ".ctl")) && !file.exists(paste0(parent_path, ".mod"))) {
      stop("Parent model file not found: ", parent_path)
    }

    parent_mod <- bbr::read_model(parent_path)
    new_mod <- bbr::copy_model_from(
      .parent_mod = parent_mod,
      .new_model = new_model_name,
      .inherit_tags = TRUE,
      .overwrite = TRUE
    )

    new_mod <- bbr::add_tags(new_mod, .tags = search_state$tags[[covariate_tag]])
    step_info <- sprintf("Step %d + %s", final_step_number, search_state$tags[[covariate_tag]])
    new_mod <- bbr::replace_all_notes(new_mod, .notes = step_info)

    cat("  [OK] BBR model created\n")

    # Sub-step 3b: Add covariate to model file
    cat("  Adding covariate to model file...\n")
    cov_info <- matching_cov[1, ]
    cov_name <- cov_info$COVARIATE
    param_name <- cov_info$PARAMETER
    cov_on_param <- paste0(cov_name, "_", param_name)

    model_result <- model_add_cov(
      search_state = search_state,
      ref_model = new_model_name,
      cov_on_param = cov_on_param,
      id_var = search_state$idcol,
      data_file = search_state$data_file,
      covariate_search = search_state$covariate_search,
      capture_log = TRUE
    )

    search_state <- model_result$search_state
    technical_log <- model_result$log_entries
    cat("  [OK] Covariate added to model file\n")

    # FIXED: Save log file with standardized naming
    if (!is.null(technical_log) && length(technical_log) > 0) {
      log_filename <- file.path(search_state$models_folder,
                                paste0(new_model_name, "_add_", covariate_name, "_log.txt"))
      writeLines(technical_log, log_filename)
      cat(sprintf("  📝 Log saved: %s\n", basename(log_filename)))
    } else {
      cat("  ⚠️  No log entries captured\n")
    }

    # Sub-step 3c: Add to database (SIMPLIFIED SCHEMA)
    cat("  Adding to database...\n")
    yaml_path <- file.path(search_state$models_folder, new_model_name, paste0(new_model_name, ".yaml"))
    yaml_data <- yaml::read_yaml(yaml_path)
    actual_tags <- if (!is.null(yaml_data$tags)) {
      yaml_data$tags
    } else {
      character(0)
    }

    tryCatch({
      new_row <- data.frame(
        model_name = new_model_name,
        step_description = sprintf("Add %s", covariate_name),  # ← Move to position 2
        phase = "forward_selection",                           # ← Position 3
        step_number = final_step_number,                       # ← Position 4
        parent_model = base_model_id,                          # ← Position 5
        covariate_tested = covariate_name,
        action = "add_covariate",
        ofv = NA_real_,
        delta_ofv = NA_real_,
        rse_max = NA_real_,
        status = "created",
        tags = I(list(actual_tags)),
        submission_time = as.POSIXct(NA),
        completion_time = as.POSIXct(NA),
        retry_attempt = 0L,
        original_model = NA_character_,
        estimation_issue = NA_character_,
        excluded_from_step = FALSE,
        stringsAsFactors = FALSE
      )

      search_state$search_database <- dplyr::bind_rows(search_state$search_database, new_row)
      cat("  [OK] Added to database\n")

    }, error = function(db_error) {
      cat(sprintf("  [ERROR] Database insertion failed: %s\n", db_error$message))
      stop("Database insertion failed: ", db_error$message)
    })

    cat(sprintf("[OK] Model %s created successfully\n", new_model_name))

    return(list(
      status = "success",
      model_name = new_model_name,
      covariate_added = covariate_name,
      step_number = final_step_number,
      search_state = search_state,
      technical_log = technical_log,
      log_file = if(exists("log_filename")) log_filename else NULL
    ))

  }, error = function(e) {
    cat(sprintf("[X] Model creation failed: %s\n", e$message))

    # Rollback model counter
    search_state$model_counter <- original_counter
    cat(sprintf("  [ROLLBACK] Model counter restored to: %d\n", original_counter))

    return(list(
      status = "error",
      error_message = e$message,
      attempted_model = new_model_name,
      step_number = final_step_number,
      search_state = search_state
    ))
  })
  return(result)
}
#' Create Model Info Log File
#'
#' @title Create detailed log file for model creation
#' @description Creates a timestamped log file with model creation details
#' @param search_state List containing search state
#' @param model_name Character. New model name
#' @param parent_model Character. Parent model name
#' @param covariate_name Character. Covariate added
#' @param cov_info Data.frame. Covariate information from search definition
#' @return NULL (side effect: creates log file)
#' @export
create_model_info_log <- function(search_state, model_name, parent_model, covariate_name, cov_info) {

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_file <- file.path(search_state$models_folder, paste0(model_name, "_info.txt"))

  # Determine relationship type
  relationship_type <- dplyr::case_when(
    cov_info$STATUS == "con" & cov_info$FORMULA == "power" ~ "power relationship",
    cov_info$STATUS == "con" & cov_info$FORMULA == "linear" ~ "linear relationship",
    cov_info$STATUS == "cat" & cov_info$FORMULA == "linear" ~ "categorical relationship",
    TRUE ~ paste(cov_info$STATUS, cov_info$FORMULA, "relationship")
  )

  # Generate formula for display
  if (cov_info$STATUS == "con" & cov_info$FORMULA == "power") {
    formula_display <- sprintf("* (%s/%s)**THETA(3)", cov_info$COVARIATE, cov_info$REFERENCE)
  } else if (cov_info$STATUS == "con" & cov_info$FORMULA == "linear") {
    formula_display <- sprintf("* (1 + (%s-%s) * THETA(3))", cov_info$COVARIATE, cov_info$REFERENCE)
  } else if (cov_info$STATUS == "cat") {
    formula_display <- sprintf("* beta_%s_%s", cov_info$COVARIATE, cov_info$PARAMETER)
  } else {
    formula_display <- "See model file for details"
  }

  # Create log content
  log_content <- c(
    sprintf("[%s] Model %s created from %s", timestamp, model_name, parent_model),
    sprintf("[%s] Added covariate: %s (%s)", timestamp, covariate_name, relationship_type),
    sprintf("[%s] Parameter: %s, Covariate: %s", timestamp, cov_info$PARAMETER, cov_info$COVARIATE),
    sprintf("[%s] Reference value: %s", timestamp, cov_info$REFERENCE),
    sprintf("[%s] Formula: %s", timestamp, formula_display),
    sprintf("[%s] THETA added with initial value: 0.1", timestamp),
    sprintf("[%s] Status: created", timestamp),
    "",
    "Model Details:",
    sprintf("- Model file: %s.ctl", model_name),
    sprintf("- BBR YAML: %s.yaml", model_name),
    sprintf("- Parent model: %s", parent_model),
    sprintf("- Creation time: %s", timestamp),
    sprintf("- Covariate type: %s (%s)", cov_info$STATUS, cov_info$FORMULA),
    sprintf("- Time dependent: %s", cov_info$TIME_DEPENDENT)
  )

  # Write log file
  writeLines(log_content, log_file)
  cat(sprintf("  📝 Model info log created: %s\n", basename(log_file)))
}
#'
#' @title Core functionality to add covariate to NONMEM model file with enhanced logging
#' @description Modifies NONMEM control file to add covariate relationship with detailed logging
#' @param search_state List containing search state
#' @param ref_model Character. Model name to modify
#' @param cov_on_param Character. Combined covariate-parameter name (e.g., "WT_CL")
#' @param id_var Character. ID variable name (default: "ID")
#' @param data_file Data.frame. Dataset for time-varying checks
#' @param covariate_search Data.frame. Covariate search configuration
#' @param capture_log Function. Logging function (optional)
#' @return Updated search_state
#' @export
model_add_cov <- function(search_state, ref_model, cov_on_param, id_var = "ID",
                          data_file, covariate_search, capture_log = FALSE) {

  # Default logging function if none provided
  captured_log <- character(0)

  # Logging function that either prints or captures
  if (capture_log) {
    log_function <- function(msg) {
      entry <- paste(Sys.time(), "-", msg)
      captured_log <<- c(captured_log, entry)
      cat(entry, "\n")
    }
  } else {
    log_function <- function(msg) cat(paste(Sys.time(), "-", msg, "\n"))
  }

  log_function(paste("=== Adding Covariate to Model File ==="))
  log_function(paste("Model:", ref_model))
  log_function(paste("Covariate parameter:", cov_on_param))

  modelcode <- read_model_file(search_state, ref_model)
  original_file_path <- attr(modelcode, "file_path")
  log_function(paste("Read model file:", basename(original_file_path)))

  cov_on_param <- paste0("beta_", cov_on_param)

  cova <- covariate_search$COVARIATE[covariate_search$cov_to_test == cov_on_param]
  param <- covariate_search$PARAMETER[covariate_search$cov_to_test == cov_on_param]
  ref <- covariate_search$REFERENCE[covariate_search$cov_to_test == cov_on_param]

  log_function(paste("Covariate:", cova, "Parameter:", param, "Reference:", ref))

  # Get max THETA number
  thetas <- modelcode[grepl('THETA\\(..?\\)', modelcode)] %>%
    gsub(pattern = '.*THETA\\(', replacement = '') %>%
    gsub(pattern = '\\).*', replacement = '') %>%
    as.double()

  newtheta <- max(thetas) + 1
  log_function(paste("Current max THETA:", max(thetas), "New THETA number:", newtheta))

  temp_cov <- dplyr::filter(covariate_search, cov_to_test == cov_on_param)

  # Determine covariate type and formula
  cov_status <- temp_cov$STATUS[temp_cov$COVARIATE == cova]
  cov_formula <- temp_cov$FORMULA[temp_cov$COVARIATE == cova]

  log_function(paste("Covariate type:", cov_status, "Formula:", cov_formula))

  FLAG <- dplyr::case_when(
    length(cov_status) == 0 | length(cov_formula) == 0 ~ "ERROR: Missing covariate info",
    cov_status == "cat" & cov_formula == "linear" ~ "1",
    cov_status == "cat" & cov_formula == "power" ~ "2",
    cov_status == "con" & cov_formula == "linear" ~ "3",
    cov_status == "con" & cov_formula == "power" ~ "2",
    cov_status == "con" & cov_formula == "power1" ~ "5",
    cov_status == "con" & cov_formula == "power0.75" ~ "6",
    .default = "Please check covariate status and formula"
  )

  log_function(paste("Covariate FLAG:", FLAG))

  initialValuethetacov <- dplyr::case_when(
    FLAG == "5" ~ "1 FIX",
    FLAG == "6" ~ "0.75 FIX",
    .default = "0.1"
  )

  log_function(paste("Initial THETA value:", initialValuethetacov))

  # Generate formula based on FLAG
  if(FLAG == "2") formule <- paste0(' * (',cova,'/',ref,')**THETA(', newtheta ,')')
  if(FLAG == "3") formule <- paste0(' * (1 + (',cova,'-',ref, ') * THETA(',newtheta ,'))')
  if(FLAG == "5") formule <- paste0(' * (',cova,'/',ref,')** THETA(', newtheta ,')')
  if(FLAG == "6") formule <- paste0(' * (',cova,'/',ref,')** THETA(', newtheta ,')')

  # Handle categorical covariates (simplified for core module)
  thetanmulti <- tibble()
  if(FLAG == "1"){
    # Try to load lookup.yaml for categorical labels (optional)
    lookup_values <- NULL
    lookup_file <- file.path("data", "spec", "lookup.yaml")
    if (file.exists(lookup_file)) {
      tryCatch({
        lookup_data <- yaml::read_yaml(lookup_file)
        # Check if this covariate exists in lookup
        if (cova %in% names(lookup_data)) {
          lookup_info <- lookup_data[[cova]]
          # Convert values and decode lists to a named list
          if (!is.null(lookup_info$values) && !is.null(lookup_info$decode)) {
            lookup_values <- setNames(
              as.list(lookup_info$decode),
              as.character(lookup_info$values)
            )
            log_function(paste("Found lookup values for", cova, "in lookup.yaml"))
          }
        }
      }, error = function(e) {
        log_function(paste("Note: Could not use lookup.yaml:", e$message))
      })
    }
    uniqueval <- unique(data_file[[cova]])
    newvari <- cov_on_param
    formule <- paste0(' * ', newvari)
    ifelcode <- paste0('IF(', cova, '.EQ.', temp_cov$REFERENCE,')THEN\n', newvari, ' = 1\n' )

    uniqueval_with_freq <- data_file %>%
      dplyr::count(!!rlang::parse_expr(cova)) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::pull(!!rlang::parse_expr(cova))

    uniqueval <- uniqueval_with_freq[uniqueval_with_freq != temp_cov$REFERENCE]

    thetanmulti <- tibble(
      covx = uniqueval,
      label = character(length(uniqueval))  # Initialize empty
    )
    for(a in 1:length(uniqueval)){
      current_level <- uniqueval[a]
      theta_num <- newtheta + a - 1

      # Determine the label to use for this level
      if (!is.null(lookup_values) && as.character(current_level) %in% names(lookup_values)) {
        # Use lookup value if available
        category_label <- lookup_values[[as.character(current_level)]]
        # Clean the label for use in parameter names
        category_label <- gsub("[^A-Za-z0-9]", "_", category_label)
        category_label <- gsub("_+", "_", category_label)  # Remove multiple underscores
        category_label <- gsub("^_|_$", "", category_label)  # Remove leading/trailing underscores
        category_label <- toupper(category_label)  # Convert to uppercase
        thetanmulti$label[a] <- category_label
        log_function(paste("  Using lookup label for category", current_level, ":", category_label))
      } else {
        # Fall back to numeric label
        thetanmulti$label[a] <- as.character(current_level)
        log_function(paste("  Using numeric label for category", current_level))
      }

      ifelcode <- paste0(ifelcode,
                         'ELSEIF(', cova, '.EQ.', current_level, ')THEN\n',
                         newvari, ' = 1 + THETA(', theta_num, ')\n')

      log_function(paste("Added ELSEIF for", cova, "=", current_level,
                         "using THETA(", theta_num, ")"))
    }
    ifelcode <- paste0(ifelcode, 'ENDIF\n')
    modelcode[grep('^\\$PK', modelcode)] <- paste0(modelcode[grep('^\\$PK', modelcode)], '\n\n', ifelcode)
    #}
  }

  # Generate formula based on FLAG
  if(FLAG == "2") formule <- paste0(' * (',cova,'/',ref,')**THETA(', newtheta ,')')
  if(FLAG == "3") formule <- paste0(' * (1 + (',cova,'-',ref, ') * THETA(',newtheta ,'))')
  if(FLAG == "5") formule <- paste0(' * (',cova,'/',ref,')** THETA(', newtheta ,')')
  if(FLAG == "6") formule <- paste0(' * (',cova,'/',ref,')** THETA(', newtheta ,')')


  log_function(paste("Generated formula:", formule))

  # Check if time-dependent
  max_levels <- max(tapply(data_file[[cova]], data_file[[id_var]],
                           function(x) length(unique(x))), na.rm = TRUE)
  time_varying <- max_levels > 1

  log_function(paste("Time-varying check:", time_varying, "(max levels:", max_levels, ")"))

  if(time_varying == FALSE){
    linetu <- grep(paste0('^\\s*TV_', param), modelcode)
    search_pattern <- paste0('^\\s*TV_', param)
  } else {
    linetu <- grep(paste0('^\\s*', param), modelcode)
    search_pattern <- paste0('^\\s*', param)
  }

  log_function(paste("Looking for parameter line with pattern:", search_pattern))
  log_function(paste("Found parameter line at index:", linetu))

  if (length(linetu) > 0) {
    original_line <- modelcode[linetu]
    log_function(paste("Original line:", original_line))

    # Add covariate to parameter line
    if(grepl(";", modelcode[linetu])){
      modelcode[linetu] <- sub("(.*?)(\\s*;)", paste0("\\1", formule, "\\2"), modelcode[linetu])
    } else {
      modelcode[linetu] <- paste0(modelcode[linetu], formule)
    }

    log_function(paste("Modified line:", modelcode[linetu]))
    log_function(paste("✓ Line", linetu, "modified successfully"))
  } else {
    log_function(paste("ERROR: Parameter line not found for pattern:", search_pattern))
  }

  # Add THETA line
  if(nrow(thetanmulti) > 0){
    # Multiple THETAs for categorical with >2 levels (excluding reference)
    newthetalines <- purrr::map_chr(1:nrow(thetanmulti), ~ {
      # Use the label from thetanmulti which now contains lookup values or numbers
      paste0('0.1 ; ', cov_on_param, "_", thetanmulti$label[[.x]], ';  ; RATIO')
    })
    newthetaline <- paste0(newthetalines, collapse = '\n')

    log_function(paste("Adding", nrow(thetanmulti), "THETA lines for categorical levels"))
  } else {
    # Single THETA for binary or continuous covariates
    newthetaline <- paste0(initialValuethetacov, ' ; ', cov_on_param, ' ;  ; RATIO')
  }

  log_function(paste("New THETA line to add:", newthetaline))

  lineomeg <- grep('\\$OMEGA', modelcode)[1]
  log_function(paste("Inserting THETA line before $OMEGA section at line:", lineomeg))

  modelcode <- c(
    modelcode[1:(lineomeg - 1)],
    newthetaline,
    modelcode[lineomeg:length(modelcode)]
  )

  log_function(paste("✓ THETA line added at position:", lineomeg))
  log_function(paste("Total lines in model:", length(modelcode)))

  # Write back
  attr(modelcode, "file_path") <- original_file_path
  search_state <- write_model_file(search_state, modelcode)
  log_function(paste("✓ Model file written successfully"))

  log_function(paste("=== Covariate Addition Complete ==="))
  if (capture_log) {
    return(list(search_state = search_state, log_entries = captured_log))
  } else {
    return(search_state)
  }
}



#' Fix THETA Renumbering
#'
#' @title Renumber THETA parameters after removing some
#' @description Adjusts THETA numbering throughout model file after removing THETAs
#' @param modelcode Character vector. Model file lines
#' @param theta_numbers_to_remove Numeric vector. THETA numbers that were removed
#' @param log_function Function. Logging function
#' @return Character vector. Updated model code with renumbered THETAs
#' @export
fix_theta_renumbering <- function(modelcode, theta_numbers_to_remove, log_function) {

  log_function("=== Starting THETA Renumbering ===")
  log_function(paste("Removing THETA numbers:", paste(theta_numbers_to_remove, collapse = ", ")))

  # Step 1: Find all unique THETA numbers currently in the model
  all_theta_nums <- c()
  for (line in modelcode) {
    if (grepl("THETA\\(", line)) {
      matches <- regmatches(line, gregexpr("THETA\\((\\d+)\\)", line))[[1]]
      if (length(matches) > 0) {
        nums <- as.numeric(gsub("THETA\\((\\d+)\\)", "\\1", matches))
        all_theta_nums <- c(all_theta_nums, nums)
      }
    }
  }

  all_theta_nums <- unique(all_theta_nums)
  all_theta_nums <- sort(all_theta_nums)

  log_function(paste("Found THETA numbers in model:", paste(all_theta_nums, collapse = ", ")))

  # Step 2: Calculate final mapping for REMAINING theta numbers
  # Key fix: Only map THETA numbers that still exist in the model
  final_mapping <- list()

  for (old_num in all_theta_nums) {
    # Calculate how many removed THETAs were before this one
    adjustment <- sum(theta_numbers_to_remove <= old_num)
    new_num <- old_num - adjustment

    # Only create mapping if the number actually changes
    if (old_num != new_num) {
      final_mapping[[as.character(old_num)]] <- new_num
      log_function(paste("THETA(", old_num, ") -> THETA(", new_num, ")"))
    } else {
      log_function(paste("THETA(", old_num, ") remains unchanged"))
    }
  }

  # Step 3: Apply renumbering only if there are mappings to make
  if (length(final_mapping) > 0) {
    log_function("Converting to temporary placeholders...")

    # Convert to temporary placeholders first
    for (old_str in names(final_mapping)) {
      old_num <- as.numeric(old_str)
      temp_placeholder <- paste0("TEMP_THETA_", old_num, "_TEMP")
      old_pattern <- paste0("THETA(", old_num, ")")

      for (i in 1:length(modelcode)) {
        if (grepl(old_pattern, modelcode[i], fixed = TRUE)) {
          old_line <- modelcode[i]
          modelcode[i] <- gsub(old_pattern, temp_placeholder, modelcode[i], fixed = TRUE)
          log_function(paste("Line", i, "converted to placeholder:"))
          log_function(paste("  Before:", old_line))
          log_function(paste("  After: ", modelcode[i]))
        }
      }
    }

    log_function("Converting placeholders to final THETA numbers...")

    # Convert placeholders to final THETA numbers
    for (old_str in names(final_mapping)) {
      old_num <- as.numeric(old_str)
      new_num <- final_mapping[[old_str]]
      temp_placeholder <- paste0("TEMP_THETA_", old_num, "_TEMP")
      final_pattern <- paste0("THETA(", new_num, ")")

      for (i in 1:length(modelcode)) {
        if (grepl(temp_placeholder, modelcode[i], fixed = TRUE)) {
          old_line <- modelcode[i]
          modelcode[i] <- gsub(temp_placeholder, final_pattern, modelcode[i], fixed = TRUE)
          log_function(paste("Line", i, "converted to final:"))
          log_function(paste("  Before:", old_line))
          log_function(paste("  After: ", modelcode[i]))
        }
      }
    }
  } else {
    log_function("No THETA renumbering needed - all remaining THETAs keep their positions")
  }

  log_function("✓ THETA renumbering completed")
  return(modelcode)
}



#' Remove Covariate from Model (Clean Interface)
#'
#' @title Remove covariate using tag name with functional state update
#' @description Removes a covariate from a model using tag-based interface.
#'   Returns updated search_state.
#' @param search_state List containing search state
#' @param model_name Character. Model name to modify
#' @param covariate_tag Character. Covariate tag to remove (e.g., "cov_cl_race")
#' @param save_as_new_model Logical. Whether to create new model (default: TRUE)
#' @return List with updated search_state and operation details
#' @export
remove_covariate_from_model <- function(search_state, model_name, covariate_tag, save_as_new_model = TRUE) {

  cat(sprintf("[-] Removing covariate %s from model %s", covariate_tag, model_name))

  # Step 1: Convert tag to beta format (consistent with add_covariate)
  if (!covariate_tag %in% names(search_state$tags)) {
    stop("Covariate tag not found: ", covariate_tag)
  }

  covariate_value <- search_state$tags[[covariate_tag]]

  # Find matching covariate in search definition
  matching_cov <- search_state$covariate_search[
    grepl(paste0("_", covariate_value, "$"), search_state$covariate_search$cov_to_test), ]

  if (nrow(matching_cov) == 0) {
    stop("No matching covariate definition found for: ", covariate_value)
  }

  # Get the beta format name and covariate info
  covariate_to_remove <- matching_cov$cov_to_test[1]  # e.g., "beta_RACE_CL"
  cov_info <- matching_cov[1, ]
  cova <- cov_info$COVARIATE
  param <- cov_info$PARAMETER

  # Initialize logging
  log_messages <- c()
  log_msg <- function(msg) {
    log_messages <<- c(log_messages, paste(Sys.time(), "-", msg))
    if (length(log_messages) %% 10 == 0) {
      cat(".")
    }
  }

  log_msg(paste("=== Removing Covariate:", covariate_value, "==="))
  log_msg(paste("Model:", model_name, "-> Beta format:", covariate_to_remove))

  # Step 2: Create new model with BBR if requested
  if (save_as_new_model) {
    search_state$model_counter <- search_state$model_counter + 1
    new_model_name <- paste0("run", search_state$model_counter)
    log_msg(paste("Creating new BBR model:", new_model_name))
    tryCatch({
      parent_path <- file.path(search_state$models_folder, model_name)
      new_mod <- bbr::copy_model_from(
        .parent_mod = bbr::read_model(parent_path),
        .new_model = new_model_name,
        .inherit_tags = TRUE
      )

      # Remove the covariate tag from BBR model
      covariate_tag_value <- paste0(cova, "_", param)
      if (covariate_tag_value %in% new_mod$tags) {
        new_mod <- bbr::remove_tags(new_mod, covariate_tag_value)
        log_msg(paste("Removed BBR tag:", covariate_tag_value))
      }

      # Add notes about the removal
      new_mod <- bbr::add_notes(new_mod, paste0("- ", covariate_tag_value))
      log_msg(paste("Added removal note:", paste0("- ", covariate_tag_value)))

      log_msg(paste("✓ BBR model created:", new_model_name))
    }, error = function(e) {
      log_msg(paste("Error creating BBR model:", e$message))
      stop("Failed to create BBR model: ", e$message)
    })
    model_to_modify <- new_model_name
  } else {
    model_to_modify <- model_name
    log_msg(paste("Modifying existing model:", model_to_modify))
  }

  # Step 3: Read and modify the model file
  modelcode <- read_model_file(search_state, model_to_modify)
  original_file_path <- attr(modelcode, "file_path")

  # Step 4: FIXED THETA DETECTION - Count actual THETA parameters, not line positions
  theta_start <- grep("^\\$THETA", modelcode)
  next_section <- grep("^\\$", modelcode)
  theta_end <- next_section[next_section > theta_start[1]][1] - 1
  if (is.na(theta_end)) theta_end <- length(modelcode)

  theta_numbers_to_remove <- c()
  theta_lines_to_remove <- c()

  # Count actual THETA parameters (skip empty lines and comments)
  theta_count <- 0
  for (i in (theta_start + 1):theta_end) {
    line <- modelcode[i]

    # Skip empty lines and comment-only lines
    if (grepl("^\\s*$", line) || grepl("^\\s*;", line)) {
      next
    }

    # This is an actual THETA parameter line
    theta_count <- theta_count + 1

    # Check if this THETA contains our covariate
    if (grepl(paste0("\\b", covariate_to_remove, "\\b"), line) ||
        grepl(paste0("\\b", covariate_to_remove, "_[^\\s;]+"), line)) {

      theta_numbers_to_remove <- c(theta_numbers_to_remove, theta_count)
      theta_lines_to_remove <- c(theta_lines_to_remove, i)
      log_msg(paste("Found THETA", theta_count, "at line", i, ":", trimws(line)))
    }
  }

  if (length(theta_numbers_to_remove) == 0) {
    stop("No THETA parameters found for covariate: ", covariate_to_remove)
  }

  theta_numbers_to_remove <- sort(theta_numbers_to_remove)
  log_msg(paste("THETA numbers to remove:", paste(theta_numbers_to_remove, collapse = ", ")))

  # Step 5: Remove covariate effects from parameter equations
  lines_modified <- 0
  for (i in 1:length(modelcode)) {
    line <- modelcode[i]

    if (grepl(cova, line)) {
      original_line <- line
      modified_line <- line
      line_changed <- FALSE

      for (theta_num in theta_numbers_to_remove) {
        # Pattern 1: * (1 + COVARIATE * THETA(X))
        pattern1 <- paste0("\\s*\\*\\s*\\(1\\s*\\+\\s*", cova, "\\s*\\*\\s*THETA\\(", theta_num, "\\)\\)")
        # Pattern 2: * (COVARIATE/REF)**THETA(X)
        pattern2 <- paste0("\\s*\\*\\s*\\(", cova, "/[0-9\\.]+\\)\\*\\*THETA\\(", theta_num, "\\)")
        # Pattern 3: * COVARIATE_VARIABLE
        pattern3 <- paste0("\\s*\\*\\s*", covariate_to_remove, "\\b")

        if (grepl(pattern1, modified_line)) {
          modified_line <- gsub(pattern1, "", modified_line)
          line_changed <- TRUE
        } else if (grepl(pattern2, modified_line)) {
          modified_line <- gsub(pattern2, "", modified_line)
          line_changed <- TRUE
        } else if (grepl(pattern3, modified_line)) {
          modified_line <- gsub(pattern3, "", modified_line)
          line_changed <- TRUE
        }
      }

      if (line_changed) {
        modelcode[i] <- modified_line
        lines_modified <- lines_modified + 1
      }
    }
  }

  log_msg(paste("Modified", lines_modified, "parameter lines"))

  # Step 6: Remove IF-THEN-ELSE blocks for categorical covariates
  if (cov_info$STATUS == "cat") {
    pk_start <- grep("^\\$PK", modelcode)
    if (length(pk_start) > 0) {
      pk_end <- grep("^\\$", modelcode)
      pk_end <- pk_end[pk_end > pk_start[1]][1] - 1
      if (is.na(pk_end)) pk_end <- length(modelcode)

      lines_to_remove <- c()

      # Find the line that contains our specific beta assignment
      for (i in pk_start:pk_end) {
        if (grepl(paste0("\\b", covariate_to_remove, "\\s*="), modelcode[i])) {
          # Found a line that sets our beta variable
          # Now find the complete IF-THEN-ELSE block containing this line

          # Search backwards for the IF statement (FIXED: proper sequence)
          block_start <- NULL
          for (j in seq(from = i, to = pk_start, by = -1)) {
            if (grepl(paste0("IF\\s*\\(", cova, "\\.EQ\\."), modelcode[j])) {
              # Found the start of our block
              block_start <- j
              break
            }
          }

          # Search forwards for ENDIF from the block start
          if (!is.null(block_start)) {
            for (k in block_start:pk_end) {
              lines_to_remove <- c(lines_to_remove, k)
              if (grepl("ENDIF", modelcode[k])) {
                # Found the end
                break
              }
            }
          }
          break  # We found and processed our block
        }
      }

      if (length(lines_to_remove) > 0) {
        modelcode <- modelcode[-lines_to_remove]
        # Adjust THETA line indices after removing IF blocks
        theta_lines_to_remove <- theta_lines_to_remove - sapply(theta_lines_to_remove, function(x) sum(lines_to_remove < x))
        log_msg(paste("Removed", length(lines_to_remove), "lines from IF-THEN-ELSE block for", covariate_to_remove))
      }
    }
  }

  # Step 7: Remove THETA lines (in reverse order)
  for (line_idx in rev(theta_lines_to_remove)) {
    if (line_idx > 0 && line_idx <= length(modelcode)) {
      log_msg(paste("Removing THETA line:", trimws(modelcode[line_idx])))
      modelcode <- modelcode[-line_idx]
    }
  }

  # Step 8: Apply THETA renumbering
  log_msg("Starting THETA renumbering...")
  modelcode <- fix_theta_renumbering(modelcode, theta_numbers_to_remove, log_msg)

  # Step 9: Write the modified model file
  attr(modelcode, "file_path") <- original_file_path
  search_state <- write_model_file(search_state, modelcode)
  log_msg(paste("Model file updated:", basename(original_file_path)))

  # Step 10: Calculate remaining tags after covariate removal
  parent_row <- search_state$search_database[search_state$search_database$model_name == model_name, ]
  parent_tags <- if (nrow(parent_row) > 0 && !is.null(parent_row$tags[[1]])) parent_row$tags[[1]] else character(0)
  covariate_tag_value <- paste0(cova, "_", param)
  remaining_tags <- setdiff(parent_tags, covariate_tag_value)
  if (save_as_new_model) {
    log_msg(paste("Parent tags:", paste(parent_tags, collapse = ", ")))
    log_msg(paste("Removing tag:", covariate_tag_value))
    log_msg(paste("Remaining tags:", paste(remaining_tags, collapse = ", ")))
  }

  # Step 11: Update database
  final_model_name <- if (save_as_new_model) new_model_name else model_name

  if (save_as_new_model) {
    new_row <- data.frame(
      model_name = new_model_name,
      step_description = sprintf("Remove %s", covariate_value),
      phase = "covariate_removal",
      step_number = max(search_state$search_database$step_number, na.rm = TRUE) + 1,
      parent_model = model_name,
      covariate_tested = covariate_value,
      action = "remove_single_covariate",
      ofv = NA_real_,
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      status = "created",
      tags = I(list(character(0))),
      submission_time = as.POSIXct(NA),
      completion_time = as.POSIXct(NA),
      retry_attempt = 0L,
      original_model = NA_character_,
      estimation_issue = NA_character_,
      excluded_from_step = FALSE,
      stringsAsFactors = FALSE
    )

    search_state$search_database <- dplyr::bind_rows(search_state$search_database, new_row)
    log_msg(paste("Added", new_model_name, "to database"))
  }

  # Step 12: Save detailed log
  log_file <- file.path(search_state$models_folder,
                        paste0(final_model_name, "_remove_", covariate_value, "_log.txt"))
  writeLines(log_messages, log_file)

  cat(" Complete. Created", final_model_name, "\n")
  log_msg(paste("✓ Covariate", covariate_value, "successfully removed"))
  log_msg(paste("Final model:", final_model_name))
  log_msg(paste("Database updated: now has", nrow(search_state$search_database), "models"))

  return(list(
    status = "success",
    model_name = final_model_name,
    covariate_removed = covariate_value,
    search_state = search_state
  ))
}




