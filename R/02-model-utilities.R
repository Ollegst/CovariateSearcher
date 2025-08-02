#' Extract Model Results
#'
#' @title Extract comprehensive model results from output files
#' @description Extracts OFV, parameters, and other results from NONMEM output
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return List with extracted results
#' @export
extract_model_results <- function(search_state, model_name) {

  status <- get_model_status(search_state, model_name)
  ofv <- get_model_ofv(search_state, model_name)

  # Basic result structure
  results <- list(
    model_name = model_name,
    status = status,
    ofv = ofv,
    parameters = NULL,
    rse_values = NULL,
    extraction_time = Sys.time()
  )

  # TODO: Add parameter and RSE extraction when .ext parsing is implemented
  # For now, return basic results

  return(results)
}

#' Read Model File
#'
#' @title Read NONMEM control file with proper path handling
#' @description Reads model control file (.ctl or .mod) and stores file path as attribute
#' @param search_state List containing search state
#' @param run_name Character. Model name
#' @param extensions Character vector. File extensions to try (default: c(".ctl", ".mod"))
#' @return Character vector of model file lines with file_path attribute
#' @export
read_model_file <- function(search_state, run_name, extensions = c(".ctl", ".mod")) {
  base_path <- file.path(search_state$models_folder, run_name)
  for (ext in extensions) {
    file_path <- paste0(base_path, ext)
    if (file.exists(file_path)) {
      lines <- readLines(file_path, warn = FALSE)
      attr(lines, "file_path") <- file_path
      return(lines)
    }
  }
  stop("No file found for ", run_name, " with extensions: ", paste(extensions, collapse = ", "))
}

#' Write Model File
#'
#' @title Write modified NONMEM control file back to disk
#' @description Writes model file lines back to original location using stored file_path attribute
#' @param search_state List containing search state (unused but kept for consistency)
#' @param lines Character vector. Model file lines with file_path attribute
#' @return Updated search_state (unchanged)
#' @export
write_model_file <- function(search_state, lines) {
  file_path <- attr(lines, "file_path")
  if (is.null(file_path)) {
    stop("No file path found. Make sure the lines were read using read_model_file()")
  }
  writeLines(lines, file_path)
  cat("File saved to:", basename(file_path), "\n")
  return(search_state)
}

#' Add Covariate to Model File
#'
#' @title Core functionality to add covariate to NONMEM model file with enhanced logging
#' @description Modifies NONMEM control file to add covariate relationship with detailed logging
#' @param search_state List containing search state
#' @param ref_model Character. Model name to modify
#' @param cov_on_param Character. Combined covariate-parameter name (e.g., "WT_CL")
#' @param id_var Character. ID variable name (default: "ID")
#' @param data_file Data.frame. Dataset for time-varying checks
#' @param covariate_search Data.frame. Covariate search configuration
#' @param log_function Function. Logging function (optional)
#' @return Updated search_state (unchanged - file modification is side effect)
#' @export
model_add_cov <- function(search_state, ref_model, cov_on_param, id_var = "ID",
                          data_file, covariate_search, log_function = NULL) {

  # Default logging function if none provided
  if (is.null(log_function)) {
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
  if(FLAG == "1"){
    uniqueval <- unique(data_file[[cova]])
    if(length(uniqueval) == 2 & sum(uniqueval) == 1 ){
      formule <- paste0(' * (1 + THETA(', newtheta ,') * ',cova ,')')
    } else {
      newvari <- cov_on_param
      formule <- paste0(' * ', newvari)
      ifelcode <- paste0('IF(', cova, '.EQ.', temp_cov$REFERENCE,')THEN\n', newvari, ' = 1\n' )

      uniqueval <- data_file %>%
        dplyr::count(!!rlang::parse_expr(cova)) %>%   # count() instead of group_by + tally
        dplyr::arrange(desc(n)) %>%
        dplyr::pull(!!rlang::parse_expr(cova))

      thetanmulti <- tibble(covx = uniqueval[uniqueval != temp_cov$REFERENCE])
      for(a in 2:length(uniqueval)){
        ifelcode <- paste0(ifelcode, 'ELSEIF(', cova, '.EQ.', uniqueval[[a]], ')THEN\n',
                           newvari, ' = 1 + THETA(', newtheta + a -2, ')\n')
      }
      ifelcode <- paste0(ifelcode, 'ENDIF\n')
      modelcode[grep('^\\$PK', modelcode)] <- paste0(modelcode[grep('^\\$PK', modelcode)], '\n\n', ifelcode)
    }
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
  newthetaline <- paste0(initialValuethetacov, ' ; ', cov_on_param, ' ;  ; RATIO')
  if(nrow(thetanmulti) > 0){
    newthetaline <- purrr::map_chr(1:nrow(thetanmulti), ~ paste0('0.1 ; ', paste0(cov_on_param,"_",thetanmulti$covx[[.x]]), ';  ; RATIO')) %>%
      paste0(collapse = '\n')
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
  return(search_state)
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

#' Remove Covariate from Model
#'
#' @title Remove covariate from NONMEM model file with detailed logging
#' @description Removes covariate relationship from model file and updates database
#' @param search_state List containing search state
#' @param model_name Character. Base model name
#' @param covariate_to_remove Character. Covariate to remove (e.g., "beta_WT_CL")
#' @param save_as_new_model Logical. Whether to create new model (default: TRUE)
#' @return List with updated search_state and new model information
#' @export
remove_covariate_from_model <- function(search_state, model_name, covariate_to_remove, save_as_new_model = TRUE) {

  # Initialize log
  log_messages <- c()
  log_msg <- function(msg) {
    log_messages <<- c(log_messages, paste(Sys.time(), "-", msg))
    if (length(log_messages) %% 10 == 0) {  # Only show progress every 10 steps
      cat(".")
    }
  }

  cat("Removing", covariate_to_remove, "from", model_name, "...")

  log_msg(paste("=== Removing Covariate with BBR Integration ==="))
  log_msg(paste("Model:", model_name))
  log_msg(paste("Removing covariate:", covariate_to_remove))

  # Find the covariate info
  cov_info <- search_state$covariate_search[search_state$covariate_search$cov_to_test == covariate_to_remove, ]
  if (nrow(cov_info) == 0) {
    stop("Covariate ", covariate_to_remove, " not found in covariate_search")
  }

  cova <- cov_info$COVARIATE
  param <- cov_info$PARAMETER

  log_msg(paste("Covariate name:", cova, "Parameter:", param))

  # Step 1: Create new model with BBR if requested
  if (save_as_new_model) {
    new_model_name <- paste0("run", search_state$model_counter + 1)
    search_state$model_counter <- search_state$model_counter + 1

    log_msg(paste("Creating new BBR model:", new_model_name))

    # Create new model using BBR
    tryCatch({
      parent_path <- file.path(search_state$models_folder, model_name)
      new_mod <- bbr::copy_model_from(
        .parent_mod = bbr::read_model(parent_path),
        .new_model = new_model_name,
        .inherit_tags = TRUE
      )

      log_msg(paste("Created BBR model with tags:", paste(new_mod$tags, collapse = ", ")))

      # Find and remove the covariate tag
      # The tag should match the pattern: COVARIATE_PARAMETER (e.g., WT_CL)
      covariate_tag_value <- paste0(cova, "_", param)  # e.g., "WT_CL"

      log_msg(paste("Looking for tag to remove:", covariate_tag_value))

      if (covariate_tag_value %in% new_mod$tags) {
        log_msg(paste("Found tag in model:", covariate_tag_value))
        # Use the working BBR syntax: remove_tags(model, tag)
        new_mod <- bbr::remove_tags(new_mod, covariate_tag_value)
        log_msg(paste("Removed tag:", covariate_tag_value))
      } else {
        log_msg(paste("Warning: Tag", covariate_tag_value, "not found in model tags"))
        log_msg(paste("Available tags in model:", paste(new_mod$tags, collapse = ", ")))
      }

      log_msg(paste("Final model tags:", paste(new_mod$tags, collapse = ", ")))
      log_msg(paste("✓ BBR model created successfully:", new_model_name))
    }, error = function(e) {
      log_msg(paste("Error creating BBR model:", e$message))
      stop("Failed to create BBR model: ", e$message)
    })

    model_to_modify <- new_model_name
  } else {
    model_to_modify <- model_name
    log_msg(paste("Modifying existing model:", model_to_modify))
  }

  # Step 2: Read and modify the model file
  modelcode <- read_model_file(search_state, model_to_modify)
  original_file_path <- attr(modelcode, "file_path")

  # Step 3: Find THETA numbers that belong to this covariate
  theta_start <- grep("^\\$THETA", modelcode)
  next_section <- grep("^\\$", modelcode)
  theta_end <- next_section[next_section > theta_start[1]][1] - 1
  if (is.na(theta_end)) theta_end <- length(modelcode)

  theta_numbers_to_remove <- c()
  theta_lines_to_remove <- c()

  # Look for THETA lines that match our covariate pattern
  for (i in (theta_start + 1):theta_end) {
    line <- modelcode[i]

    # Check for exact match or level-specific variants
    if (grepl(paste0("\\b", covariate_to_remove, "\\b"), line) ||
        grepl(paste0("\\b", covariate_to_remove, "_[^\\s;]+"), line)) {

      theta_line_pos <- i - theta_start
      theta_numbers_to_remove <- c(theta_numbers_to_remove, theta_line_pos)
      theta_lines_to_remove <- c(theta_lines_to_remove, i)
      log_msg(paste("Found THETA", theta_line_pos, "at line", i, ":", trimws(line)))
    }
  }

  if (length(theta_numbers_to_remove) == 0) {
    stop("No THETA parameters found for covariate: ", covariate_to_remove)
  }

  theta_numbers_to_remove <- sort(theta_numbers_to_remove)
  log_msg(paste("THETA numbers to remove:", paste(theta_numbers_to_remove, collapse = ", ")))

  # Step 4: Store original parameter lines for logging
  original_param_lines <- list()
  lines_to_modify <- c()

  for (i in 1:length(modelcode)) {
    line <- modelcode[i]

    if (grepl(cova, line)) {
      original_param_lines[[as.character(i)]] <- line
      lines_to_modify <- c(lines_to_modify, i)
    }
  }

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
          log_msg(paste("Removed linear effect for THETA(", theta_num, ")"))
        } else if (grepl(pattern2, modified_line)) {
          modified_line <- gsub(pattern2, "", modified_line)
          line_changed <- TRUE
          log_msg(paste("Removed power effect for THETA(", theta_num, ")"))
        } else if (grepl(pattern3, modified_line)) {
          modified_line <- gsub(pattern3, "", modified_line)
          line_changed <- TRUE
          log_msg(paste("Removed categorical variable for THETA(", theta_num, ")"))
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
    log_msg("Looking for IF-THEN-ELSE blocks to remove...")

    pk_start <- grep("^\\$PK", modelcode)
    if (length(pk_start) > 0) {
      pk_end <- grep("^\\$", modelcode)
      pk_end <- pk_end[pk_end > pk_start[1]][1] - 1
      if (is.na(pk_end)) pk_end <- length(modelcode)

      lines_to_remove <- c()
      in_if_block <- FALSE

      for (i in pk_start:pk_end) {
        line <- modelcode[i]

        if (grepl(paste0("IF\\s*\\(", cova), line) ||
            grepl(paste0("\\b", covariate_to_remove, "\\s*="), line)) {
          in_if_block <- TRUE
          log_msg(paste("Found IF block or variable assignment at line", i))
        }

        if (in_if_block) {
          lines_to_remove <- c(lines_to_remove, i)
        }

        if (grepl("ENDIF", line) && in_if_block) {
          in_if_block <- FALSE
          log_msg(paste("Found IF block end at line", i))
        }
      }

      if (length(lines_to_remove) > 0) {
        log_msg(paste("Removing", length(lines_to_remove), "lines from IF-THEN-ELSE block"))
        modelcode <- modelcode[-lines_to_remove]
        # Adjust THETA line indices after removing IF blocks
        theta_lines_to_remove <- theta_lines_to_remove - sapply(theta_lines_to_remove, function(x) sum(lines_to_remove < x))
        # Adjust parameter line indices for final logging
        for (idx_str in names(original_param_lines)) {
          idx <- as.numeric(idx_str)
          adjustment <- sum(lines_to_remove < idx)
          if (adjustment > 0) {
            new_idx <- idx - adjustment
            names(original_param_lines)[names(original_param_lines) == idx_str] <- as.character(new_idx)
          }
        }
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

  # Step 9: Log final parameter line changes (AFTER renumbering)
  log_msg("=== Final Parameter Line Changes ===")
  for (idx_str in names(original_param_lines)) {
    idx <- as.numeric(idx_str)
    if (idx <= length(modelcode)) {
      original_line <- original_param_lines[[idx_str]]
      final_line <- modelcode[idx]
      if (original_line != final_line) {
        log_msg(paste("Final change for line", idx, ":"))
        log_msg(paste("  Before:", original_line))
        log_msg(paste("  After: ", final_line))
      }
    }
  }

  # Step 10: Write the modified model file
  attr(modelcode, "file_path") <- original_file_path
  search_state <- write_model_file(search_state, modelcode)
  log_msg(paste("Model file updated:", basename(original_file_path)))

  # Step 11: Update database
  final_model_name <- if (save_as_new_model) new_model_name else model_name

  if (save_as_new_model) {
    # Add new model to database
    new_row <- data.frame(
      model_name = new_model_name,
      step_description = sprintf("Remove %s", gsub("beta_", "", covariate_to_remove)),
      phase = "covariate_removal",
      step_number = max(search_state$search_database$step_number, na.rm = TRUE) + 1,
      parent_model = model_name,
      covariate_tested = gsub("beta_", "", covariate_to_remove),
      action = "remove_single_covariate",
      ofv = NA_real_,
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      status = "created",
      tags = I(list(character(0))),  # Empty tags after removal
      submission_time = as.POSIXct(NA),
      completion_time = as.POSIXct(NA),
      retry_attempt = 0L,
      original_model = NA_character_,
      estimation_issue = NA_character_,
      excluded_from_step = FALSE,
      stringsAsFactors = FALSE
    )

    search_state$search_database <- rbind(search_state$search_database, new_row)
    log_msg(paste("Added", new_model_name, "to database"))
  }

  # Step 12: Save detailed log
  log_file <- file.path(search_state$models_folder, paste0(final_model_name, "_remove_", gsub("beta_", "", covariate_to_remove), "_log.txt"))
  writeLines(log_messages, log_file)

  cat(" Complete. Created", final_model_name, "\n")
  log_msg(paste("✓ Covariate", covariate_to_remove, "successfully removed"))
  log_msg(paste("Final model:", final_model_name))
  log_msg(paste("Log saved to:", basename(log_file)))

  return(list(
    search_state = search_state,
    model_name = final_model_name,
    log_file = log_file
  ))
}

#' Add Covariate to Model with Detailed Logging
#'
#' @title Wrapper for add_covariate_to_model with comprehensive logging
#' @description Calls add_covariate_to_model with detailed timestamped logging
#'   and saves complete log files for debugging and audit purposes.
#' @param search_state List containing search state
#' @param base_model_id Character. Base model identifier (e.g., "run1")
#' @param covariate_tag Character. Covariate tag to add (e.g., "cov_cl_wt")
#' @return List with result and log information (search_state updated in place)
#' @export
add_covariate_with_detailed_logging <- function(search_state, base_model_id, covariate_tag) {

  # Initialize logging
  log_entries <- character(0)
  start_time <- Sys.time()

  # Logging helper function
  log_msg <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    entry <- paste0(timestamp, " - ", message)
    log_entries <<- c(log_entries, entry)
    cat(entry, "\n")
  }

  # Start logging
  log_msg("=== Starting Covariate Addition Process ===")
  log_msg(paste("Parent model:", base_model_id))
  log_msg(paste("Covariate tag:", covariate_tag))

  # Get and validate tag value
  if (covariate_tag %in% names(search_state$tags)) {
    tag_value <- search_state$tags[[covariate_tag]]
    log_msg(paste("Tag value found:", tag_value))
  } else {
    log_msg(paste("ERROR: Covariate tag not found:", covariate_tag))
    error_log_filename <- file.path(search_state$models_folder, paste0("ERROR_", covariate_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_log.txt"))
    writeLines(log_entries, error_log_filename)
    return(list(
      status = "error",
      error_message = "Covariate tag not found",
      log_file = error_log_filename,
      log_entries = log_entries
    ))
  }

  # Find matching covariate in search definition
  matching_cov <- search_state$covariate_search[
    grepl(paste0("_", tag_value, "$"), search_state$covariate_search$cov_to_test), ]

  if (nrow(matching_cov) > 0) {
    log_msg(paste("Matching covariate found:", matching_cov$COVARIATE[1]))
    combined_param <- paste0(matching_cov$COVARIATE[1], "_", matching_cov$PARAMETER[1])
    log_msg(paste("Combined parameter name:", combined_param))
  } else {
    log_msg(paste("ERROR: No matching covariate definition found for:", tag_value))
    error_log_filename <- file.path(search_state$models_folder, paste0("ERROR_", covariate_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_log.txt"))
    writeLines(log_entries, error_log_filename)
    return(list(
      status = "error",
      error_message = "No matching covariate definition found",
      log_file = error_log_filename,
      log_entries = log_entries
    ))
  }

  # Predict new model name
  new_model_number <- search_state$model_counter + 1
  predicted_model <- paste0("run", new_model_number)
  log_msg(paste("Predicted model name:", predicted_model))

  # Call the actual function with comprehensive error capture
  log_msg("Step 1: Creating BBR model and adding covariate...")

  # Call the updated add_covariate_to_model function (modifies search_state in place)
  result <- add_covariate_to_model(search_state, base_model_id, covariate_tag)

  # Calculate processing time
  process_time <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)

  if (result$status == "success") {
    log_msg("BBR model created successfully")
    log_msg("Step 2: Covariate addition completed")
    log_msg(paste("=== Process completed successfully in", process_time, "seconds ==="))
    log_msg(paste("Final model created:", result$model_name))

    # Save success log file
    log_filename <- file.path(search_state$models_folder, paste0(result$model_name, "_add_", tag_value, "_log.txt"))
    log_msg(paste("Saving log to:", basename(log_filename)))

    writeLines(log_entries, log_filename)

    return(list(
      status = "success",
      model_name = result$model_name,
      covariate_added = tag_value,
      log_file = log_filename,
      log_entries = log_entries,
      processing_time_seconds = process_time
    ))

  } else {
    # Handle error case
    log_msg(paste("ERROR occurred:", result$error_message))
    log_msg(paste("=== Process failed after", process_time, "seconds ==="))

    if (!is.null(result$file_exists) && result$file_exists) {
      log_msg(paste("Physical model file was created:", result$attempted_model))
      log_msg("Model counter was updated to match existing file")
    } else {
      log_msg("No physical model file was created")
    }

    # Save error log with detailed error information
    error_log_filename <- file.path(search_state$models_folder, paste0("ERROR_", covariate_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_log.txt"))
    log_msg(paste("Saving error log to:", basename(error_log_filename)))

    writeLines(log_entries, error_log_filename)

    return(list(
      status = "error",
      error_message = result$error_message,
      log_file = error_log_filename,
      log_entries = log_entries,
      attempted_model = result$attempted_model,
      file_exists = result$file_exists,
      processing_time_seconds = process_time
    ))
  }
}
