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
#' @param lookup_file Character or NULL. Optional path to lookup YAML for
#'   categorical labels. If NULL, uses search_state configuration/default.
#' @return List with updated search_state and new model information
#' @export
add_covariate_to_model <- function(search_state, base_model_id, covariate_tag,
                                   step_number = NULL,
                                   lookup_file = NULL) {
  cat(sprintf("[+] Adding covariate %s to model %s\n", covariate_tag, base_model_id))

  # STEP 1: Validate inputs and calculate step number FIRST
  if (!covariate_tag %in% names(search_state$tags)) {
    stop("Covariate tag not found: ", covariate_tag)
  }

  covariate_name <- search_state$tags[[covariate_tag]]
  cat(sprintf("  Covariate: %s\n", covariate_name))

  # Step number is REQUIRED. The automated search passes it (the phase's current
  # step); a manual addition must supply it explicitly, because the correct step
  # depends on which phase/round the user is building into and cannot be guessed
  # from the parent alone (a model from forward selection must not be silently
  # assigned to the next backward step, and vice versa).
  if (is.null(step_number)) {
    stop("step_number is required. Provide the step number this model belongs to ",
         "(e.g. the current forward/backward step you are extending). The automated ",
         "search supplies this for you; manual add/remove must set it explicitly.")
  }
  final_step_number <- as.integer(step_number)
  cat(sprintf("  Step number: %d\n", final_step_number))

  # Find covariate definition
  matching_cov <- search_state$covariate_search[
    grepl(paste0("_", covariate_name, "$"), search_state$covariate_search$cov_to_test), ]

  if (nrow(matching_cov) == 0) {
    stop("No matching covariate definition found for: ", covariate_name)
  }

  validate_covariate_parameter_mapping(
    covariate_search = search_state$covariate_search,
    model_name = base_model_id,
    models_folder = search_state$models_folder,
    covariate_tags = matching_cov$cov_to_test[1],
    strict = TRUE,
    verbose = FALSE
  )

  # STEP 2: Calculate model name and prepare for creation
  original_counter <- search_state$model_counter
  search_state$model_counter <- search_state$model_counter + 1
  new_model_name <- sprintf("run%d", search_state$model_counter)
  cat(sprintf("  New model: %s\n", new_model_name))

  # STEP 3: Comprehensive error handling wrapper
  tryCatch({
    # Sub-step 3a: Create BBR model
      technical_log <- character(0)  # Initialize early for error handler scope
      log_filename <- NULL
   
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

    # Wrap model_add_cov with error tracing to capture logs even on failure
    model_result <- tryCatch({
      model_add_cov(
        search_state = search_state,
        ref_model = new_model_name,
        cov_on_param = cov_on_param,
        id_var = search_state$idcol,
        data_file = search_state$data_file,
        covariate_search = search_state$covariate_search,
        capture_log = TRUE,
        lookup_file = lookup_file
      )
    }, error = function(cov_error) {
      # Even on error, we want to save what we captured
      cat(sprintf("  [ERROR] Covariate addition failed: %s\n", cov_error$message))
      # Save error details including what we know so far
      list(
        status = "error",
        error_message = cov_error$message,
        log_entries = character(0)  # Will be populated if we could extract it
      )
    })

    # Check if model_add_cov succeeded or failed
    if (!is.null(model_result$status) && model_result$status == "error") {
      # Save error log with diagnostics
      error_log_content <- c(
        "=== COVARIATE ADDITION FAILED ===",
        sprintf("Time: %s", Sys.time()),
        sprintf("Model: %s", new_model_name),
        sprintf("Covariate parameter: %s", cov_on_param),
        sprintf("Covariate: %s (Reference: %s)", cov_name, matching_cov$REFERENCE[1]),
        sprintf("ID column: %s", search_state$idcol),
        sprintf("Data file dimensions: %d rows x %d columns", nrow(search_state$data_file), ncol(search_state$data_file)),
        sprintf("Available columns: %s", paste(names(search_state$data_file), collapse = ", ")),
        "",
        "ERROR DETAILS:",
        model_result$error_message,
        "",
        "POSSIBLE CAUSES:",
        sprintf("- Covariate column '%s' missing from data? Check available columns above.", cov_name),
        sprintf("- ID column '%s' not found in data? Available: %s", search_state$idcol, paste(names(search_state$data_file), collapse = ", ")),
        "- Data mismatch in tapply() call (arguments must have same length)"
      )
      
      log_filename <<- file.path(search_state$models_folder,
                                 paste0(new_model_name, "_add_", covariate_name, "_error_log.txt"))
      writeLines(error_log_content, log_filename)
      cat(sprintf("  📝 Error log saved: %s\n", basename(log_filename)))
      
      # Now throw to be caught by outer handler
      stop(model_result$error_message)
    }

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
    yaml_path <- file.path(search_state$models_folder,  paste0(new_model_name, ".yaml"))
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
        covariate_tested = matching_cov$cov_to_test[1],
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
#' @param lookup_file Character or NULL. Optional path to lookup YAML for
#'   categorical labels. If NULL, uses search_state$search_config$lookup_file
#'   then defaults to data/spec/lookup.yaml.
#' @return Updated search_state
#' @export
model_add_cov <- function(search_state, ref_model, cov_on_param, id_var = "ID",
                          data_file, covariate_search, capture_log = FALSE,
                          lookup_file = NULL) {

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

  resolved_lookup_file <- if (!is.null(lookup_file)) {
    lookup_file
  } else if (!is.null(search_state$search_config) &&
             !is.null(search_state$search_config$lookup_file)) {
    search_state$search_config$lookup_file
  } else {
    file.path("data", "spec", "lookup.yaml")
  }

  modelcode <- read_model_file(search_state, ref_model)
  original_file_path <- attr(modelcode, "file_path")
  log_function(paste("Read model file:", basename(original_file_path)))

  cov_on_param <- paste0("beta_", cov_on_param)

  cova <- covariate_search$COVARIATE[covariate_search$cov_to_test == cov_on_param]
  param <- covariate_search$PARAMETER[covariate_search$cov_to_test == cov_on_param]
  ref <- covariate_search$REFERENCE[covariate_search$cov_to_test == cov_on_param]

  log_function(paste("Covariate:", cova, "Parameter:", param, "Reference:", ref))

  # Get max THETA number
  thetas <- modelcode[grepl('THETA\\(\\d+\\)', modelcode)] %>%
    gsub(pattern = '.*THETA\\(', replacement = '') %>%
    gsub(pattern = '\\).*', replacement = '') %>%
    as.double()

  if (length(thetas) == 0) {
    newtheta <- 1
    log_function("No existing THETAs found, starting at THETA(1)")
  } else {
    newtheta <- max(thetas) + 1
    log_function(paste("Current max THETA:", max(thetas), "New THETA number:", newtheta))
  }

  temp_cov <- dplyr::filter(covariate_search, cov_to_test == cov_on_param)

  # Determine covariate type and formula
  cov_status <- temp_cov$STATUS[temp_cov$COVARIATE == cova]
  cov_formula <- temp_cov$FORMULA[temp_cov$COVARIATE == cova]

  log_function(paste("Covariate type:", cov_status, "Formula:", cov_formula))

  # Look up the covariate-effect form in the registry (single source of truth for
  # the NONMEM factor + the R reconstruction, so the two can't drift). cat.linear
  # is flagged categorical and keeps its per-level IF/ELSEIF block below; every
  # other form is a single factor rendered from the registry.
  if (length(cov_status) == 0 || length(cov_formula) == 0) {
    stop("Missing covariate STATUS/FORMULA for '", cova, "'")
  }
  cov_formula_def <- get_covariate_formula(cov_status, cov_formula)
  if (is.null(cov_formula_def)) {
    stop("Unknown covariate formula '", cov_formula, "' (status '", cov_status,
         "'). Registered forms: ", paste(list_covariate_formulas(), collapse = ", "))
  }
  is_categorical <- isTRUE(cov_formula_def$categorical)
  log_function(paste("Covariate form:", cov_status, "/", cov_formula,
                     if (is_categorical) "(categorical, per-level)" else "(single-factor)"))

  # Single-factor expressions with more than one parameter (e.g. Emax/Imax) parse
  # correctly but need multi-THETA allocation + per-theta tags, which is a later
  # build step. Guard here so we never write a half-allocated model.
  if (length(cov_formula_def$theta_names) > 1L) {
    stop("Multi-theta formula '", cov_formula, "' (",
         length(cov_formula_def$theta_names), " parameters: ",
         paste(cov_formula_def$theta_names, collapse = ", "),
         ") is not yet supported — multi-THETA allocation lands in a later step.")
  }

  # Initial THETA value: prefer an explicit INIT from the covariate_search table
  # (per covariate-parameter row); fall back to the formula-derived default when
  # INIT is absent, NA, or blank. Backward-compatible: tables without an INIT
  # column behave exactly as before. Used for the single-THETA path and, as one
  # shared value, for every categorical per-level theta (per-level DIFFERENT
  # inits are the multi-value INIT, a later step).
  formula_default_init <- cov_formula_def$init

  table_init <- if ("INIT" %in% names(temp_cov)) {
    temp_cov$INIT[temp_cov$COVARIATE == cova]
  } else {
    NA_character_
  }

  initialValuethetacov <- if (length(table_init) == 1 &&
                              !is.na(table_init) &&
                              trimws(as.character(table_init)) != "") {
    trimws(as.character(table_init))
  } else {
    formula_default_init
  }

  log_function(paste("Initial THETA value:", initialValuethetacov,
                     if (identical(initialValuethetacov, formula_default_init)) "(formula default)" else "(from INIT column)"))

  # ---- Placement (population vs individual) & rendering mode -------------------
  # A time-varying covariate (changes within a subject) is an individual-level
  # effect -> its factor goes on the individual PARAM line and is ALWAYS
  # multiplicative (correct for any parameterization). A time-constant covariate
  # is a population effect -> it goes on the typical-value (TV_) line and is
  # rendered per the parameter's transformation: multiplicative for a normal-scale
  # parameter (PARAM = TV*EXP(ETA)), additive for a log-scale one
  # (PARAM = EXP(TV+ETA)) so the effect is not multiplied into a log value.
  max_levels <- max(tapply(data_file[[cova]], data_file[[id_var]],
                           function(x) length(unique(x))), na.rm = TRUE)
  if (is.infinite(max_levels)) {
    log_function("WARNING: Could not determine time-varying status - defaulting to non-time-varying")
    max_levels <- 1
  }
  time_varying <- max_levels > 1
  log_function(paste("Time-varying check:", time_varying, "(max levels:", max_levels, ")"))

  if (time_varying) {
    op_mode <- "mult"                          # individual level -> multiplicative
    log_function("Placement: individual level (time-varying) -> multiplicative")
  } else {
    param_transform <- detect_param_transform(modelcode, param)
    log_function(paste("Parameter transformation:", param_transform))
    if (identical(param_transform, "log")) {
      op_mode <- "add"
    } else if (identical(param_transform, "normal")) {
      op_mode <- "mult"
    } else {
      stop("Cannot determine the parameterization of parameter '", param,
           "' in model '", ref_model, "'. Expected normal-scale '", param,
           " = TV * EXP(ETA)' or log-scale '", param, " = EXP(TV + ETA)'. ",
           "Covariate rendering for other parameterizations is not supported.")
    }
    log_function(paste("Placement: population level ->",
                       if (op_mode == "add") "additive (log-scale parameter)" else "multiplicative"))
  }

  # Generate the covariate factor/term per op_mode: single-factor built-ins render
  # from the registry -- multiplicative nonmem() for "mult", additive nonmem_log()
  # for "add". A user expression has no additive form -> it is inserted
  # multiplicatively (its correctness on a log scale is the user's responsibility).
  # cat.linear keeps the per-level IF/ELSEIF block below, with the assignment and
  # join operator switched to the additive form when op_mode is "add".
  thetanmulti <- tibble()
  if (!is_categorical) {
    if (op_mode == "add" && !is.null(cov_formula_def$nonmem_log)) {
      formule <- cov_formula_def$nonmem_log(cova, ref, newtheta)
    } else {
      if (op_mode == "add") {
        log_function(paste0("WARNING: user expression on log-scale parameter '", param,
                            "' inserted as a multiplicative factor; ensure the expression ",
                            "is written correctly for the log scale."))
      }
      formule <- cov_formula_def$nonmem(cova, ref, newtheta)
    }
  } else {
    # Additive (log) categorical uses beta = 0 / THETA and joins with '+';
    # multiplicative uses beta = 1 / 1+THETA and joins with '*'.
    if (op_mode == "add") {
      cat_ref_assign <- "0"
      cat_nonref_fn  <- function(k) paste0("THETA(", k, ")")
      combine_op     <- " + "
    } else {
      cat_ref_assign <- "1"
      cat_nonref_fn  <- function(k) paste0("1 + THETA(", k, ")")
      combine_op     <- " * "
    }
    # Try to load lookup YAML for categorical labels (optional)
    lookup_values <- NULL
    if (!is.null(resolved_lookup_file) && file.exists(resolved_lookup_file)) {
      tryCatch({
        lookup_data <- yaml::read_yaml(resolved_lookup_file)
        # Check if this covariate exists in lookup
        if (cova %in% names(lookup_data)) {
          lookup_info <- lookup_data[[cova]]
          # Convert values and decode lists to a named list
          if (!is.null(lookup_info$values) && !is.null(lookup_info$decode)) {
            lookup_values <- setNames(
              as.list(lookup_info$decode),
              as.character(lookup_info$values)
            )
            log_function(paste("Found lookup values for", cova, "in", resolved_lookup_file))
          }
        }
      }, error = function(e) {
        log_function(paste("Note: Could not use lookup file:", e$message))
      })
    } else {
      log_function(paste("No lookup file found at:", resolved_lookup_file, "(using numeric labels)"))
    }
    if (!cova %in% names(data_file)) {
      stop("Covariate '", cova, "' not found in data file. Available columns: ",
           paste(head(names(data_file), 20), collapse = ", "))
    }
    uniqueval <- unique(data_file[[cova]])
    newvari <- cov_on_param
    formule <- paste0(combine_op, newvari)
    ifelcode <- paste0('IF(', cova, '.EQ.', temp_cov$REFERENCE,')THEN\n', newvari, ' = ', cat_ref_assign, '\n' )

    uniqueval_with_freq <- data_file %>%
      dplyr::count(!!rlang::parse_expr(cova)) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::pull(!!rlang::parse_expr(cova))

    uniqueval <- uniqueval_with_freq[uniqueval_with_freq != temp_cov$REFERENCE]

    thetanmulti <- tibble(
      covx = uniqueval,
      label = character(length(uniqueval))  # Initialize empty
    )
    for(a in seq_along(uniqueval)){
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
                         newvari, ' = ', cat_nonref_fn(theta_num), '\n')

      log_function(paste("Added ELSEIF for", cova, "=", current_level,
                         "using THETA(", theta_num, ")"))
    }
    ifelcode <- paste0(ifelcode, 'ENDIF\n')
    modelcode[grep('^\\$PK', modelcode)] <- paste0(modelcode[grep('^\\$PK', modelcode)], '\n\n', ifelcode)
    #}
  }

  log_function(paste("Generated formula:", formule))

  if(time_varying == FALSE) {
    # First try to find TV_ prefixed parameter (typical values)
    linetu <- grep(paste0('^\\s*TV_', param, '\\b'), modelcode)
    search_pattern <- paste0('^\\s*TV_', param, '\\b')

    # If not found, try parameter without TV_ prefix (e.g., Km, Vmax, etc.)
    if (length(linetu) == 0) {
      linetu <- grep(paste0('^\\s*', param, '\\b\\s*='), modelcode)
      search_pattern <- paste0('^\\s*', param, '\\b\\s*=')
     }
  } else {
    # Time-varying: search for parameter as-is
    linetu <- grep(paste0('^\\s*', param, '\\b'), modelcode)
    search_pattern <- paste0('^\\s*', param, '\\b')
  }


  log_function(paste("Looking for parameter line with pattern:", search_pattern))
  log_function(paste("Found parameter line at index:", linetu))

  # If multiple matches, use the last one (final assignment to the parameter)
  if (length(linetu) > 1) {
    log_function(paste("WARNING: Multiple matches found, using last match at index:", linetu[length(linetu)]))
    linetu <- linetu[length(linetu)]
  }

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
    msg <- paste(
      "Parameter line not found for pattern:",
      search_pattern,
      "for covariate",
      cov_on_param,
      "in model",
      ref_model
    )
    log_function(paste("ERROR:", msg))
    stop(msg)
  }

  # Add THETA line
  if(nrow(thetanmulti) > 0){
    # Multiple THETAs for categorical with >2 levels (excluding reference)
    newthetalines <- purrr::map_chr(1:nrow(thetanmulti), ~ {
      # Use the label from thetanmulti which now contains lookup values or numbers
      paste0(initialValuethetacov, ' ; ', cov_on_param, "_", thetanmulti$label[[.x]], ';  ; RATIO')
    })
    newthetaline <- paste0(newthetalines, collapse = '\n')

    log_function(paste("Adding", nrow(thetanmulti), "THETA lines for categorical levels"))
  } else if (length(cov_formula_def$theta_names) > 1L) {
    # Multi-parameter single-factor expression (Emax/Imax/Hill): one THETA per
    # parameter, named beta_<COV>_<PARAM>_<parameter> and initialised from the
    # named INIT spec ("EMAX=0.1; EC50=10"), defaulting to 0.1. Order matches the
    # parameter order the nonmem renderer used for THETA(newtheta), THETA(newtheta+1)...
    tn <- cov_formula_def$theta_names
    init_per_theta <- parse_named_init(table_init, tn)
    newthetalines <- vapply(seq_along(tn), function(i) {
      paste0(init_per_theta[i], ' ; ', cov_on_param, "_", tn[i], ' ;  ; RATIO')
    }, character(1))
    newthetaline <- paste0(newthetalines, collapse = '\n')

    log_function(paste("Adding", length(tn), "THETA lines for multi-parameter expression"))
  } else {
    # Single THETA for binary or continuous covariates
    newthetaline <- paste0(initialValuethetacov, ' ; ', cov_on_param, ' ;  ; RATIO')
  }

  log_function(paste("New THETA line to add:", newthetaline))

  lineomeg <- grep('\\$OMEGA', modelcode)[1]
  if (is.na(lineomeg)) {
    log_function("ERROR: No $OMEGA section found in model file")
    stop("Model file missing $OMEGA section - cannot insert THETA line")
  }
  log_function(paste("Inserting THETA line before $OMEGA section at line:", lineomeg))

  modelcode <- c(
    modelcode[1:(lineomeg - 1)],
    newthetaline,
    modelcode[lineomeg:length(modelcode)]
  )

  log_function(paste("✓ THETA line added at position:", lineomeg))
  log_function(paste("Total lines in model:", length(modelcode)))

  # Update $TABLE FILE= names to match new model number
  # e.g. FILE=catab1 -> FILE=catab11 when ref_model is "run11"
  new_model_num <- gsub("^run", "", ref_model)
  if (grepl("^\\d+$", new_model_num)) {
    table_lines <- grep("FILE=", modelcode, ignore.case = TRUE)
    for (i in table_lines) {
      old_line <- modelcode[i]
      modelcode[i] <- gsub(
        "(FILE=)([a-zA-Z]+)\\d+",
        paste0("\\1\\2", new_model_num),
        modelcode[i],
        ignore.case = TRUE
      )
      if (modelcode[i] != old_line) {
        log_function(paste("Updated TABLE FILE reference:"))
        log_function(paste("  Before:", old_line))
        log_function(paste("  After: ", modelcode[i]))
      }
    }
  }

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

# =============================================================================
# PRE-INITIALIZATION MODEL PREPARATION
# Create one model with multiple covariates and one combined log
# =============================================================================

#' Prepare Search Base Model by Adding Multiple Covariates
#'
#' @title Create one prepared base model with multiple covariates
#' @description Copies a parent model, adds multiple covariates into the same
#'   child model, writes one combined technical log, and returns metadata for
#'   later initialization. Does NOT require search_database.
#'
#' @param base_model_path Character. Parent/base model id, e.g. "run1"
#' @param covariate_tags Character vector of covariate tags, e.g.
#'   c("beta_WT_CL", "beta_AGE_V2")
#' @param new_model_number Integer. Required model number for the new model
#' @param data_file_path Character. Path to NONMEM dataset CSV
#' @param covariate_search_path Character. Path to covariate search CSV
#' @param models_folder Character. Models directory
#' @param idcol Character. ID column name
#' @param overwrite Logical. Overwrite existing model if present
#' @param lookup_file Character or NULL. Optional path to lookup YAML for
#'   categorical labels. If NULL, defaults to data/spec/lookup.yaml.
#'
#' @return List with status, model_name, model_path, log_file, covariates_added
#' @export
prepare_search_base_model <- function(base_model_path,
                                      covariate_tags,
                                      new_model_number,
                                      data_file_path,
                                      covariate_search_path,
                                      models_folder = "models",
                                      idcol = "ID",
                                      overwrite = TRUE,
                                      lookup_file = NULL) {

  cat("Preparing search base model with multiple covariates...\n")

  # ---------------------------------------------------------------------------
  # Step 1: Validate basic inputs
  # ---------------------------------------------------------------------------
  if (length(covariate_tags) == 0) {
    stop("covariate_tags cannot be empty")
  }

  if (is.null(new_model_number) || length(new_model_number) != 1 || is.na(new_model_number)) {
    stop("new_model_number must be a single non-missing integer")
  }

  new_model_number <- as.integer(new_model_number)
  new_model_name <- sprintf("run%d", new_model_number)

  parent_path <- file.path(models_folder, base_model_path)
  if (!file.exists(paste0(parent_path, ".ctl")) && !file.exists(paste0(parent_path, ".mod"))) {
    stop("Parent model file not found: ", parent_path)
  }

  # ---------------------------------------------------------------------------
  # Step 2: Load data needed for model editing
  # ---------------------------------------------------------------------------
  data_file <- readr::read_csv(data_file_path, show_col_types = FALSE)
  covariate_search <- readr::read_csv(covariate_search_path, show_col_types = FALSE)

  if (!"cov_to_test" %in% names(covariate_search)) {
    covariate_search$cov_to_test <- paste0(
      "beta_",
      covariate_search$COVARIATE, "_",
      covariate_search$PARAMETER
    )
  }

  # Build tags exactly as initialize does via tags.yaml semantics:
  # beta_WT_CL -> "WT_CL"
  tags <- stats::setNames(
    as.list(gsub("^beta_", "", covariate_search$cov_to_test)),
    covariate_search$cov_to_test
  )

  invalid_tags <- setdiff(covariate_tags, names(tags))
  if (length(invalid_tags) > 0) {
    stop("Unknown covariate tag(s): ", paste(invalid_tags, collapse = ", "))
  }

  validate_covariate_parameter_mapping(
    covariate_search = covariate_search,
    model_name = base_model_path,
    models_folder = models_folder,
    covariate_tags = covariate_tags,
    strict = TRUE,
    verbose = TRUE
  )

  covariate_tags <- unique(covariate_tags)
  covariate_values <- unname(unlist(tags[covariate_tags]))

  resolved_lookup_file <- if (!is.null(lookup_file)) {
    lookup_file
  } else {
    file.path("data", "spec", "lookup.yaml")
  }

  # ---------------------------------------------------------------------------
  # Step 3: Build minimal temporary state required by model_add_cov()
  # ---------------------------------------------------------------------------
  temp_state <- list(
    base_model = base_model_path,
    data_file = data_file,
    covariate_search = covariate_search,
    models_folder = models_folder,
    idcol = idcol,
    tags = tags,
    search_config = list(lookup_file = resolved_lookup_file)
  )

  # ---------------------------------------------------------------------------
  # Step 4: Create one child model from parent
  # ---------------------------------------------------------------------------
  cat(sprintf("  Parent model: %s\n", base_model_path))
  cat(sprintf("  New model: %s\n", new_model_name))

  parent_mod <- bbr::read_model(parent_path)

  new_mod <- bbr::copy_model_from(
    .parent_mod = parent_mod,
    .new_model = new_model_name,
    .inherit_tags = TRUE,
    .overwrite = overwrite
  )

  new_mod <- bbr::add_tags(new_mod, .tags = covariate_values)

  note_text <- sprintf("Prepared base + %s", paste(covariate_values, collapse = "; "))
  new_mod <- bbr::replace_all_notes(new_mod, .notes = note_text)

  # ---------------------------------------------------------------------------
  # Step 5: Add all covariates into the same child model
  # ---------------------------------------------------------------------------
  combined_log <- c(
    "========================================",
    "PREPARE SEARCH BASE MODEL",
    "========================================",
    sprintf("Timestamp: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("Parent model: %s", base_model_path),
    sprintf("New model: %s", new_model_name),
    sprintf("Requested tags: %s", paste(covariate_tags, collapse = ", ")),
    sprintf("Resolved covariates: %s", paste(covariate_values, collapse = ", ")),
    ""
  )

  for (cov_tag in covariate_tags) {
    cov_value <- tags[[cov_tag]]

    matching_cov <- covariate_search[covariate_search$cov_to_test == cov_tag, , drop = FALSE]
    if (nrow(matching_cov) == 0) {
      stop("No matching covariate definition found for tag: ", cov_tag)
    }

    cov_name <- matching_cov$COVARIATE[1]
    param_name <- matching_cov$PARAMETER[1]
    cov_on_param <- paste0(cov_name, "_", param_name)

    combined_log <- c(
      combined_log,
      "----------------------------------------",
      sprintf("Adding: %s", cov_tag),
      sprintf("Resolved as: %s", cov_value),
      sprintf("cov_on_param: %s", cov_on_param),
      ""
    )

    model_result <- model_add_cov(
      search_state = temp_state,
      ref_model = new_model_name,
      cov_on_param = cov_on_param,
      id_var = idcol,
      data_file = data_file,
      covariate_search = covariate_search,
      capture_log = TRUE
    )

    temp_state <- model_result$search_state

    if (!is.null(model_result$log_entries) && length(model_result$log_entries) > 0) {
      combined_log <- c(combined_log, model_result$log_entries, "")
    } else {
      combined_log <- c(combined_log, "[No technical log entries captured]", "")
    }
  }

  # ---------------------------------------------------------------------------
  # Step 6: Save one combined log file
  # ---------------------------------------------------------------------------
  safe_suffix <- paste(gsub("[^A-Za-z0-9_]+", "_", covariate_values), collapse = "__")
  log_file <- file.path(
    models_folder,
    paste0(new_model_name, "_prepare_base_", safe_suffix, "_log.txt")
  )

  writeLines(combined_log, log_file)
  cat(sprintf("  Combined log saved: %s\n", basename(log_file)))

  # ---------------------------------------------------------------------------
  # Step 7: Return metadata for later initialization
  # ---------------------------------------------------------------------------
  return(list(
    status = "success",
    model_name = new_model_name,
    model_path = file.path(models_folder, new_model_name),
    parent_model = base_model_path,
    covariate_tags = covariate_tags,
    covariates_added = covariate_values,
    log_file = log_file
  ))
}


# Strip covariate factors from a $PK/$PRED parameter line: remove any top-level
# ' * <factor>' whose factor references one of `theta_nums` (balanced-paren aware),
# leaving the structural term and other covariates' factors intact. Handles the
# built-ins (power/linear/exp), user expressions, and multi-parameter forms in one
# place, replacing four brittle per-shape regexes.
#' @keywords internal
#' @noRd
strip_covariate_factors <- function(line, theta_nums) {
  semi    <- regexpr(";", line, fixed = TRUE)
  code    <- if (semi > 0) substring(line, 1, semi - 1) else line
  comment <- if (semi > 0) substring(line, semi) else ""
  eq <- regexpr("=", code, fixed = TRUE)
  if (eq <= 0 || !grepl("THETA\\(", code)) return(line)
  lhs <- substring(code, 1, eq)
  rhs <- substring(code, eq + 1)
  chars <- strsplit(rhs, "")[[1]]; n <- length(chars)
  if (n == 0L) return(line)
  # Scan at paren depth 0 for additive '+' separators (covariate terms on a
  # LOG-scale typical value) and multiplicative '*' separators (factors on a
  # normal-scale value; the '**' power operator is skipped). A parameter line is
  # one style or the other; if both appear (an additive term that is itself a
  # product), '+' wins so whole additive terms are split rather than broken apart.
  depth <- 0L; plus <- integer(0); star <- integer(0)
  for (i in seq_len(n)) {
    ch <- chars[i]
    if (ch == "(") depth <- depth + 1L
    else if (ch == ")") depth <- depth - 1L
    else if (depth == 0L && ch == "+") plus <- c(plus, i)
    else if (depth == 0L && ch == "*" &&
             !(i > 1L && chars[i - 1L] == "*") &&
             !(i < n  && chars[i + 1L] == "*")) star <- c(star, i)
  }
  splits <- if (length(plus) > 0L) plus else star
  if (length(splits) == 0L) return(line)
  starts <- c(1L, splits); ends <- c(splits - 1L, n)
  segs <- vapply(seq_along(starts),
                 function(k) paste(chars[starts[k]:ends[k]], collapse = ""), character(1))
  theta_re <- paste0("THETA\\(\\s*(", paste(theta_nums, collapse = "|"), ")\\s*\\)")
  keep <- rep(TRUE, length(segs))
  if (length(segs) >= 2L) for (j in 2:length(segs)) if (grepl(theta_re, segs[j])) keep[j] <- FALSE
  if (all(keep)) return(line)
  paste0(lhs, paste(segs[keep], collapse = ""), comment)
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
#' @param step_number Integer or NULL. Optional step number for the new model's
#'   database row. NULL (default) auto-calculates as the parent model's step + 1,
#'   so covariates removed from the same parent share a step (one round).
#' @return List with updated search_state and operation details
#' @export
remove_covariate_from_model <- function(search_state, model_name, covariate_tag, save_as_new_model = TRUE, step_number = NULL) {

  cat(sprintf("[-] Removing covariate %s from model %s", covariate_tag, model_name))

  # Step number is REQUIRED (same contract as add_covariate_to_model): the automated
  # search supplies it (the current backward step); a manual removal must set it
  # explicitly rather than have it guessed from the parent, which cannot know the
  # intended phase/round.
  if (is.null(step_number)) {
    stop("step_number is required. Provide the step number this model belongs to ",
         "(e.g. the current backward step). The automated search supplies this for ",
         "you; manual add/remove must set it explicitly.")
  }

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

  # On any failure while editing the control stream (Steps 3-12), persist the log
  # so far as <model>_remove_<cov>_error_log.txt (mirrors the add-side error log),
  # then let the error propagate. withCallingHandlers writes without altering control
  # flow, and search_state edits inside the block persist to the success return.
  withCallingHandlers({

  # Step 3: Read and modify the model file
  modelcode <- read_model_file(search_state, model_to_modify)
  original_file_path <- attr(modelcode, "file_path")

  # Step 4: FIXED THETA DETECTION - Count actual THETA parameters, not line positions
  theta_start <- grep("^\\$THETA", modelcode)
  if (length(theta_start) == 0) {
    stop("No $THETA section found in model file for ", model_to_modify)
  }
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
  log_msg(paste("THETA numbers to remove:", paste(theta_numbers_to_remove, collapse = ", "))) # nolint: line_length_linter.

  # Step 5: Remove covariate effects from parameter equations. Two factor shapes:
  #   (a) categorical -> a ' * beta_<COV>_<PARAM>' (normal) or ' + beta_<COV>_<PARAM>'
  #       (log) variable term (its per-level THETAs live in the IF/ELSEIF block
  #       removed below); and
  #   (b) everything else -> a ' * (...)' factor (normal scale) or ' + ...' term
  #       (log scale) referencing one of the removed THETAs. strip_covariate_factors()
  #       handles (b) uniformly across built-ins (power/linear/exp), user
  #       expressions, and both multiplicative and additive placements.
  cat_var_re <- paste0("\\s*[*+]\\s*", covariate_to_remove, "\\b")
  lines_modified <- 0
  for (i in seq_along(modelcode)) {
    line <- modelcode[i]
    if (!grepl("=", line)) next
    modified_line <- gsub(cat_var_re, "", line)                                       # (a)
    modified_line <- strip_covariate_factors(modified_line, theta_numbers_to_remove)  # (b)
    if (!identical(modified_line, line)) {
      modelcode[i] <- modified_line
      lines_modified <- lines_modified + 1
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

  # Step 8b: Update $TABLE FILE= names to match the new model number
  if (save_as_new_model) {
    new_model_num <- gsub("^run", "", model_to_modify)
    if (grepl("^\\d+$", new_model_num)) {
      table_lines <- grep("FILE=", modelcode, ignore.case = TRUE)
      for (i in table_lines) {
        old_line <- modelcode[i]
        modelcode[i] <- gsub(
          "(FILE=)([a-zA-Z]+)\\d+",
          paste0("\\1\\2", new_model_num),
          modelcode[i],
          ignore.case = TRUE
        )
        if (modelcode[i] != old_line) {
          log_msg(paste("Updated TABLE FILE reference:"))
          log_msg(paste("  Before:", old_line))
          log_msg(paste("  After: ", modelcode[i]))
        }
      }
    }
  }

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
      step_number = as.integer(step_number),
      parent_model = model_name,
      covariate_tested = covariate_to_remove,
      action = "remove_covariate",
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

  }, error = function(e) {
    err_file <- file.path(search_state$models_folder,
                          paste0(model_to_modify, "_remove_", covariate_value, "_error_log.txt"))
    tryCatch({
      writeLines(c(log_messages, "", paste0("[ERROR] Covariate removal failed: ", e$message)),
                 err_file)
      cat(sprintf("  📝 Error log saved: %s\n", basename(err_file)))
    }, error = function(e2) NULL)
  })

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




