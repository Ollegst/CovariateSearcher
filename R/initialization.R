# =============================================================================
# INITIALIZATION
# File: R/initialization.R
# Part of CovariateSearcher Package
# Package initialization and setup
# =============================================================================



#' Initialize Covariate Search
#'
#' @title Initialize covariate search state with validation and setup
#' @description Main initialization function that sets up the search state,
#'   loads data files, validates configuration, and discovers existing models.
#' @param base_model_path Character. Path to base model (e.g., "run1")
#' @param data_file_path Character. Path to NONMEM dataset CSV
#' @param covariate_search_path Character. Path to covariate search CSV
#' @param models_folder Character. Directory containing models (default: "models")
#' @param timecol Character. Time column name (default: "TIME")
#' @param idcol Character. ID column name (default: "ID")
#' @param threads Integer. Number of threads for execution (default: 60)
#' @param discover_existing Logical. Discover existing models (default: TRUE)
#' @param validate_parameters Logical. Validate parameter block formatting (default: TRUE)
#' @return List containing complete search state configuration
#' @export
initialize_covariate_search <- function(base_model_path,
                                        data_file_path,
                                        covariate_search_path,
                                        models_folder = "models",
                                        timecol = "TIME",
                                        idcol = "ID",
                                        threads = 60,
                                        discover_existing = TRUE,
                                        validate_parameters = TRUE) {

  cat("Initializing CovariateSearcher (Core Module)...\n")

  # Initialize search state structure
  search_state <- list(
    base_model = base_model_path,
    data_file = NULL,
    covariate_search = NULL,
    models_folder = models_folder,
    timecol = timecol,
    idcol = idcol,
    threads = threads,
    tags = NULL,
    model_counter = NULL,
    search_database = NULL,
    search_config = NULL,
    discovered_models = NULL
  )

  # Load data files
  cat("Loading data files...\n")
  search_state$data_file <- readr::read_csv(data_file_path, show_col_types = FALSE)
  search_state$covariate_search <- readr::read_csv(covariate_search_path, show_col_types = FALSE)

  # Add cov_to_test column if missing
  if (!"cov_to_test" %in% names(search_state$covariate_search)) {
    cat("Adding cov_to_test column...\n")
    search_state$covariate_search$cov_to_test <- paste0("beta_",
                                                        search_state$covariate_search$COVARIATE, "_",
                                                        search_state$covariate_search$PARAMETER)
  }


  # AUTO-GENERATE/UPDATE TAGS.YAML FROM COVARIATE SEARCH TABLE
  tags_yaml_path <- file.path("data", "spec", "tags.yaml")
  cat("📝 Auto-generating/updating tags.yaml from covariate search table...\n")

  # Always generate/update to ensure synchronization
  tags_generated <- generate_tags_from_covariate_search(
    covariate_search = search_state$covariate_search,  # This is a data frame
    tags_yaml_path = tags_yaml_path,
    verbose = FALSE
  )

  if (tags_generated) {
    if (file.exists(tags_yaml_path)) {
      cat("  ✅ tags.yaml updated successfully\n")
    } else {
      cat("  ✅ tags.yaml created successfully\n")
    }
  } else {
    cat("  ⚠️ Failed to generate tags.yaml, will try to load existing\n")
  }

  # Load tags and run validations
  search_state <- load_tags(search_state)
  search_state <- validate_setup(search_state)

  # Initialize search database and configuration
  search_state <- initialize_search_database_core(search_state)
  search_state <- ensure_base_model_in_database(search_state)  # ADD THIS LINE
  search_state <- initialize_search_config(search_state)

  # Discover existing models if requested
  if (discover_existing) {
    search_state <- discover_existing_models(search_state)
  }

  # Set model counter based on existing models
  search_state <- update_model_counter(search_state)
  scm_rds_dir <- file.path(search_state$models_folder, "scm_rds")
  if (!dir.exists(scm_rds_dir)) {
    dir.create(scm_rds_dir, recursive = TRUE)
    cat("📁 Created: models/scm_rds/ for interim saves\n")
  }
  if (validate_parameters) {
    validate_base_model_parameters(
      base_model_path = base_model_path,
      strict = TRUE  # Stops if issues found
    )
  }
  cat("CovariateSearcher (Core) initialized successfully!\n")
  return(search_state)
}




#' Load Tags from YAML File
#'
#' @title Load covariate tags from YAML configuration
#' @description Loads the tags.yaml file containing covariate mappings
#' @param search_state List containing search state
#' @return Updated search_state with tags loaded
#' @export
load_tags <- function(search_state) {
  tags_file <- file.path("data", "spec", "tags.yaml")
  if (file.exists(tags_file)) {
    search_state$tags <- yaml::read_yaml(tags_file)
    cat("Tags loaded from:", tags_file, "\n")
  } else {
    stop("tags.yaml file not found at: ", tags_file)
  }
  return(search_state)
}



#' Validate Setup Configuration
#'
#' @title Validate covariate search setup and data consistency
#' @description Performs validation checks on data files and configuration
#' @param search_state List containing search state
#' @return Updated search_state (unchanged if validation passes)
#' @export
validate_setup <- function(search_state) {
  cat("Running validation checks...\n")

  # Check required columns in covariate_search
  required_cols <- c("PARAMETER", "COVARIATE", "STATUS", "FORMULA",
                     "LEVELS", "REFERENCE", "TIME_DEPENDENT")
  missing_cols <- setdiff(required_cols, names(search_state$covariate_search))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in covariate_search: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check that covariates exist in dataset
  missing_covs <- setdiff(search_state$covariate_search$COVARIATE,
                          names(search_state$data_file))
  if (length(missing_covs) > 0) {
    stop("Covariates not found in dataset: ", paste(missing_covs, collapse = ", "))
  }
  # Check categorical covariate levels
  cat("Checking categorical covariate levels...\n")
  categorical_covs <- search_state$covariate_search[search_state$covariate_search$STATUS == "cat", ]

  for (i in seq_len(nrow(categorical_covs))) {
    cov_name <- categorical_covs$COVARIATE[i]
    specified_levels <- categorical_covs$LEVELS[i]

    if (cov_name %in% names(search_state$data_file)) {
      actual_levels <- sort(unique(search_state$data_file[[cov_name]]))
      actual_levels <- actual_levels[!is.na(actual_levels)]

      # Parse specified levels
      if (grepl(";", specified_levels)) {
        expected_levels <- sort(as.numeric(unlist(strsplit(specified_levels, ";"))))
      } else {
        next  # Skip if can't parse
      }

      # Check if they match
      if (!identical(actual_levels, expected_levels)) {
        stop(sprintf(
          "Level mismatch for %s: Specified=%s, Actual=%s. Update covariate_search.csv",
          cov_name,
          paste(expected_levels, collapse=";"),
          paste(actual_levels, collapse = ";")

        ))
      }
    }
  }

  cat("All validation checks passed!\n")
  return(search_state)
}



#' Initialize Search Configuration
#'
#' @title Initialize search configuration parameters
#' @description Sets up default configuration for SCM workflow
#' @param search_state List containing search state
#' @return Updated search_state with initialized configuration
#' @export
initialize_search_config <- function(search_state) {
  search_state$search_config <- list(
    forward_p_value = 0.05,        # p-value for forward selection
    backward_p_value = 0.01,       # p-value for backward elimination (more stringent)
    max_rse_threshold = 50,
    max_forward_steps = 10,
    max_backward_steps = 10,
    timeout_minutes = 60,
    threads = search_state$threads,
    current_phase = "initialization",
    current_step = 0
  )
  cat("Search configuration initialized with p-values:\n")
  cat(sprintf("  Forward p-value: %.3f\n", search_state$search_config$forward_p_value))
  cat(sprintf("  Backward p-value: %.3f\n", search_state$search_config$backward_p_value))
  cat(sprintf("  RSE threshold: %d%%\n", search_state$search_config$max_rse_threshold))
  return(search_state)
}

#' Generate or Update tags.yaml File from Covariate Search Table
#'
#' @description Creates or updates the tags.yaml file with covariate definitions
#' based on the covariate search table. Preserves existing content and only
#' updates the covariates section.
#'
#' @param covariate_search Either a data frame or path to the covariate search CSV file
#' @param tags_yaml_path Path to tags.yaml file (default: "data/spec/tags.yaml")
#' @param verbose Logical. Print progress messages (default: TRUE)
#' @return Logical. TRUE if successful, FALSE otherwise
#' @export
generate_tags_from_covariate_search <- function(covariate_search,
                                                tags_yaml_path = "data/spec/tags.yaml",
                                                verbose = TRUE) {

  if (verbose) cat("📝 Generating tags.yaml from covariate search table...\n")

  # Handle both data frame and file path inputs
  if (is.character(covariate_search)) {
    # It's a file path
    if (!file.exists(covariate_search)) {
      stop(sprintf("Covariate search file not found: %s", covariate_search))
    }

    covariate_search_df <- tryCatch({
      readr::read_csv(covariate_search, show_col_types = FALSE)
    }, error = function(e) {
      stop(sprintf("Failed to read covariate search file: %s", e$message))
    })
  } else if (is.data.frame(covariate_search)) {
    # It's already a data frame
    covariate_search_df <- covariate_search
  } else {
    stop("covariate_search must be either a data frame or a file path")
  }

  if (verbose) cat(sprintf("  Found %d covariates in search table\n", nrow(covariate_search_df)))

  # Ensure required columns exist
  required_cols <- c("cov_to_test", "COVARIATE", "PARAMETER", "LEVELS", "FORMULA", "TIME_DEPENDENT")
  missing_cols <- setdiff(required_cols, names(covariate_search_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Generate covariate tag entries
  generate_tag_entry <- function(row) {
    # Use beta_ prefix directly from cov_to_test
    # e.g., "beta_BLWT_CL" stays as "beta_BLWT_CL"
    tag_name <- row$cov_to_test

    # If for some reason there's no beta_ prefix, add it
    if (!grepl("^beta_", tag_name)) {
      tag_name <- paste0("beta_", tag_name)
    }

    # Extract covariate description from cov_to_test
    # Remove the beta_ prefix to get the full covariate name (e.g., "BLWT_CL")
    if (grepl("^beta_", tag_name)) {
      cov_desc <- gsub("^beta_", "", tag_name)
    } else {
      # If no beta_ prefix, construct from COVARIATE_PARAMETER
      cov_desc <- paste0(row$COVARIATE, "_", row$PARAMETER)
    }

    # Build comment parts
    comment_parts <- character()

    # Determine type based on LEVELS
    if (!is.na(row$LEVELS) && row$LEVELS != "") {
      # Parse levels to determine if categorical
      if (grepl(";", row$LEVELS)) {
        # Format like "0;1;2" - count the levels
        levels_split <- strsplit(row$LEVELS, ";")[[1]]
        n_levels <- length(levels_split)
        comment_parts <- c(comment_parts, sprintf("Categorical %d-level", n_levels))
      } else if (grepl("^\\d+$", row$LEVELS)) {
        # Just a number indicating count of levels
        n_levels <- as.numeric(row$LEVELS)
        comment_parts <- c(comment_parts, sprintf("Categorical %d-level", n_levels))
      } else {
        # Unknown format, just mark as categorical
        comment_parts <- c(comment_parts, "Categorical")
      }
    } else {
      comment_parts <- c(comment_parts, "Continuous")
    }

    # Add formula type
    if (!is.na(row$FORMULA) && row$FORMULA != "") {
      formula_lower <- tolower(row$FORMULA)
      if (formula_lower %in% c("linear", "power", "exponential", "logistic")) {
        comment_parts <- c(comment_parts, formula_lower)
      }
    }

    # Add time dependency
    if (!is.na(row$TIME_DEPENDENT) && row$TIME_DEPENDENT != "") {
      time_dep <- tolower(row$TIME_DEPENDENT)
      if (time_dep %in% c("no", "false", "0")) {
        comment_parts <- c(comment_parts, "time-independent")
      } else if (time_dep %in% c("yes", "true", "1")) {
        comment_parts <- c(comment_parts, "time-dependent")
      }
    } else {
      comment_parts <- c(comment_parts, "time-independent")  # Default
    }

    # Build the final line
    comment <- paste(comment_parts, collapse = ", ")

    # Format the line with proper spacing
    # Calculate spacing for alignment (adjust as needed)
    tag_part <- sprintf('%s: "%s"', tag_name, cov_desc)
    spaces_needed <- max(1, 40 - nchar(tag_part))  # Align comments at column 40
    spacing <- paste(rep(" ", spaces_needed), collapse = "")

    sprintf('%s%s# %s', tag_part, spacing, comment)
  }

  # Generate all tag entries
  tag_entries <- sapply(seq_len(nrow(covariate_search_df)), function(i) {
    generate_tag_entry(covariate_search_df[i, ])
  })

  # Read existing tags.yaml or create new content
  if (file.exists(tags_yaml_path)) {
    if (verbose) cat(sprintf("  Reading existing tags.yaml from %s\n", tags_yaml_path))

    existing_lines <- readLines(tags_yaml_path)

    # Find the covariates section
    cov_start_idx <- grep("^## Covariates", existing_lines)

    if (length(cov_start_idx) == 0) {
      # No covariates section found, append it
      if (verbose) cat("  No '## Covariates' section found, appending to file\n")

      new_lines <- c(
        existing_lines,
        "",
        "## Covariates",
        tag_entries,
        ""
      )
    } else {
      # Found covariates section, replace ALL content between it and next section
      if (verbose) cat("  Found '## Covariates' section, replacing all content in section\n")

      # Find the next section (any line starting with #) after ## Covariates
      if (cov_start_idx[1] < length(existing_lines)) {
        # Look for next section starting from the line after ## Covariates
        remaining_lines <- (cov_start_idx[1] + 1):length(existing_lines)
        next_section_matches <- grep("^#", existing_lines[remaining_lines])

        if (length(next_section_matches) > 0) {
          # Calculate actual line number
          next_section_idx <- remaining_lines[next_section_matches[1]]

          # Keep everything before ## Covariates and everything from next section onward
          before_section <- existing_lines[1:cov_start_idx[1]]
          after_section <- existing_lines[next_section_idx:length(existing_lines)]

          new_lines <- c(
            before_section,
            tag_entries,
            "",
            after_section
          )
        } else {
          # No next section found, replace everything after ## Covariates
          before_section <- existing_lines[1:cov_start_idx[1]]
          new_lines <- c(
            before_section,
            tag_entries,
            ""
          )
        }
      } else {
        # ## Covariates is the last line
        before_section <- existing_lines[1:cov_start_idx[1]]
        new_lines <- c(
          before_section,
          tag_entries,
          ""
        )
      }
    }

  } else {
    # Create new file with minimal content
    if (verbose) cat(sprintf("  Creating new tags.yaml file at %s\n", tags_yaml_path))

    # Ensure directory exists
    dir_path <- dirname(tags_yaml_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      if (verbose) cat(sprintf("  Created directory: %s\n", dir_path))
    }

    new_lines <- c(
      "# Tags configuration file",
      "# Auto-generated from covariate search table",
      sprintf("# Generated: %s", Sys.Date()),
      "",
      "## Covariates",
      tag_entries,
      ""
    )
  }

  # Write the file
  tryCatch({
    writeLines(new_lines, tags_yaml_path)
    if (verbose) {
      cat(sprintf("✅ Successfully wrote %d covariate tags to %s\n",
                  length(tag_entries), tags_yaml_path))
    }
    return(TRUE)
  }, error = function(e) {
    cat(sprintf("❌ Failed to write tags.yaml: %s\n", e$message))
    return(FALSE)
  })
}


#' Generate Tags YAML with Search State Integration
#'
#' @description Wrapper function that uses search_state if available
#' @param search_state Optional. Search state containing covariate_search data or path
#' @param covariate_search Data frame or path to covariate search CSV (used if search_state not provided)
#' @param tags_yaml_path Path to tags.yaml file (default: "data/spec/tags.yaml")
#' @param verbose Logical. Print progress messages
#' @return Logical. TRUE if successful
#' @export
update_tags_yaml <- function(search_state = NULL,
                             covariate_search = NULL,
                             tags_yaml_path = "data/spec/tags.yaml",
                             verbose = TRUE) {

  # Determine covariate search source
  if (!is.null(search_state)) {
    # Try to get data frame first, then fall back to path
    if (!is.null(search_state$covariate_search) && is.data.frame(search_state$covariate_search)) {
      cov_source <- search_state$covariate_search
      if (verbose) cat("📋 Using covariate search data from search_state\n")
    } else if (!is.null(search_state$covariate_search_path)) {
      cov_source <- search_state$covariate_search_path
      if (verbose) cat("📋 Using covariate search path from search_state\n")
    } else {
      stop("search_state does not contain covariate_search data or path")
    }
  } else if (!is.null(covariate_search)) {
    cov_source <- covariate_search
  } else {
    stop("Either search_state or covariate_search must be provided")
  }

  # Call the main function
  generate_tags_from_covariate_search(
    covariate_search = cov_source,
    tags_yaml_path = tags_yaml_path,
    verbose = verbose
  )
}
