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
#' @param validate_parameters Logical. Validate parameter block formatting (default: TRUE)
#' @param require_base_run Logical. Require the base model to have a completed run
#'   (readable `.lst`/`.ext` with an OFV) before initializing (default: TRUE). Set
#'   to FALSE for SETUP/TESTING only -- it skips the run/OFV check so you can build
#'   the search_state and exercise covariate add/remove on control streams without
#'   running NONMEM. A real search still needs a completed base model OFV.
#' @param lookup_file Character or NULL. Optional path to lookup YAML for
#'   categorical covariate labels. If NULL, defaults to data/spec/lookup.yaml.
#' @param starting_model_number Optional integer. Sets the model counter manually.
#'   Use this when covariate search starts from an existing structural model
#'   and newly created covariate models should continue numbering from the
#'   last model already present in the workflow. For example, if the last
#'   existing model is `run10`, set `starting_model_number = 10` so the next model
#'   created by covariate search will be `run11`.
#' @return List containing complete search state configuration
#' @export
initialize_covariate_search <- function(base_model_path,
                                        data_file_path,
                                        covariate_search_path,
                                        models_folder = "models",
                                        timecol = "TIME",
                                        idcol = "ID",
                                        threads = 60,
                                        validate_parameters = TRUE,
                                        require_base_run = TRUE,
                                        lookup_file = NULL,
                                        starting_model_number = NULL) {

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

  # VALIDATION: Check that the ID column exists in the data
  # First try exact match, then search for columns containing "ID"
  if (!idcol %in% names(search_state$data_file)) {
    # Search for columns containing "ID" (case-insensitive)
    id_cols <- grep("ID", names(search_state$data_file), ignore.case = TRUE, value = TRUE)
    
    if (length(id_cols) == 1) {
      # Auto-detect single ID column (silently use it - no ambiguity)
      actual_idcol <- id_cols[1]
      search_state$idcol <- actual_idcol
      idcol <- actual_idcol
    } else if (length(id_cols) > 1) {
      # Multiple ID columns found
      available_cols <- paste(names(search_state$data_file), collapse = ", ")
      stop(
        sprintf("ID column '%s' not found in data file.\n", idcol),
        sprintf("Multiple potential ID columns found: %s\n", paste(id_cols, collapse = ", ")),
        sprintf("Please specify the correct one in initialize_covariate_search():\n"),
        sprintf("  idcol = '%s'  # Choose one of: %s\n", id_cols[1], paste(id_cols, collapse = ", ")),
        sprintf("\nAll available columns: %s\n", available_cols)
      )
    } else {
      # No ID column found
      available_cols <- paste(names(search_state$data_file), collapse = ", ")
      stop(
        sprintf("ID column '%s' not found in data file.\n", idcol),
        sprintf("No columns containing 'ID' were found.\n"),
        sprintf("Available columns: %s\n", available_cols),
        sprintf("Please specify the correct ID column name in initialize_covariate_search():\n"),
        sprintf("  idcol = 'COLUMN_NAME'\n")
      )
    }
  }

  cat(sprintf("✓ ID column '%s' verified in data\n", idcol))

  # VALIDATION: Check that the data file name in the model matches what user specified
  model_path <- file.path(models_folder, base_model_path)
  ctl_file <- if (file.exists(paste0(model_path, ".ctl"))) {
    paste0(model_path, ".ctl")
  } else if (file.exists(paste0(model_path, ".mod"))) {
    paste0(model_path, ".mod")
  } else {
    NULL
  }

  if (!is.null(ctl_file)) {
    modelcode <- readLines(ctl_file)
    # Find DATA line (case-insensitive, may have spaces)
    data_line <- grep("^\\s*\\$DATA", modelcode, ignore.case = TRUE, value = TRUE)
    if (length(data_line) > 0) {
      # Extract filename token from DATA statement (e.g., "$DATA mydata.csv IGNORE=@")
      data_tokens <- strsplit(trimws(data_line[1]), "[[:space:]]+")[[1]]
      if (length(data_tokens) < 2) {
        stop("Could not parse $DATA line in model: ", data_line[1])
      }
      data_filename <- gsub("^['\"]|['\"]$", "", data_tokens[2])
      # Compare using file names only (ignore relative/absolute directory paths)
      model_data_basename <- basename(data_filename)
      user_filename <- basename(data_file_path)
      
      # Compare file names only, case-insensitive
      if (!identical(tolower(model_data_basename), tolower(user_filename))) {
        stop(
          sprintf("⚠️  DATA FILE MISMATCH!\n"),
          sprintf("  Model '%s' references: %s\n", base_model_path, data_filename),
          sprintf("  You specified: %s\n", data_file_path),
          sprintf("  Filename mismatch: '%s' vs '%s'\n", model_data_basename, user_filename),
          sprintf("  Please ensure the data file name matches the $DATA line in your model,\n"),
          sprintf("  or update the data_file_path parameter in initialize_covariate_search()\n")
        )
      } else {
        cat(sprintf("✓ Data file name verified: %s\n", model_data_basename))
      }
    }
  }

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

  cat("Checking base model results...\n")
  if (isTRUE(require_base_run)) {
    validate_base_model_for_search(
      base_model_path = search_state$base_model,
      models_folder = search_state$models_folder
    )
  } else {
    # SETUP/TESTING mode: skip the run/OFV requirement, but still confirm the
    # base control stream exists so covariate add/remove have something to read.
    mp <- file.path(search_state$models_folder, search_state$base_model)
    if (!file.exists(paste0(mp, ".ctl")) && !file.exists(paste0(mp, ".mod"))) {
      stop("Base model control stream not found: ", mp, " (.ctl or .mod)")
    }
    cat("⚠️  require_base_run = FALSE: skipping base-model run/OFV check.\n",
        "   This search_state is for SETUP/TESTING (build + add/remove control\n",
        "   streams) only — a real search cannot run without a completed base OFV.\n",
        sep = "")
  }
  # Load tags and run validations
  search_state <- load_tags(search_state)
  search_state <- validate_setup(search_state)

  # Initialize search database and configuration
  search_state <- initialize_search_database_core(search_state)

  search_state <- initialize_search_config(
    search_state = search_state,
    lookup_file = lookup_file
  )
  search_state <- discover_existing_models(search_state)


  # Set model counter based on existing models
  search_state <- update_model_counter(search_state)
  # Optional manual override for model numbering
  if (!is.null(starting_model_number)) {

    if (!is.numeric(starting_model_number) ||
        length(starting_model_number) != 1 ||
        is.na(starting_model_number)) {
      stop("starting_model_number must be a single non-missing numeric value")
    }

    starting_model_number <- as.integer(starting_model_number)

    if (starting_model_number < 1) {
      stop("starting_model_number must be >= 1")
    }

    base_model_number <- suppressWarnings(
      as.integer(gsub("^run", "", base_model_path))
    )

    if (!is.na(base_model_number) && starting_model_number < base_model_number) {
      stop(
        "starting_model_number (", starting_model_number,
        ") cannot be lower than base model number (", base_model_number, ")"
      )
    }

    search_state$model_counter <- starting_model_number

    cat("Model counter manually set to: run", search_state$model_counter, "\n", sep = "")
  }

  scm_rds_dir <- file.path(search_state$models_folder, "scm_rds")
  if (!dir.exists(scm_rds_dir)) {
    dir.create(scm_rds_dir, recursive = TRUE)
    cat("📁 Created: models/scm_rds/ for interim saves\n")
  }
  if (validate_parameters) {
    validate_base_model_parameters(
      base_model_path = base_model_path,
      models_folder = models_folder,
      strict = TRUE  # Stops if issues found
    )
  }
  cat("CovariateSearcher (Core) initialized successfully!\n")
  cat("To run  covariate search use run_automated_scm_testing function \n")
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




#' Build a covariate parameter-search reference table
#'
#' @description
#' Combines a user-specified Parameter/Covariate/Category/Formula mapping with
#' subject-level longitudinal data to automatically compute reference values
#' (median for continuous covariates, most prevalent level for categorical
#' covariates) and detected levels from baseline (Time == 0) data, cross-check
#' categorical levels against a YAML covariate specification, and determine
#' whether each covariate is time-dependent using the FULL dataset (not just
#' baseline). The result is a long table ready for
#' \code{\link{validate_covariate_search_table}} /
#' \code{\link{initialize_covariate_search}}.
#'
#' @param data      A data.frame containing subject-level longitudinal data,
#'   including an ID column, a time column, and all covariates referenced in
#'   \code{Covariate}.
#' @param id        Name (string) of the subject identifier column in \code{data}.
#' @param time      Name (string) of the time column in \code{data}.
#' @param Parameter Character vector naming the PK/PD parameter for each row
#'   (e.g. "CL", "CL", "V"), aligned by position with the other spec vectors.
#' @param Covariate Character vector naming the covariate for each row.
#' @param Category  Character vector, one of "con" or "cat" per row.
#' @param Formula   Character vector. Per row, either a built-in shortcut --
#'   continuous: \code{"linear"}, \code{"power"}, \code{"exponential"};
#'   categorical: \code{"linear"} (per-level) or \code{"power"} (numeric levels)
#'   -- or a single-factor expression written in the reserved symbols \code{cov}
#'   (the covariate) and \code{ref} (its REFERENCE) plus one symbol per estimated
#'   THETA, e.g. \code{"EMAX*cov/(EC50+cov)"}. (\code{"power1"}/\code{"power0.75"}
#'   were removed: use \code{"power"} with a fixed \code{INIT} such as
#'   \code{"1 FIX"} / \code{"0.75 FIX"}.)
#' @param yaml_data A named list (parsed YAML) where each categorical
#'   covariate has a \code{values} element listing its valid numeric codes.
#' @param INIT      Optional character vector, aligned by position with the spec
#'   vectors, giving the initial \code{$THETA} value per row (e.g. \code{"0.1"},
#'   \code{"(0, 0.5, 2)"}, \code{"1 FIX"}, or a named spec for multi-parameter
#'   expressions such as \code{"EMAX=0.5; EC50=10"}). \code{NULL} (default) omits
#'   the column, so every beta uses the formula default (\code{0.1}).
#'
#' @return A data.frame with columns PARAMETER, COVARIATE, STATUS, FORMULA,
#'   LEVELS, REFERENCE, TIME_DEPENDENT (and INIT when supplied) -- one row per
#'   (Parameter, Covariate) pair, matching the input spec order.
#'
#' @seealso \code{\link{validate_covariate_search_table}},
#'   \code{\link{initialize_covariate_search}}
#' @importFrom rlang .data
#' @export
build_covariate_reference_table <- function(data, id, time,
                                            Parameter, Covariate, Category, Formula,
                                            yaml_data, INIT = NULL) {

  builtin_con <- c("linear", "power", "exponential")
  builtin_cat <- c("linear", "power")  # cat.linear (per-level) + cat.power (numeric levels)

  # ---- Step 1: build & validate spec ------------------------------------
  n <- length(Parameter)
  if (length(Covariate) != n || length(Category) != n || length(Formula) != n) {
    stop("Parameter, Covariate, Category, and Formula must all be the same length.")
  }
  if (!is.null(INIT) && length(INIT) != n) {
    stop("INIT must be NULL or the same length as Parameter (", n, ").")
  }

  spec_df <- data.frame(
    PARAMETER = Parameter,
    COVARIATE = Covariate,
    STATUS    = Category,
    FORMULA   = Formula,
    stringsAsFactors = FALSE
  )
  if (!is.null(INIT)) spec_df$INIT <- as.character(INIT)

  bad_category <- unique(spec_df$STATUS[!spec_df$STATUS %in% c("con", "cat")])
  if (length(bad_category) > 0) {
    stop("Category must be 'con' or 'cat'. Invalid value(s): ",
         paste(bad_category, collapse = ", "))
  }

  # A FORMULA is valid if it is a built-in shortcut for its STATUS, or a
  # single-factor expression that parses AND actually references `cov`. This
  # mirrors the package's own acceptance (built-in OR parseable expression, see
  # generate_tags_from_covariate_search / model_add_cov) and rejects removed
  # names like power1/power0.75, and typos, with an actionable message.
  formula_ok <- function(status, f) {
    f_chr    <- tolower(trimws(as.character(f)))
    builtins <- if (status == "con") builtin_con else builtin_cat
    if (f_chr %in% builtins) return(TRUE)
    d <- parse_covariate_expression(f)
    !is.null(d) && "cov" %in% all.vars(d$expr)
  }
  bad_idx <- which(!mapply(formula_ok, spec_df$STATUS, spec_df$FORMULA))
  if (length(bad_idx) > 0) {
    stop(
      "Invalid FORMULA at row(s) ", paste(bad_idx, collapse = ", "), ": ",
      paste(unique(as.character(spec_df$FORMULA[bad_idx])), collapse = ", "),
      ".\n  Use a built-in shortcut (continuous: ",
      paste(builtin_con, collapse = "/"), "; categorical: ",
      paste(builtin_cat, collapse = "/"),
      ") or a single-factor expression in cov/ref + parameter names",
      " (e.g. \"EMAX*cov/(EC50+cov)\").",
      "\n  Note: power1/power0.75 were removed — use \"power\" with a fixed",
      " INIT such as \"1 FIX\" or \"0.75 FIX\"."
    )
  }

  # Each covariate must have a single consistent Category across all its rows
  # (e.g. WT cannot be "con" under CL and "cat" under V).
  cov_category_counts <- tapply(spec_df$STATUS, spec_df$COVARIATE, function(x) length(unique(x)))
  inconsistent_covs <- names(cov_category_counts)[cov_category_counts > 1]
  if (length(inconsistent_covs) > 0) {
    stop("Covariate(s) have inconsistent Category (con vs cat) across rows: ",
         paste(inconsistent_covs, collapse = ", "))
  }

  unique_covs <- unique(spec_df$COVARIATE)
  missing_cols <- setdiff(unique_covs, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Covariate column(s) not found in 'data': ", paste(missing_cols, collapse = ", "))
  }
  if (!(id %in% colnames(data)))   stop("ID column '", id, "' not found in 'data'.")
  if (!(time %in% colnames(data))) stop("Time column '", time, "' not found in 'data'.")

  cov_status <- setNames(spec_df$STATUS[match(unique_covs, spec_df$COVARIATE)], unique_covs)

  # ---- Step 2: baseline data (Time == 0), first row per ID --------------
  baseline <- data %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::filter(.data[[time]] == 0) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # ---- Step 3: REFERENCE + LEVELS per unique covariate -------------------
  compute_ref_levels <- function(cov) {
    status <- cov_status[[cov]]
    x <- baseline[[cov]]
    x <- x[!is.na(x)]

    if (status == "con") {
      list(reference = as.character(round(median(x), 2)), levels = NA_character_)
    } else {
      tab <- table(x)
      levels_found <- sort(unique(as.character(x)))
      most_prevalent <- names(tab)[which.max(tab)]
      list(reference = most_prevalent, levels = paste(levels_found, collapse = ";"))
    }
  }

  ref_levels <- lapply(unique_covs, compute_ref_levels)
  names(ref_levels) <- unique_covs

  # ---- Step 4: YAML level check (categorical covariates only) -----------
  for (cov in unique_covs) {
    if (cov_status[[cov]] != "cat") next

    if (is.null(yaml_data[[cov]])) {
      stop("Covariate '", cov, "' not found in yaml_data specification.")
    }

    yaml_values  <- as.character(yaml_data[[cov]]$values)
    data_levels  <- strsplit(ref_levels[[cov]]$levels, ";")[[1]]

    extra_in_data <- setdiff(data_levels, yaml_values)
    if (length(extra_in_data) > 0) {
      stop("Covariate '", cov, "': level(s) found in baseline data not defined in yaml_data: ",
           paste(extra_in_data, collapse = ", "))
    }

    extra_in_yaml <- setdiff(yaml_values, data_levels)
    if (length(extra_in_yaml) > 0) {
      warning("Covariate '", cov, "': yaml_data defines level(s) never observed in baseline data: ",
              paste(extra_in_yaml, collapse = ", "))
    }
  }

  # ---- Step 5: TIME_DEPENDENT from FULL data -----------------------------
  compute_time_dependent <- function(cov) {
    df <- data[!is.na(data[[cov]]), c(id, cov)]
    n_distinct_per_id <- tapply(df[[cov]], df[[id]], function(v) length(unique(v)))
    if (any(n_distinct_per_id > 1)) "Yes" else "No"
  }

  time_dep <- setNames(vapply(unique_covs, compute_time_dependent, character(1)), unique_covs)

  # ---- Step 6: assemble final long table ---------------------------------
  spec_df$LEVELS         <- vapply(spec_df$COVARIATE, function(cov) ref_levels[[cov]]$levels, character(1))
  spec_df$REFERENCE      <- vapply(spec_df$COVARIATE, function(cov) ref_levels[[cov]]$reference, character(1))
  spec_df$TIME_DEPENDENT <- unname(time_dep[spec_df$COVARIATE])

  out_cols <- c("PARAMETER", "COVARIATE", "STATUS", "FORMULA", "LEVELS", "REFERENCE", "TIME_DEPENDENT")
  if (!is.null(INIT)) out_cols <- c(out_cols, "INIT")
  spec_df[, out_cols]
}




#' Validate covariate search table
#'
#' Performs reusable checks on the covariate search table and returns the
#' validated table. Creates `cov_to_test` if it does not exist.
#'
#' @param covariate_search data.frame with covariate search specification
#' @param data_file data.frame with analysis dataset
#'
#' @return validated covariate_search data.frame
#' @export
validate_covariate_search_table <- function(covariate_search, data_file) {

  cat("Checking covariate search table...\n")

  if (!is.data.frame(covariate_search)) {
    stop("covariate_search must be a data.frame")
  }

  if (!is.data.frame(data_file)) {
    stop("data_file must be a data.frame")
  }

  required_cols <- c(
    "PARAMETER", "COVARIATE", "STATUS", "FORMULA",
    "LEVELS", "REFERENCE", "TIME_DEPENDENT"
  )

  missing_cols <- setdiff(required_cols, names(covariate_search))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in covariate_search: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (any(is.na(covariate_search$COVARIATE) |
          trimws(as.character(covariate_search$COVARIATE)) == "")) {
    stop("covariate_search contains missing or empty values in COVARIATE")
  }

  if (any(is.na(covariate_search$PARAMETER) |
          trimws(as.character(covariate_search$PARAMETER)) == "")) {
    stop("covariate_search contains missing or empty values in PARAMETER")
  }

  # Single-token COVARIATE / PARAMETER: the beta_<COV>_<PARAM>[_<theta|level>] tag
  # convention is positional (parts[2]=covariate, parts[3]=parameter, trailing
  # tokens = theta names or a numeric level), so names must not contain underscores
  # or they cannot be parsed back unambiguously.
  bad_cov <- unique(as.character(covariate_search$COVARIATE)[grepl("_", covariate_search$COVARIATE)])
  bad_par <- unique(as.character(covariate_search$PARAMETER)[grepl("_", covariate_search$PARAMETER)])
  if (length(bad_cov) > 0 || length(bad_par) > 0) {
    stop(
      "COVARIATE and PARAMETER must be single tokens without underscores ",
      "(the beta_<COV>_<PARAM> tag convention is positional). Offending ",
      if (length(bad_cov) > 0) paste0("COVARIATE(s): ", paste(bad_cov, collapse = ", ")) else "",
      if (length(bad_cov) > 0 && length(bad_par) > 0) "; " else "",
      if (length(bad_par) > 0) paste0("PARAMETER(s): ", paste(bad_par, collapse = ", ")) else "",
      "."
    )
  }

  if (!"cov_to_test" %in% names(covariate_search)) {
    covariate_search$cov_to_test <- paste0(
      "beta_",
      covariate_search$COVARIATE,
      "_",
      covariate_search$PARAMETER
    )
    cat("Added cov_to_test column.\n")
  }

  duplicated_tags <- unique(covariate_search$cov_to_test[
    duplicated(covariate_search$cov_to_test)
  ])

  if (length(duplicated_tags) > 0) {
    stop(
      "Duplicate cov_to_test values found: ",
      paste(duplicated_tags, collapse = ", ")
    )
  }

  missing_covs <- setdiff(unique(covariate_search$COVARIATE), names(data_file))
  if (length(missing_covs) > 0) {
    stop(
      "The following covariates are missing from the dataset: ",
      paste(missing_covs, collapse = ", ")
    )
  }

  cat_rows <- covariate_search[
    as.character(covariate_search$STATUS) == "cat",
    ,
    drop = FALSE
  ]

  if (nrow(cat_rows) > 0) {
    for (cov_name in unique(cat_rows$COVARIATE)) {

      cov_rows <- cat_rows[cat_rows$COVARIATE == cov_name, , drop = FALSE]

      levels_values <- unique(as.character(cov_rows$LEVELS))
      levels_values <- levels_values[!is.na(levels_values)]
      levels_values <- trimws(levels_values)
      levels_values <- levels_values[levels_values != ""]

      if (length(levels_values) == 0) {
        stop(
          "Categorical covariate '", cov_name,
          "' has STATUS == 'cat' but no LEVELS specified"
        )
      }

      if (length(unique(levels_values)) > 1) {
        stop(
          "Categorical covariate '", cov_name,
          "' has inconsistent LEVELS definitions: ",
          paste(unique(levels_values), collapse = " | ")
        )
      }

      declared_levels <- unlist(strsplit(levels_values[1], ";", fixed = TRUE))
      declared_levels <- trimws(declared_levels)
      declared_levels <- declared_levels[declared_levels != ""]

      declared_levels_num <- suppressWarnings(as.numeric(declared_levels))
      if (any(is.na(declared_levels_num))) {
        stop(
          "LEVELS for categorical covariate '", cov_name,
          "' must be numeric values separated by ';'. Found: ",
          levels_values[1]
        )
      }

      observed_levels_num <- sort(unique(stats::na.omit(data_file[[cov_name]])))

      if (!all(observed_levels_num %in% declared_levels_num)) {
        missing_levels <- setdiff(observed_levels_num, declared_levels_num)
        stop(
          "Covariate '", cov_name,
          "' has values in dataset not present in LEVELS: ",
          paste(missing_levels, collapse = ", ")
        )
      }

      # Check REFERENCE is within declared LEVELS
      ref_values <- unique(as.character(cov_rows$REFERENCE))
      ref_values <- ref_values[!is.na(ref_values) & trimws(ref_values) != ""]

      if (length(ref_values) > 0) {
        ref_num <- suppressWarnings(as.numeric(ref_values))
        invalid_ref <- ref_values[is.na(ref_num)]
        if (length(invalid_ref) > 0) {
          stop(
            "Categorical covariate '", cov_name,
            "' has non-numeric REFERENCE value(s): ",
            paste(invalid_ref, collapse = ", ")
          )
        }
        outside_levels <- ref_num[!ref_num %in% declared_levels_num]
        if (length(outside_levels) > 0) {
          stop(
            "Categorical covariate '", cov_name,
            "' has REFERENCE value(s) not present in LEVELS (", levels_values[1], "): ",
            paste(outside_levels, collapse = ", ")
          )
        }
      }
    }
  }

  cat("Covariate search table check passed.\n")

  # Validate LEVELS column for all rows: must be empty/NA or numeric semicolon-separated
  for (i in seq_len(nrow(covariate_search))) {
    levels_val <- as.character(covariate_search$LEVELS[i])
    if (!is.na(levels_val) && trimws(levels_val) != "") {
      parts <- trimws(unlist(strsplit(levels_val, ";", fixed = TRUE)))
      parts <- parts[parts != ""]
      parts_num <- suppressWarnings(as.numeric(parts))
      if (any(is.na(parts_num))) {
        stop(
          "Row ", i, ": LEVELS for covariate '", covariate_search$COVARIATE[i],
          "' on parameter '", covariate_search$PARAMETER[i],
          "' contains non-numeric values: '", levels_val,
          "'. LEVELS must be numeric codes separated by ';' (e.g., '0;1;2')"
        )
      }
    }
  }

  return(covariate_search)
}
#' Validate initialized search setup
#'
#' Performs high-level setup validation for an initialized search_state and
#' reuses validate_covariate_search_table() for table/data checks.
#'
#' @param search_state list
#'
#' @return updated search_state
#' @export
validate_setup <- function(search_state) {

  if (!is.list(search_state)) {
    stop("search_state must be a list")
  }

  required_fields <- c("covariate_search", "data_file")
  missing_fields <- setdiff(required_fields, names(search_state))

  if (length(missing_fields) > 0) {
    stop(
      "search_state is missing required fields: ",
      paste(missing_fields, collapse = ", ")
    )
  }

  search_state$covariate_search <- validate_covariate_search_table(
    covariate_search = search_state$covariate_search,
    data_file = search_state$data_file
  )

  validate_covariate_parameter_mapping(
    covariate_search = search_state$covariate_search,
    model_name = search_state$base_model,
    models_folder = search_state$models_folder,
    strict = TRUE,
    verbose = TRUE
  )

  validate_param_transformations(
    covariate_search = search_state$covariate_search,
    data_file        = search_state$data_file,
    id_col           = search_state$idcol,
    model_name       = search_state$base_model,
    models_folder    = search_state$models_folder
  )

  cat("All validation checks passed!\n")
  search_state
}

#' Validate parameter transformations for population covariates
#'
#' @description
#' For every parameter that will receive a POPULATION (time-constant) covariate,
#' checks that the base model writes it in a recognised form -- normal-scale
#' \code{PARAM = TV * EXP(ETA)} or log-scale \code{PARAM = EXP(TV + ETA)} -- so
#' \code{model_add_cov} knows whether to render the covariate multiplicatively
#' (\code{*}) or additively (\code{+}). Fails fast at initialization on any
#' parameter whose parameterization cannot be classified. Also requires a
#' log-parameterized population target to have a separate \code{TV_<param>} line
#' (the additive covariate is written on it, inside the \code{EXP}); an inline
#' \code{PARAM = EXP(THETA + ETA)} is rejected with a refactor message, because
#' the additive term would otherwise land outside the \code{EXP}. Time-varying
#' covariates are placed on the individual parameter line (always multiplicative)
#' and so do not require either check.
#'
#' @param covariate_search data.frame with COVARIATE, PARAMETER, TIME_DEPENDENT.
#' @param data_file data.frame with the analysis dataset (for the time-varying check).
#' @param id_col Character. Subject identifier column name.
#' @param model_name Character. Base model name (without extension).
#' @param models_folder Character. Folder containing the model (default "models").
#' @return Invisibly \code{TRUE} if all population parameters are classifiable.
#' @export
validate_param_transformations <- function(covariate_search, data_file, id_col,
                                           model_name, models_folder = "models") {

  model_path <- file.path(models_folder, model_name)
  ctl <- if (file.exists(paste0(model_path, ".ctl"))) {
    paste0(model_path, ".ctl")
  } else if (file.exists(paste0(model_path, ".mod"))) {
    paste0(model_path, ".mod")
  } else {
    NULL
  }
  if (is.null(ctl)) return(invisible(TRUE))   # base-model existence checked elsewhere
  modelcode <- readLines(ctl, warn = FALSE)

  # A covariate is placed at population level (and thus needs a classifiable
  # transform) when it is time-CONSTANT: at most one distinct value per subject.
  # Mirror the data-based check model_add_cov uses.
  is_time_varying <- function(cov) {
    if (!cov %in% names(data_file) || !id_col %in% names(data_file)) return(FALSE)
    lv <- tapply(data_file[[cov]], data_file[[id_col]],
                 function(x) length(unique(x[!is.na(x)])))
    isTRUE(max(lv, na.rm = TRUE) > 1)
  }

  pop_params <- character(0)
  for (i in seq_len(nrow(covariate_search))) {
    if (!is_time_varying(as.character(covariate_search$COVARIATE[i]))) {
      pop_params <- c(pop_params, as.character(covariate_search$PARAMETER[i]))
    }
  }
  pop_params <- unique(pop_params)

  bad <- pop_params[vapply(
    pop_params,
    function(p) identical(detect_param_transform(modelcode, p), "unknown"),
    logical(1)
  )]

  if (length(bad) > 0) {
    stop(
      "Cannot determine the parameterization of parameter(s): ",
      paste(bad, collapse = ", "),
      ". Population (time-constant) covariates require the base model to write the ",
      "parameter as normal-scale '", bad[1], " = TV * EXP(ETA)' or log-scale '",
      bad[1], " = EXP(TV + ETA)'. Adjust the base model or covariate table."
    )
  }

  # A population covariate on a LOG-parameterized parameter is written additively
  # INSIDE the EXP, which model_add_cov does by appending to the TV_<param> line.
  # An inline log form (PARAM = EXP(THETA + ETA), with no separate TV_ line) would
  # receive the additive term OUTSIDE the EXP -> wrong. Require a TV_ line up front.
  log_params <- pop_params[vapply(
    pop_params,
    function(p) identical(detect_param_transform(modelcode, p), "log"),
    logical(1)
  )]
  no_tv <- log_params[vapply(
    log_params,
    function(p) length(grep(paste0("^\\s*TV_", p, "\\b"), modelcode)) == 0L,
    logical(1)
  )]
  if (length(no_tv) > 0) {
    p1 <- no_tv[1]
    stop(
      "Log-parameterized parameter(s) written inline without a TV_ line: ",
      paste(no_tv, collapse = ", "),
      ". A population (time-constant) covariate on a log parameter is added ",
      "additively inside the EXP, which requires a typical-value line. Refactor ",
      "the base model, e.g.\n",
      "    TV_", p1, " = THETA(n)\n",
      "    ", p1, " = EXP(TV_", p1, " + ETA(n))\n",
      "then re-run. (An inline '", p1, " = EXP(THETA + ETA)' would place the ",
      "covariate term outside the EXP.)"
    )
  }

  # A typical-value line written as `TV_<param> = EXP(THETA(n))` (with
  # `<param> = TV_<param> * EXP(ETA)`) is a LOG parameterization DISGUISED in
  # normal-IIV form: detect_param_transform inspects only the `<param> =` line,
  # sees `TV * EXP(ETA)`, and reports "normal", so a population covariate is
  # placed multiplicatively on a typical value that is really on the log scale.
  # It is mathematically EXP(THETA) * EXP(ETA) == EXP(THETA + ETA); forbid it and
  # require the canonical log form so the transform is detected unambiguously.
  disguised <- pop_params[vapply(pop_params, function(p) {
    ln <- grep(paste0("^\\s*TV_", p, "\\b"), modelcode, value = TRUE)
    if (length(ln) == 0L) return(FALSE)
    rhs <- trimws(sub(";.*$", "", sub("^[^=]*=", "", ln[length(ln)])))
    grepl("\\bEXP\\s*\\(", rhs) && grepl("\\bTHETA\\b", rhs)
  }, logical(1))]
  if (length(disguised) > 0) {
    p1 <- disguised[1]
    stop(
      "Typical-value line(s) written as 'TV_", p1, " = EXP(THETA(n))': ",
      paste(disguised, collapse = ", "),
      ". This is a log parameterization disguised in normal form (TV = EXP(THETA), ",
      p1, " = TV * EXP(ETA)); the covariate would be placed multiplicatively on a ",
      "log-scale typical value. Refactor the base model to the canonical log form:\n",
      "    TV_", p1, " = THETA(n)\n",
      "    ", p1, " = EXP(TV_", p1, " + ETA(n))\n",
      "then re-run."
    )
  }

  invisible(TRUE)
}

#' Validate base model readiness for covariate search
#'
#' Checks that the selected base model:
#' - exists in the models folder
#' - finished successfully
#' - has a readable OFV result
#'
#' This function should be used inside `initialize_covariate_search()`
#' before starting the actual covariate search workflow.
#'
#' @param base_model_path Character. Base model name, for example `"run6"`.
#'   Should be provided without file extension.
#' @param models_folder Character. Path to the folder containing model files.
#'
#' @return Logical `TRUE` if the base model is valid for search initialization.
#' @export
validate_base_model_for_search <- function(base_model_path,
                                           models_folder = "models") {

  model_path <- file.path(models_folder, base_model_path)

  ctl_exists <- file.exists(paste0(model_path, ".ctl"))
  mod_exists <- file.exists(paste0(model_path, ".mod"))

  if (!ctl_exists && !mod_exists) {
    stop("Base model file not found: ", model_path, " (.ctl or .mod)")
  }

  model_status <- get_model_status_from_files(model_path)

  if (!identical(model_status, "completed")) {
    lst_info <- tryCatch(
      read_nonmem_lst(model_path),
      error = function(e) list(found = FALSE, error_message = paste("LST read error:", e$message))
    )

    ext_info <- tryCatch(
      read_nonmem_ext(model_path),
      error = function(e) list(found = FALSE, error = paste("EXT read error:", e$message), ofv = NA_real_)
    )

    failure_reason <- NULL

    if (!is.null(lst_info$error_message) && !is.na(lst_info$error_message) && nzchar(lst_info$error_message)) {
      failure_reason <- lst_info$error_message
    } else if (!is.null(lst_info$error_excerpt) && !is.na(lst_info$error_excerpt) && nzchar(lst_info$error_excerpt)) {
      failure_reason <- paste("LST error excerpt:", lst_info$error_excerpt)
    } else if (!is.null(ext_info$error) && !is.na(ext_info$error) && nzchar(ext_info$error)) {
      failure_reason <- ext_info$error
    } else if (isFALSE(ext_info$found)) {
      failure_reason <- "EXT results missing or unreadable"
    } else if (is.null(ext_info$ofv) || length(ext_info$ofv) == 0 || is.na(ext_info$ofv[1])) {
      failure_reason <- "OFV missing or unreadable in EXT results"
    } else {
      failure_reason <- "No additional diagnostic details available"
    }

    stop(
      "Base model '", base_model_path,
      "' is not ready for covariate search. Current status: ", model_status,
      ". Reason: ", failure_reason
    )
  }

  ext_data <- tryCatch(
    read_nonmem_ext(model_path),
    error = function(e) NULL
  )

  if (is.null(ext_data)) {
    stop(
      "Base model '", base_model_path,
      "' does not have readable .ext results"
    )
  }

  if (!"ofv" %in% names(ext_data)) {
    stop(
      "Base model '", base_model_path,
      "' .ext results do not contain OFV"
    )
  }

  if (length(ext_data$ofv) == 0 || is.na(ext_data$ofv[1])) {
    stop(
      "Base model '", base_model_path,
      "' does not have a readable OFV result"
    )
  }

  return(TRUE)
}


#' Initialize Search Configuration
#'
#' @title Initialize search configuration parameters
#' @description Sets up default configuration for SCM workflow
#' @param search_state List containing search state
#' @param lookup_file Character or NULL. Optional path to lookup YAML for
#'   categorical covariate labels.
#' @return Updated search_state with initialized configuration
#' @export
initialize_search_config <- function(search_state, lookup_file = NULL) {
  resolved_lookup_file <- if (!is.null(lookup_file)) {
    lookup_file
  } else {
    file.path("data", "spec", "lookup.yaml")
  }

  search_state$search_config <- list(
    forward_p_value = 0.05,        # p-value for forward selection
    backward_p_value = 0.001,      # default p-value for backward elimination (stricter; overridden by a user-supplied value)
    max_rse_threshold = 50,
    timeout_minutes = 3600,
    threads = search_state$threads,
    lookup_file = resolved_lookup_file,
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

  # Validate FORMULA for continuous covariates: each must be a built-in shortcut
  # (linear/power/exponential) OR a valid single-factor expression written in
  # cov/ref + parameter names (e.g. "EMAX*cov/(EC50+cov)").
  builtin_con <- c("linear", "power", "exponential")
  con_idx <- which(tolower(as.character(covariate_search_df$STATUS)) == "con")
  invalid_formulas <- character(0)
  for (i in con_idx) {
    f <- covariate_search_df$FORMULA[i]
    ok <- tolower(as.character(f)) %in% builtin_con ||
      !is.null(parse_covariate_expression(f))
    if (!ok) invalid_formulas <- c(invalid_formulas, as.character(f))
  }

  if (length(invalid_formulas) > 0) {
    stop(sprintf(
      "Invalid FORMULA for continuous covariate(s): %s\nUse a built-in (%s) or a valid expression in cov/ref + parameter names.",
      paste(unique(invalid_formulas), collapse = ", "),
      paste(builtin_con, collapse = ", ")
    ))
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
