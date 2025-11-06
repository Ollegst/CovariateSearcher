# =============================================================================
# MODEL OUTPUT TABLES
# File: R/model-output-tables.R
# Part of CovariateSearcher Package
# Functions for creating formatted model parameter tables
# =============================================================================

#' Apply PPS Theme to Flextable
#'
#' @description Internal function to apply consistent formatting to flextables
#' @param x A flextable object
#' @return A formatted flextable object with PPS theme applied
#' @keywords internal
#' @export
theme_pps_table <- function(x) {
  # Set global defaults
  flextable::set_flextable_defaults(big.mark = "")

  # Define borders
  big_border <- officer::fp_border(color = "black", width = 1)
  std_border <- officer::fp_border(color = "gray", width = 0.5)

  # Typography settings
  font_name_table <- "Times New Roman"
  font_size_table <- 10

  if (!inherits(x, "flextable")) {
    stop("theme_pps_table supports only flextable objects.")
  }

  # Apply borders
  x <- flextable::border_remove(x)
  x <- flextable::border_inner(x, part = "all", border = std_border)
  x <- flextable::border_outer(x, part = "all", border = std_border)
  x <- flextable::hline_bottom(x, part = "header", border = big_border)
  x <- flextable::hline_bottom(x, part = "body", border = big_border)

  # Apply text formatting
  x <- flextable::bold(x = x, bold = TRUE, part = "header")
  x <- flextable::fix_border_issues(x, part = "all")

  # Apply font sizes
  x <- flextable::fontsize(x = x, size = font_size_table, part = "header")
  x <- flextable::fontsize(x = x, size = font_size_table, part = "body")
  x <- flextable::fontsize(x = x, size = font_size_table - 1, part = "footer")

  # Apply font and alignment
  x <- flextable::font(x = x, fontname = font_name_table, part = "all")
  x <- flextable::align(x = x, i = 1, j = c(-1), align = "center", part = "header")
  x <- flextable::set_table_properties(x = x, layout = "autofit", width = 1)

  return(x)
}


#' Extract Parameters from NONMEM Control File
#'
#' @description Internal function to extract parameter definitions from control file blocks
#' @param lines Character vector of control file lines
#' @param block_tag Character string identifying the block (e.g., "THETA", "OMEGA")
#' @param remove_prefix Logical. Remove numeric prefix from parameter names (default: FALSE)
#' @return A tibble with parameter names and transformations
#' @keywords internal
extract_params <- function(lines, block_tag, remove_prefix = FALSE) {

  # Find all indices where the block starts
  block_starts <- which(stringr::str_detect(
    lines,
    stringr::regex(paste0("^\\$", block_tag), ignore_case = TRUE)
  ))

  # Process each block header found
  params <- purrr::map_dfr(block_starts, function(start_idx) {
    # Get the lines after the header
    sub_lines <- lines[(start_idx + 1):length(lines)]

    # The block ends when the next line starting with "$" is encountered
    end_idx <- dplyr::if_else(
      any(stringr::str_detect(sub_lines, "^\\$")),
      min(which(stringr::str_detect(sub_lines, "^\\$"))) - 1,
      length(sub_lines)
    )

    block_lines <- sub_lines[1:end_idx] %>%
      # Remove blank lines
      purrr::keep(~ stringr::str_detect(.x, "\\S"))

    # Create a tibble and split each line by semicolon
    tibble::tibble(line = block_lines) %>%
      tidyr::separate(
        line,
        into = c("field1", "param", "field3", "transformation"),
        sep = ";",
        fill = "right",
        extra = "drop"
      ) %>%
      dplyr::mutate(
        param = stringr::str_trim(param),
        trans = stringr::str_trim(transformation)
      ) %>%
      # Remove numeric prefix for theta blocks if requested
      dplyr::mutate(
        param = if (remove_prefix) stringr::str_remove(param, "^[0-9]+_") else param
      ) %>%
      dplyr::filter(param != "") %>%
      dplyr::select(param, trans)
  })

  return(params)
}


#' Extract Model Parameters from Control File
#'
#' @description Internal function to extract all parameter names from a NONMEM control file
#' @param model_name Character string. Name of the model (without .ctl extension)
#' @param models_folder Character string. Path to models folder (default: "models")
#' @return A list containing THETAS, OMEGAS, and SIGMA parameters with transformations
#' @keywords internal
extract_model_params <- function(model_name, models_folder = "models") {
  # Construct file path
  ctl_path <- file.path(models_folder, paste0(model_name, ".ctl"))

  if (!file.exists(ctl_path)) {
    stop(sprintf("Control file not found: %s", ctl_path))
  }

  # Read the file as a vector of lines
  lines <- readLines(ctl_path)

  # Extract theta parameter names (removing numeric prefixes)
  theta_params <- extract_params(lines, "THETA", remove_prefix = TRUE)

  # Extract omega parameter names (keeping the names as-is)
  omega_params <- extract_params(lines, "OMEGA", remove_prefix = FALSE)

  # Extract sigma parameter names
  sigma_params <- extract_params(lines, "SIGMA", remove_prefix = FALSE)

  # Return a named list with the results
  list(
    THETAS = theta_params,
    OMEGAS = omega_params,
    SIGMA = sigma_params
  )
}


#' Get Model Parameters and Statistics
#'
#' @description Internal function to extract and format model parameters with estimates and statistics
#' @param model_number Character string. Model name/number
#' @param count_model Integer. Number of models being compared
#' @param shrinkage Character string. Type of shrinkage to report ("etasd", "etavr", "ebvsd", "ebvvr")
#' @param models_folder Character string. Path to models folder
#' @param spec_pk List. Optional parameter specifications with labels and units
#' @return A formatted data frame with model parameters and statistics
#' @keywords internal
get_param2 <- function(model_number,
                       count_model,
                       shrinkage = "etasd",
                       models_folder = "models",
                       spec_pk = NULL) {

  # Read model using bbr
  model_path <- file.path(models_folder, model_number)
  model_obj <- bbr::read_model(model_path)
  model_stats <- bbr::model_summary(model_obj)

  # Extract shrinkage values based on type
  shrinkage_value <- dplyr::case_when(
    shrinkage == "etasd" ~ c(
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eta_sd,
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eps_sd
    ),
    shrinkage == "etavr" ~ c(
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eta_vr,
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eps_vr
    ),
    shrinkage == "ebvsd" ~ c(
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$ebv_sd,
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eps_sd
    ),
    shrinkage == "ebvvr" ~ c(
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$ebv_vr,
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eps_vr
    )
  )

  # Extract OFV
  OFV <- data.table::data.table(
    parameter_names = "OFV",
    Parameter = tail(model_stats$ofv, 1)[[1]]$ofv_no_constant
  )

  # Extract parameter definitions from control file
  params_list <- extract_model_params(model_number, models_folder)

  # Get parameter estimates
  param_est <- model_stats %>%
    bbr::param_estimates() %>%
    dplyr::mutate(dplyr::across(where(is.numeric))) %>%
    dplyr::filter(random_effect_sd != 0 | is.na(random_effect_sd) | diag == TRUE) %>%
    dplyr::mutate(
      parameter_names = rbind(
        params_list$THETAS,
        params_list$OMEGAS,
        params_list$SIGMA
      )$param,
      trans = rbind(
        params_list$THETAS,
        params_list$OMEGAS,
        params_list$SIGMA
      )$trans
    ) %>%
    dplyr::mutate(
      estimate = dplyr::case_when(
        diag == FALSE ~ random_effect_sd,   # Use correlation for off-diagonal
        TRUE ~ estimate                      # Keep original for others
      ),
      stderr = dplyr::case_when(
        diag == FALSE ~ random_effect_sdse,  # Use correlation SE for off-diagonal
        TRUE ~ stderr                         # Keep original for others
      )
    ) %>%
    dplyr::mutate(
      Parameter = dplyr::case_when(
        is.na(random_effect_sd) & trans == "RATIO" ~ estimate,
        is.na(random_effect_sd) & trans == "LOG" ~ exp(estimate),
        diag == TRUE ~ 100 * sqrt(ifelse(exp(estimate) - 1 >= 0, exp(estimate) - 1, NA)),
        diag == FALSE ~ estimate
      ),
      RSE = dplyr::case_when(
        is.na(random_effect_sd) & trans == "RATIO" ~ abs((stderr/estimate) * 100),
        is.na(random_effect_sd) & trans == "LOG" ~ abs(stderr) * 100,
        diag == TRUE ~ 100 * stderr * exp(estimate) / (2 * exp(estimate) - 1),
        diag == FALSE ~ 100 * stderr / estimate
      )
    )

  # Extract shrinkage for diagonal elements
  shrinkage_df <- dplyr::filter(param_est, diag == TRUE) %>%
    dplyr::select(parameter_names, shrinkage) %>%
    dplyr::rename(SHRINKAGE = shrinkage) %>%
    dplyr::mutate(SHRINKAGE = shrinkage_value)

  # Combine all parameters
  param_est <- param_est %>%
    dplyr::select(parameter_names, Parameter, RSE, fixed) %>%
    dplyr::left_join(shrinkage_df, by = "parameter_names") %>%
    dplyr::bind_rows(OFV) %>%
    dplyr::rowwise()

  # Add labels and comments if spec_pk is provided
  if (!is.null(spec_pk)) {
    param_est <- param_est %>%
      dplyr::mutate(
        comment = dplyr::case_when(
          parameter_names %in% names(spec_pk) ~ {
            comment_value <- spec_pk[[parameter_names]]$comment
            if (!is.null(comment_value)) comment_value else NA_character_
          },
          TRUE ~ "Parameter-Covariate relationships"
        ),
        label = dplyr::case_when(
          grepl("^beta_", parameter_names) ~ {
            # Split the parameter name
            parts <- strsplit(parameter_names, "_")[[1]]

            # beta_COV_PARAM or beta_COV_PARAM_LEVEL
            # parts[1] = "beta"
            # parts[2] = covariate name (e.g., "SMOKING")
            # parts[3:n-1] = parameter name (e.g., "F1" or "CL" or "V")
            # parts[n] = level (optional, if categorical with >2 levels)

            if (length(parts) < 3) {
              # Malformed name
              NA_character_
            } else {
              # Check if last part is a numeric level
              last_part <- parts[length(parts)]
              is_level <- !is.na(suppressWarnings(as.numeric(last_part)))

              if (is_level && length(parts) >= 4) {
                # Has level: beta_SMOKING_F1_2
                # Covariate: parts[2] = "SMOKING"
                # Parameter: parts[3:(length-1)] = "F1"
                # Level: parts[length] = "2"
                cov_name <- parts[2]
                param_parts <- parts[3:(length(parts)-1)]
                param_name <- paste(param_parts, collapse = "_")
                level <- last_part

                # Look up parameter in spec_pk
                if (param_name %in% names(spec_pk)) {
                  paste0("Effect of ", cov_name, " level ", level, " on ", spec_pk[[param_name]]$label)
                } else {
                  paste0("Effect of ", cov_name, " level ", level, " on ", param_name)
                }
              } else {
                # No level: beta_AGE_CL
                # Covariate: parts[2] = "AGE"
                # Parameter: parts[3:end] = "CL"
                cov_name <- parts[2]
                param_parts <- parts[3:length(parts)]
                param_name <- paste(param_parts, collapse = "_")

                # Look up parameter in spec_pk
                if (param_name %in% names(spec_pk)) {
                  paste0("Effect of ", cov_name, " on ", spec_pk[[param_name]]$label)
                } else {
                  paste0("Effect of ", cov_name, " on ", param_name)
                }
              }
            }
          },
          parameter_names %in% names(spec_pk) ~ {
            label_value <- spec_pk[[parameter_names]]$label
            if (!is.null(label_value)) label_value else NA_character_
          },
          TRUE ~ NA_character_
        ),
        parameter_names = dplyr::case_when(
          grepl("^beta_", parameter_names) ~ {
            # Same parsing logic for short name
            parts <- strsplit(parameter_names, "_")[[1]]

            if (length(parts) < 3) {
              parameter_names
            } else {
              last_part <- parts[length(parts)]
              is_level <- !is.na(suppressWarnings(as.numeric(last_part)))

              if (is_level && length(parts) >= 4) {
                # Has level
                cov_name <- parts[2]
                param_parts <- parts[3:(length(parts)-1)]
                param_name <- paste(param_parts, collapse = "_")
                level <- last_part

                if (param_name %in% names(spec_pk)) {
                  paste0(cov_name, " level ", level, "~", spec_pk[[param_name]]$short)
                } else {
                  paste0(cov_name, " level ", level, "~", param_name)
                }
              } else {
                # No level
                cov_name <- parts[2]
                param_parts <- parts[3:length(parts)]
                param_name <- paste(param_parts, collapse = "_")

                if (param_name %in% names(spec_pk)) {
                  paste0(cov_name, "~", spec_pk[[param_name]]$short)
                } else {
                  paste0(cov_name, "~", param_name)
                }
              }
            }
          },
          parameter_names %in% names(spec_pk) ~ {
            if (!is.null(spec_pk[[parameter_names]]) && !is.null(spec_pk[[parameter_names]]$unit)) {
              paste0(spec_pk[[parameter_names]]$short, " (", spec_pk[[parameter_names]]$unit, ")")
            } else if (!is.null(spec_pk[[parameter_names]]) && !is.null(spec_pk[[parameter_names]]$short)) {
              spec_pk[[parameter_names]]$short
            } else {
              parameter_names
            }
          },
          TRUE ~ parameter_names
        )
      ) %>%
      dplyr::relocate(label, .after = parameter_names)
  } else {
    # Default labels if no spec provided
    param_est <- param_est %>%
      dplyr::mutate(
        comment = dplyr::case_when(
          grepl("^beta_", parameter_names) ~ "Parameter-Covariate relationships",
          grepl("^THETA", parameter_names) ~ "Typical parameters",
          grepl("^OMEGA", parameter_names) ~ "Inter-individual variability",
          grepl("^SIGMA", parameter_names) ~ "Residual variability",
          parameter_names == "OFV" ~ NA_character_,
          TRUE ~ NA_character_
        ),
        label = parameter_names
      )
  }

  # Final formatting
  param_est <- param_est %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      comment = factor(comment, levels = c(
        "Typical parameters",
        "Inter-individual variability",
        "Correlation of random effects",
        "Parameter-Covariate relationships",
        "Residual variability",
        NA
      ))
    ) %>%
    dplyr::arrange(comment) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

  # Format output based on number of models
  if (count_model > 1) {
    # Multiple model comparison format
    param_est <- param_est %>%
      dplyr::mutate(
        model = dplyr::case_when(
          fixed == TRUE ~ paste(Parameter, "FIX"),
          is.na(SHRINKAGE) & !is.na(Parameter) & !is.na(RSE) ~
            paste0(Parameter, " (", RSE, "%)"),
          !is.na(SHRINKAGE) & !is.na(Parameter) & !is.na(RSE) ~
            paste0(Parameter, " (", RSE, "%)", " [", SHRINKAGE, "%]"),
          is.na(SHRINKAGE) & !is.na(Parameter) & is.na(RSE) & is.na(fixed) ~
            as.character(Parameter),

          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::select("parameter_names", "label", "model", "comment") %>%
      dplyr::rename(!!as.character(model_number) := model)
  } else {
    # Single model format
    param_est <- param_est %>%
      dplyr::mutate(
        Parameter = dplyr::case_when(
          fixed == TRUE ~ paste(Parameter, "FIX"),
          fixed == FALSE ~ as.character(Parameter),
          TRUE ~ as.character(Parameter)
        )
      ) %>%
      dplyr::select(-fixed)
  }

  return(param_est)
}


# FINAL CORRECTED model_report WITH COMPLETE ERROR HANDLING
# Replace the entire model_report function in model-output-tables.R

#' Generate Parameter Table Report for Multiple Models
#'
#' @param model_names Character vector of model names
#' @param shrinkage Type of shrinkage to report ("etasd", "etavr", "ebvsd", "ebvvr")
#' @param models_folder Path to models folder
#' @param spec_pk Optional yspec object for parameter formatting
#' @return flextable object with formatted parameter table
#' @export
model_report <- function(model_names,
                         shrinkage = "etasd",
                         models_folder = "models",
                         spec_pk = NULL) {

  if (length(model_names) == 0) {
    stop("At least one model name must be provided")
  }

  count_model <- length(model_names)

  cat(sprintf("\n📊 Generating report for %d model(s)...\n", count_model))

  # Try to generate parameter table for each model with error handling
  param_results <- list()
  failed_models <- character(0)
  successful_models <- character(0)

  for (model_name in model_names) {
    cat(sprintf("  Processing %s... ", model_name))

    result <- tryCatch({
      param_table <- get_param2(
        model_number = model_name,
        count_model = count_model,
        shrinkage = shrinkage,
        models_folder = models_folder,
        spec_pk = spec_pk
      )
      cat("✓\n")
      list(success = TRUE, data = param_table, model = model_name)
    }, error = function(e) {
      cat("✗\n")
      cat(sprintf("    ⚠️  Error: %s\n", conditionMessage(e)))
      list(success = FALSE, error = conditionMessage(e), model = model_name)
    }, warning = function(w) {
      cat("⚠️\n")
      cat(sprintf("    Warning: %s\n", conditionMessage(w)))
      list(success = FALSE, error = paste("Warning:", conditionMessage(w)), model = model_name)
    })

    if (result$success) {
      param_results[[model_name]] <- result$data
      successful_models <- c(successful_models, model_name)
    } else {
      failed_models <- c(failed_models, model_name)
    }
  }

  # Check if we have any successful models
  if (length(successful_models) == 0) {
    stop("\nAll models failed to process. Cannot generate report.")
  }

  # Print summary
  cat("\n")
  if (length(failed_models) > 0) {
    cat("⚠️  WARNING: The following models failed and were excluded:\n")
    for (failed_model in failed_models) {
      cat(sprintf("  • %s\n", failed_model))
    }
    cat("\n")
  }

  cat(sprintf("✅ Successfully processed %d/%d model(s)\n\n",
              length(successful_models), count_model))

  # Update count to reflect successful models only
  count_model <- length(successful_models)

  # Combine successful model tables
  param_est <- param_results %>%
    purrr::reduce(dplyr::full_join, by = c("parameter_names", "label", "comment")) %>%
    dplyr::arrange(comment)

  # Create grouped table with section headers
  grouped_table <- param_est %>%
    dplyr::group_by(comment) %>%
    dplyr::summarise(
      parameter_names = "",
      label = "",
      comment_info = dplyr::first(comment),
      .groups = "drop"
    ) %>%
    dplyr::mutate(comment_info = as.character(comment)) %>%
    dplyr::bind_rows(param_est, .id = "group") %>%
    dplyr::arrange(comment, group) %>%
    dplyr::mutate(
      parameter_names = ifelse(!is.na(comment_info), comment_info, parameter_names),
      label = ifelse(!is.na(comment_info), "", label),
      comment = ifelse(!is.na(comment_info), "", comment)
    )

  # Rename columns based on single vs multiple models
  if (count_model == 1) {
    # For single model, check which columns actually exist before renaming
    col_names <- names(grouped_table)

    # Build rename list dynamically based on existing columns
    rename_list <- list()
    if ("Parameter" %in% col_names) rename_list[["Estimate"]] <- "Parameter"
    if ("RSE" %in% col_names) rename_list[["RSE (%)"]] <- "RSE"
    if ("SHRINKAGE" %in% col_names) rename_list[["Shrinkage (%)"]] <- "SHRINKAGE"

    # Only rename if we have columns to rename
    if (length(rename_list) > 0) {
      grouped_table <- grouped_table %>%
        dplyr::rename(!!!rename_list)
    }

    # Remove helper columns
    grouped_table <- grouped_table %>%
      dplyr::select(-dplyr::any_of(c("comment_info", "group", "comment")))

  } else {
    # Multiple models
    keep_cols <- c("group", "comment", "parameter_names", "label", "comment_info")
    new_names <- names(grouped_table)
    new_names[!new_names %in% keep_cols] <- paste0(
      "Model ",
      new_names[!new_names %in% keep_cols],
      " Estimate (RSE%) [Shrinkage%]"
    )
    colnames(grouped_table) <- new_names
    grouped_table <- grouped_table %>%
      dplyr::select(-dplyr::any_of(c("comment_info", "group", "comment")))
  }

  # Create and format flextable
  flextable_object <- grouped_table %>%
    flextable::flextable() %>%
    flextable::align(j = c(-1), align = "center", part = "all") %>%
    theme_pps_table() %>%
    flextable::bold(
      i = which(grouped_table$label == ""),
      bold = TRUE,
      part = "body"
    ) %>%
    flextable::merge_v(j = 1) %>%
    flextable::autofit()

  # Add footer note if models were skipped
  if (length(failed_models) > 0) {
    footer_text <- sprintf(
      "Note: %d model(s) excluded due to errors: %s",
      length(failed_models),
      paste(failed_models, collapse = ", ")
    )
    flextable_object <- flextable::add_footer_lines(
      flextable_object,
      values = footer_text
    )
  }

  return(flextable_object)
}
