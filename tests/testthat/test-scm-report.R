# generate_scm_report() must report the decision the search made, not a second
# opinion on the same numbers. Every verdict, both winner rules and the
# final-model rule come from the shared evaluator; these guard that.

report_state <- function() {
  list(
    base_model = "run2",
    models_folder = tempdir(),   # no .yaml there: covariates display as "Unknown"
    search_config = list(forward_p_value = 0.05, backward_p_value = 0.001,
                         max_rse_threshold = 50, require_cov_step = TRUE),
    covariate_search = data.frame(
      COVARIATE = c("AGE", "SMK"), STATUS = "con", FORMULA = "power",
      LEVELS = NA, INIT = NA, stringsAsFactors = FALSE),
    # The real run: run17 has the best fit of all three and 7429% RSE.
    search_database = data.frame(
      model_name       = c("run2", "run15", "run16", "run17"),
      parent_model     = c(NA, "run2", "run2", "run15"),
      original_model   = NA_character_,
      covariate_tested = c(NA, "beta_AGE_CL", "beta_SMK_V1", "beta_SMK_V1"),
      step_number      = c(0L, 2L, 2L, 3L),
      phase            = c("base", rep("forward_selection", 3)),
      action           = c("base_model", rep("add_covariate", 3)),
      step_description = c("Base", "Add AGE", "Add SMK", "Add SMK"),
      status           = "completed",
      ofv              = c(144062.43, 144047.85, 144042.48, 144028.06),
      delta_ofv        = c(NA, 14.58, 19.96, 19.79),
      rse_max          = c(20, 26.8, 897.7, 7429.3),
      stringsAsFactors = FALSE))
}

report_lines <- function(st) {
  out <- file.path(tempdir(), "scm_report_test.txt")
  suppressWarnings(suppressMessages(
    capture.output(generate_scm_report(st, output_file = out, print_console = TRUE))
  ))
}


test_that("the winner is the best model meeting BOTH criteria, not the best fit", {
  lines <- report_lines(report_state())

  # run16 has the larger ΔOFV (19.96 vs 14.58) but 897.7% RSE.
  expect_true(any(grepl("Winner: run15", lines)))
  expect_false(any(grepl("Winner: run16", lines)))
})


test_that("the final model is the one the steps arrived at, not the last one run", {
  lines <- report_lines(report_state())

  # run17 is step 3 and holds the lowest OFV in the whole search, so both
  # "highest step number" and "best fit" would name it. It fails on RSE.
  expect_true(any(grepl("Final Model: run15", lines)))
  expect_false(any(grepl("Final Model: run17", lines)))
})


test_that("a step where nothing qualifies holds the model rather than advancing", {
  lines <- report_lines(report_state())

  expect_true(any(grepl("No model meets the criteria", lines)))
  # ...and says why, rather than reporting an insufficient OFV.
  expect_true(any(grepl("blocked by RSE", lines)))
})


test_that("backward steps are judged by the backward rule", {
  st <- report_state()
  st$search_database$phase[2:4]  <- "backward_elimination"
  st$search_database$action[2:4] <- "remove_covariate"
  st$search_database$rse_max     <- c(20, 26.8, 26.8, 26.8)
  # ΔOFV 14.58 / 19.96 / 19.79 all exceed the backward threshold of 10.83, so
  # every removal costs too much and none is accepted.
  lines <- report_lines(st)

  expect_true(any(grepl("BACKWARD ELIMINATION", lines)))
  expect_true(any(grepl("No removals meet the criteria", lines)))
  expect_true(any(grepl("Final Model: run2", lines)))
})


test_that("a backward step with no parent model does not error or borrow an OFV", {
  # base_ofv is assigned only when the step names a parent but is read below
  # regardless; without a per-step reset this errored on the first step and
  # reported the previous step's OFV on later ones.
  st <- report_state()
  st$search_database$parent_model <- NA_character_
  st$search_database$phase[2:4]   <- "backward_elimination"
  st$search_database$action[2:4]  <- "remove_covariate"

  lines <- report_lines(st)
  expect_true(any(grepl("Base model OFV not available", lines)))
  # No step may print a base OFV it does not have. Matching the value alone
  # would be wrong: run2's OFV legitimately appears as the final model's.
  expect_false(any(grepl("Base model .+ OFV: [0-9]", lines)))
  expect_false(any(grepl("Base model: .+ \\(OFV:", lines)))
})


test_that("the summary counts are reported even when no final model can be named", {
  st <- report_state()
  st$base_model <- NULL
  st$search_database$step_number <- rep(0L, 4)   # no steps at all
  st$search_database$parent_model <- NA_character_

  lines <- report_lines(st)
  expect_true(any(grepl("Final Model: not determined", lines)))
  expect_true(any(grepl("Total Models Created: 4", lines)))
})


test_that("the report survives a database missing optional columns", {
  for (drop in c("rse_max", "action", "phase", "covariate_tested")) {
    st <- report_state()
    st$search_database[[drop]] <- NULL
    expect_error(report_lines(st), NA, info = paste("dropped column:", drop))
  }
})
