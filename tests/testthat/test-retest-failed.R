# Covariates excluded after a failed model and failed retry: the exclusion
# takes effect, and retest_failed decides whether they are tested again.

excl_state <- function(retest = NULL) {
  db <- data.frame(
    model_name = c("run1", "run2", "run2001", "run3"),
    parent_model = c(NA, "run1", "run1", "run1"), step_number = c(NA, 1L, 1L, 1L),
    covariate_tested = c(NA, "beta_WT_CL", "beta_WT_CL", "beta_AGE_V"),
    status = c("completed", "failed", "failed", "completed"),
    excluded_from_step = c(FALSE, TRUE, TRUE, FALSE),
    phase = c("base", "forward_selection", "retry", "forward_selection"),
    stringsAsFactors = FALSE)
  list(tags = list(beta_WT_CL = "WT_CL", beta_AGE_V = "AGE_V", beta_SEX_CL = "SEX_CL"),
       search_database = db,
       search_config = if (is.null(retest)) list() else list(retest_failed = retest))
}

test_that("an excluded covariate is left out when exclusions apply", {
  # REC-04: exclusion rows hold the tag name (beta_WT_CL) and were compared
  # with tag values (WT_CL), so nothing was ever excluded.
  local_mocked_bindings(get_model_covariates_from_db = function(...) character(0))
  st <- excl_state()
  invisible(capture.output(
    out <- get_remaining_covariates(st, "run1", include_excluded = FALSE)))
  expect_setequal(out, c("beta_AGE_V", "beta_SEX_CL"))
  invisible(capture.output(
    out <- get_remaining_covariates(st, "run1", include_excluded = TRUE)))
  expect_setequal(out, c("beta_WT_CL", "beta_AGE_V", "beta_SEX_CL"))
})

test_that("retest_failed reads TRUE unless explicitly FALSE", {
  expect_true(.retest_failed(excl_state()))
  expect_true(.retest_failed(excl_state(TRUE)))
  expect_false(.retest_failed(excl_state(FALSE)))
  expect_true(.retest_failed(excl_state(NA)))
  expect_error(.check_retest_failed(NA), "TRUE or FALSE")
  expect_error(.check_retest_failed("no"), "TRUE or FALSE")
})

test_that("continue_search rejects a malformed retest_failed before anything else", {
  expect_error(continue_search(excl_state(), retest_failed = "yes"), "TRUE or FALSE")
})

test_that("standard SCM passes retest_failed to the step it builds", {
  seen <- NULL
  local_mocked_bindings(
    get_remaining_covariates = function(search_state, base_model_id, include_excluded = TRUE) {
      seen <<- c(seen, include_excluded); character(0)
    })
  for (flag in c(TRUE, FALSE)) {
    st <- excl_state(flag)
    st$base_model <- "run1"
    st$search_config$forward_p_value <- 0.05
    invisible(capture.output(try(run_stepwise_covariate_modeling(st, "run1", auto_submit = FALSE),
                                 silent = TRUE)))
  }
  expect_equal(seen, c(TRUE, FALSE))
})


test_that("redemption starts with covariates not yet tested on the best model", {
  # SCM-01: forward ends with step k (built on the best model, nothing
  # significant). Redemption re-tested step k's covariates on that same model.
  # A covariate whose model on the best model failed is tested again.
  local_mocked_bindings(get_model_covariates_from_db = function(search_state, model_name)
    if (model_name == "run3") "AGE_V" else character(0))
  db <- data.frame(
    model_name = c("run1", "run2", "run3", "run4", "run5", "run6"),
    parent_model = c(NA, "run1", "run1", "run3", "run3", "run3"),
    covariate_tested = c(NA, "beta_WT_CL", "beta_AGE_V", "beta_WT_CL", "beta_SEX_CL", "beta_AGE_V"),
    status = c("completed", "completed", "completed", "completed", "failed", "completed"),
    phase = c("base", rep("forward_selection", 4), "covariate_removal"),
    action = c(NA, rep("add_single_covariate", 4), "remove_single_covariate"),
    step_number = c(NA, 1L, 1L, 2L, 2L, 3L), stringsAsFactors = FALSE)
  st <- list(tags = list(beta_WT_CL = "WT_CL", beta_AGE_V = "AGE_V",
                         beta_SEX_CL = "SEX_CL", beta_CRCL_CL = "CRCL_CL"),
             search_database = db)
  # AGE_V is in run3; WT_CL has a completed model on run3; SEX_CL's failed.
  expect_setequal(.redemption_candidates(st, "run3"), c("beta_SEX_CL", "beta_CRCL_CL"))
  # From the base model, WT and AGE were completed on run1.
  expect_setequal(.redemption_candidates(st, "run1"), c("beta_SEX_CL", "beta_CRCL_CL"))
})
