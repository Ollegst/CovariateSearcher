# Acceptance criteria: one evaluator, every phase and caller.
#
# The regression these guard against: a model failing the RSE limit became the
# starting model for backward elimination because the post-redemption pick
# ranked on OFV alone. Its OFV was the best in the search, so "best fit" and
# "acceptable" disagreed - and only the OFV half was being asked.

# A covariate table covering the shapes calculate_covariate_df() distinguishes:
# a plain continuous covariate (df 1), a 3-level categorical (df 2), and a
# fully-FIX covariate that adds no estimated parameter (df 0).
criteria_covariates <- function() {
  data.frame(
    COVARIATE = c("AGE", "SMK", "RACE", "PPI"),
    STATUS    = c("con", "con", "cat", "con"),
    FORMULA   = c("power", "power", "linear", "power"),
    LEVELS    = c(NA, NA, "0;1;2", NA),
    INIT      = c(NA, NA, NA, "0.1 FIX"),
    stringsAsFactors = FALSE
  )
}

# The real step that exposed the bug. run17 has the LOWEST OFV of the three and
# clears the ΔOFV threshold, so any rule that ranks on fit alone picks it - but
# its RSE is 7429%.
criteria_state <- function(rse_threshold = 50) {
  list(
    search_config = list(
      forward_p_value   = 0.05,
      backward_p_value  = 0.001,
      max_rse_threshold = rse_threshold
    ),
    covariate_search = criteria_covariates(),
    search_database = data.frame(
      model_name       = c("run15", "run16", "run17"),
      parent_model     = c("run2", "run2", "run15"),
      covariate_tested = c("beta_AGE_CL", "beta_SMK_V1", "beta_SMK_V1"),
      step_number      = c(2L, 2L, 3L),
      status           = rep("completed", 3),
      ofv              = c(144047.85, 144042.48, 144028.06),
      delta_ofv        = c(14.58, 19.96, 19.79),
      rse_max          = c(26.8, 897.7, 7429.3),
      stringsAsFactors = FALSE
    )
  )
}


test_that("the best-fitting model does not win when it fails the RSE limit", {
  st <- criteria_state()

  # Fit alone would choose run17 - that is exactly the rule that broke.
  expect_equal(
    st$search_database$model_name[which.min(st$search_database$ofv)],
    "run17"
  )

  pick <- .best_acceptable_model(st, c("run15", "run16", "run17"))

  expect_equal(pick$model, "run15")
  # Both rejects had the OFV to qualify and were dropped purely on precision,
  # so both must be reported rather than silently absent.
  expect_setequal(pick$rejected$model_name, c("run16", "run17"))
})


test_that("raising the RSE limit past a model's RSE lets it win again", {
  # Guards against the filter being hardcoded rather than threshold-driven.
  pick <- .best_acceptable_model(criteria_state(rse_threshold = 10000),
                                 c("run15", "run16", "run17"))
  expect_equal(pick$model, "run17")
  expect_equal(nrow(pick$rejected), 0)
})


test_that("an explicit rse_threshold overrides the configured one", {
  st <- criteria_state(rse_threshold = 10000)
  pick <- .best_acceptable_model(st, c("run15", "run16", "run17"),
                                 rse_threshold = 50)
  expect_equal(pick$model, "run15")
})


test_that("get_significant_models_from_step applies the RSE limit", {
  st <- criteria_state()
  sig <- get_significant_models_from_step(st, step_number = 2, p_value = 0.05)

  # run16 has the larger ΔOFV (19.96 vs 14.58) but 897.7% RSE.
  expect_equal(sig, "run15")
})


test_that("forward and backward invert the OFV test but share the RSE test", {
  st <- criteria_state()

  fwd <- .evaluate_model_criteria(st, "run15", phase = "forward",
                                  p_value = 0.05, rse_threshold = 50)
  bwd <- .evaluate_model_criteria(st, "run15", phase = "backward",
                                  p_value = 0.05, rse_threshold = 50)

  # Same ΔOFV, same threshold, opposite verdicts: forward asks whether the
  # covariate buys enough, backward whether the removal costs little enough.
  expect_equal(fwd$ofv_threshold, bwd$ofv_threshold)
  expect_true(fwd$meets_ofv)
  expect_false(bwd$meets_ofv)

  # The RSE test is identical in both phases.
  expect_equal(fwd$meets_rse, bwd$meets_rse)
})


test_that("degrees of freedom come from the covariate, not a fixed df=1", {
  st <- criteria_state()
  st$search_database$covariate_tested <- c("beta_AGE_CL", "beta_RACE_CL",
                                           "beta_PPI_CL")

  ev <- .evaluate_model_criteria(st, c("run15", "run16", "run17"),
                                 phase = "forward", p_value = 0.05)

  # continuous -> 1; 3-level categorical -> 2; fully-FIX -> 0 (threshold 0, so
  # the covariate is judged by direct OFV comparison).
  expect_equal(ev$covariate_df, c(1L, 2L, 0L))
  expect_equal(ev$ofv_threshold[3], 0)
  expect_equal(ev$ofv_threshold[1], qchisq(0.95, df = 1))
  expect_equal(ev$ofv_threshold[2], qchisq(0.95, df = 2))
})


test_that("a model that did not complete is never acceptable", {
  st <- criteria_state()
  st$search_database$status <- c("failed", "completed", "completed")
  st$search_database$rse_max <- c(26.8, 12.0, 15.0)

  ev <- .evaluate_model_criteria(st, c("run15", "run16", "run17"),
                                 phase = "forward", p_value = 0.05)

  expect_false(ev$meets_threshold[1])
  expect_true(all(ev$meets_threshold[2:3]))

  # ...and it cannot be picked even with the best fit.
  st$search_database$ofv[1] <- 1
  expect_equal(
    .best_acceptable_model(st, c("run15", "run16", "run17"))$model,
    "run17"
  )
})


test_that("an unreadable RSE is never presented as a numeric one", {
  # Whether it rejects is decided by require_cov_step (tested below). Either
  # way the reason must name the absence: "0.0%" or "NA% > 50%" reads as a
  # measured value, and a model whose RSE could not be read must not look like
  # a model with a perfect one.
  st <- criteria_state()
  st$search_database$rse_max <- c(26.8, NA_real_, 7429.3)

  for (require_cov in c(TRUE, FALSE)) {
    st$search_config$require_cov_step <- require_cov
    ev <- .evaluate_model_criteria(st, "run16", phase = "forward", p_value = 0.05)

    expect_true(is.na(ev$rse_max))
    expect_false(grepl("0.0%", ev$note, fixed = TRUE))
    expect_false(grepl("NA%", ev$note, fixed = TRUE))
  }
})


test_that("models absent from the database are dropped, not invented", {
  st <- criteria_state()
  ev <- .evaluate_model_criteria(st, c("run15", "nonexistent"),
                                 phase = "forward", p_value = 0.05)

  expect_equal(ev$model_name, "run15")
  expect_equal(nrow(.evaluate_model_criteria(st, character(0),
                                             phase = "forward")), 0)
})


# --- the delta_ofv sign is a property of the model, not of the caller ------

test_that("the ΔOFV sign follows the model's action, and a retry follows its original", {
  st <- list(search_database = data.frame(
    model_name     = c("add1", "rem1", "rem1001", "manual1"),
    action         = c("add_covariate", "remove_covariate", "retry",
                       "manual_modification"),
    original_model = c(NA, NA, "rem1", NA),
    stringsAsFactors = FALSE))

  # Identical OFVs; only the model's own action decides the subtraction order.
  # An addition stores reference - model (positive = better); a removal stores
  # model - reference (positive = the removal made the fit worse).
  expect_equal(.signed_delta_ofv(st, "add1", 110, 100), -10)
  expect_equal(.signed_delta_ofv(st, "rem1", 110, 100), 10)

  # The case the sign-writers could not previously get right on their own.
  expect_equal(.signed_delta_ofv(st, "rem1001", 110, 100), 10)

  # Anything that names no direction is treated as an addition, matching
  # .is_removal_model()'s contract.
  expect_equal(.signed_delta_ofv(st, "manual1", 110, 100), -10)
  expect_equal(.signed_delta_ofv(st, "not_in_db", 110, 100), -10)

  # Missing or absent OFVs never produce a number.
  expect_true(is.na(.signed_delta_ofv(st, "add1", NA_real_, 100)))
  expect_true(is.na(.signed_delta_ofv(st, "add1", 110, NA_real_)))
  expect_true(is.na(.signed_delta_ofv(st, "add1", 110, numeric(0))))

  # An explicit direction overrides the lookup. This is what a caller that also
  # fixes the acceptance test's direction must use, so the two cannot disagree.
  expect_equal(.signed_delta_ofv(st, "add1", 110, 100, direction = "backward"), 10)
  expect_equal(.signed_delta_ofv(st, "rem1", 110, 100, direction = "forward"), -10)
})


# --- forward pools must not contain backward rows -------------------------

test_that("backward rows are identified by phase or by action", {
  st <- list(search_database = data.frame(
    model_name = c("run3", "run15", "base"),
    phase = c("backward_elimination", "forward_selection", "base"),
    action = c("remove_covariate", "add_covariate", "base_model"),
    original_model = NA_character_,
    stringsAsFactors = FALSE
  ))
  expect_equal(.scm_backward_rows(st), c(TRUE, FALSE, FALSE))

  # The other spelling the package writes for a removal step.
  st$search_database$phase <- c("covariate_removal", "forward_selection", "base")
  expect_equal(.scm_backward_rows(st), c(TRUE, FALSE, FALSE))

  # phase alone is not the test: action still identifies the removal.
  st$search_database$phase <- NULL
  expect_equal(.scm_backward_rows(st), c(TRUE, FALSE, FALSE))

  # ...and neither is action alone.
  st$search_database$action <- NULL
  st$search_database$phase <- c("backward_elimination", "forward_selection", "base")
  expect_equal(.scm_backward_rows(st), c(TRUE, FALSE, FALSE))

  expect_equal(.scm_backward_rows(list(search_database = NULL)), logical(0))
})


test_that("a retry of a backward removal is caught, though its phase says 'retry'", {
  # create_retry_model() writes phase = "retry" but keeps the original's
  # step_number, and update_model_status_from_files() later fills delta_ofv
  # using .is_removal_model(), which resolves the retry to what it retries - so
  # the row ends up with a BACKWARD-signed delta_ofv and a non-backward phase.
  # Matching on phase alone would let it into the forward pools.
  st <- criteria_state()
  st$search_database <- data.frame(
    model_name       = c("run6", "run6001", "run15"),
    parent_model     = c("run2", "run2", "run2"),
    original_model   = c(NA, "run6", NA),
    covariate_tested = c("beta_SMK_V1", "beta_SMK_V1", "beta_AGE_CL"),
    step_number      = c(1L, 1L, 2L),
    phase            = c("backward_elimination", "retry", "forward_selection"),
    action           = c("remove_covariate", "retry", "add_covariate"),
    status           = rep("completed", 3),
    ofv              = c(144132.52, 143000.00, 144047.85),
    delta_ofv        = c(70.09, 70.09, 14.58),
    rse_max          = c(32.6, 32.6, 26.8),
    stringsAsFactors = FALSE
  )

  expect_equal(.scm_backward_rows(st), c(TRUE, TRUE, FALSE))

  # The retry holds the lowest OFV, so without the action-aware test it would
  # win the forward pick outright.
  db <- st$search_database
  pool <- db[db$status == "completed" &
               !is.na(db$ofv) &
               !is.na(db$delta_ofv) &
               !.scm_backward_rows(st) &
               db$step_number > 0, ]

  expect_equal(pool$model_name, "run15")
  expect_equal(.best_acceptable_model(st, db$model_name)$model, "run6001")
  expect_equal(.best_acceptable_model(st, pool$model_name)$model, "run15")
})


test_that("phase identifies a removal when the action column is absent", {
  # `action` is not in update_model_status_from_files()'s self-heal list, so a
  # database can carry none. Without the phase fallback the sign-writer reads
  # such a row as an addition while .scm_backward_rows() reads it as backward,
  # and a removal that COST 50 OFV points gets stored as -50 and then passes the
  # backward test - recommending removal of a covariate that must be kept.
  st <- list(search_database = data.frame(
    model_name     = c("run2", "run20"),
    parent_model   = c(NA, "run2"),
    phase          = c("base", "backward_elimination"),
    original_model = NA_character_,
    stringsAsFactors = FALSE))

  expect_equal(.is_removal_model(st, c("run2", "run20")), c(FALSE, TRUE))
  # model - reference: the removal made the fit worse by 50.
  expect_equal(.signed_delta_ofv(st, "run20", 144050, 144000), 50)

  # An action that IS recorded wins over a contradicting phase: a contradiction
  # is reported by the sign-writers, not silently reinterpreted here.
  st$search_database$action <- c("base_model", "add_covariate")
  expect_equal(.is_removal_model(st, "run20"), FALSE)
})


test_that("a costly removal is not mistaken for a large forward improvement", {
  # A search that starts with backward elimination leaves rows whose delta_ofv
  # is stored as model - base: run6's +70.09 means removing SEX_CL made the fit
  # much worse. Read with the forward rule it looks like a 70-point gain.
  st <- criteria_state()
  st$search_database <- data.frame(
    model_name       = c("run6", "run15"),
    parent_model     = c("run2", "run2"),
    covariate_tested = c("beta_SMK_V1", "beta_AGE_CL"),
    step_number      = c(1L, 2L),
    phase            = c("backward_elimination", "forward_selection"),
    status           = rep("completed", 2),
    ofv              = c(144132.52, 144047.85),
    delta_ofv        = c(70.09, 14.58),
    rse_max          = c(32.6, 26.8),
    stringsAsFactors = FALSE
  )

  # The forward rule does accept it on its own terms - which is the hazard.
  ev <- .evaluate_model_criteria(st, "run6", phase = "forward", p_value = 0.05)
  expect_true(ev$meets_threshold)

  # So the pool must exclude it rather than rely on it losing on OFV.
  expect_equal(.scm_backward_rows(st), c(TRUE, FALSE))
})


test_that("a backward row cannot win the forward pick even with the best OFV", {
  # Same rows, but the removal now holds the lowest OFV - the only way a
  # backward row could ever have won the which.min(ofv) pick.
  st <- criteria_state()
  st$search_database <- data.frame(
    model_name       = c("run6", "run15"),
    parent_model     = c("run2", "run2"),
    covariate_tested = c("beta_SMK_V1", "beta_AGE_CL"),
    step_number      = c(1L, 2L),
    phase            = c("backward_elimination", "forward_selection"),
    status           = rep("completed", 2),
    ofv              = c(143000.00, 144047.85),
    delta_ofv        = c(70.09, 14.58),
    rse_max          = c(32.6, 26.8),
    stringsAsFactors = FALSE
  )

  db <- st$search_database
  forward_pool <- db[db$status == "completed" &
                       !is.na(db$ofv) &
                       !is.na(db$delta_ofv) &
                       !.scm_backward_rows(st) &
                       db$step_number > 0, ]

  expect_equal(forward_pool$model_name, "run15")
  expect_equal(.best_acceptable_model(st, forward_pool$model_name)$model, "run15")

  # Without the phase filter the removal wins on fit alone.
  expect_equal(.best_acceptable_model(st, db$model_name)$model, "run6")
})


test_that("every forward candidate pool in the file carries the phase filter", {
  # Four pools feed .best_acceptable_model inside run_scm_selective_forward.
  # Guarding some but not all is the failure mode this test exists to catch:
  # each unguarded one is the same misread in a different place.
  # Reads the source, so it only runs against a source tree. Under R CMD check
  # the tests execute from <pkg>.Rcheck/tests/testthat, where the package is
  # installed rather than sourced and R/*.R does not exist - skip there instead
  # of erroring. A plain relative path, not test_path(), which throws when it
  # cannot locate the test directory rather than returning a missing path.
  src_file <- file.path("..", "..", "R", "scm-selective-forward.R")
  skip_if_not(file.exists(src_file), "package sources not available")
  src <- readLines(src_file, warn = FALSE)

  pools <- c("all_previous_models <- ", "all_main_models <- ",
             "all_models_so_far <- ", "all_completed_candidates <- ")

  for (pool in pools) {
    start <- grep(pool, src, fixed = TRUE)
    expect_length(start, 1)
    window <- src[start:min(start + 12L, length(src))]
    expect_true(
      any(grepl(".scm_backward_rows", window, fixed = TRUE)),
      info = paste("pool has no backward-row filter:", trimws(pool))
    )
  }
})


# --- the results table reports the decision, not a second opinion ---------

test_that("the results table rejects the high-RSE models the engine rejected", {
  st <- criteria_state()
  # Nothing is written here; the folder only has to exist so the covariate
  # display lookup can miss and fall back to "Unknown".
  st$models_folder <- tempdir()
  st$search_database$phase <- "forward_selection"
  st$search_database$action <- "add_covariate"
  st$search_database$step_description <- paste("Add",
                                               st$search_database$covariate_tested)

  tbl <- create_scm_results_table(st)
  sel <- stats::setNames(tbl$Selected, tbl$Model)

  expect_equal(sel[["run15"]], "BEST")
  expect_equal(sel[["run16"]], "NO")
  expect_equal(sel[["run17"]], "NO")

  # ...and says why, rather than reporting an insufficient OFV.
  expect_match(tbl$Comment[tbl$Model == "run17"], "RSE too high")
})


test_that("the step winner is judged against each model's own df threshold", {
  # A 3-level categorical needs ΔOFV > 5.99 (df=2); a continuous needs > 3.84
  # (df=1). Here only the continuous qualifies, so it is the step winner -
  # applying its threshold to the whole step would let the categorical's larger
  # but insufficient ΔOFV set the bar and demote it to "YES".
  st <- criteria_state()
  # Nothing is written here; the folder only has to exist so the covariate
  # display lookup can miss and fall back to "Unknown".
  st$models_folder <- tempdir()
  st$search_database <- data.frame(
    model_name       = c("run40", "run41"),
    parent_model     = c("base", "base"),
    covariate_tested = c("beta_AGE_CL", "beta_RACE_CL"),
    step_number      = c(2L, 2L),
    status           = rep("completed", 2),
    phase            = rep("forward_selection", 2),
    action           = rep("add_covariate", 2),
    step_description = c("Add AGE", "Add RACE"),
    ofv              = c(100.0, 99.5),
    delta_ofv        = c(4.5, 5.0),
    rse_max          = c(10, 10),
    stringsAsFactors = FALSE
  )

  tbl <- create_scm_results_table(st)
  sel <- stats::setNames(tbl$Selected, tbl$Model)

  expect_equal(sel[["run40"]], "BEST")
  expect_equal(sel[["run41"]], "NO")
})


# --- backward elimination ------------------------------------------------

# Removals off a base at OFV 144000: delta_ofv is stored as model - base, so a
# positive value means the removal made the fit worse.
backward_state <- function() {
  st <- criteria_state()
  st$search_database <- data.frame(
    model_name       = c("base", "run20", "run21", "run22"),
    parent_model     = c(NA, "base", "base", "base"),
    original_model   = NA_character_,
    covariate_tested = c(NA, "beta_AGE_CL", "beta_SMK_V1", "beta_RACE_CL"),
    step_number      = c(1L, 2L, 2L, 2L),
    phase            = c("base", rep("backward_elimination", 3)),
    action           = c("base_model", rep("remove_covariate", 3)),
    status           = rep("completed", 4),
    ofv              = c(144000, 144002.0, 144050.0, 144003.0),
    delta_ofv        = c(NA, 2.0, 50.0, 3.0),
    rse_max          = c(10, 12.0, 15.0, 900.0),
    stringsAsFactors = FALSE
  )
  st
}


test_that("backward accepts cheap removals and keeps expensive ones", {
  st <- backward_state()
  ev <- evaluate_removal_impacts(
    search_state     = st,
    base_model       = "base",
    removal_models   = list(AGE = "run20", SMK = "run21"),
    completed_models = c("run20", "run21"),
    backward_p_value = 0.001
  )

  # threshold at p=0.001, df=1 is 10.83: 2.0 is below it, 50.0 is not.
  imp <- ev$removal_impacts
  expect_true(imp$meets_threshold[imp$model_name == "run20"])
  expect_false(imp$meets_threshold[imp$model_name == "run21"])
  expect_equal(ev$covariate_to_remove, "AGE")
})


test_that("a high-RSE reduced model blocks its own removal", {
  st <- backward_state()
  ev <- evaluate_removal_impacts(
    search_state     = st,
    base_model       = "base",
    removal_models   = list(RACE = "run22"),
    completed_models = "run22",
    backward_p_value = 0.001
  )

  imp <- ev$removal_impacts
  # ΔOFV of 3.0 is cheap enough, but the reduced model's RSE is 900%.
  expect_true(imp$meets_ofv)
  expect_false(imp$meets_rse)
  expect_false(imp$meets_threshold)
  expect_null(ev$covariate_to_remove)
})


test_that("the caller's completed_models decides eligibility, not the db status", {
  # submit_and_wait_for_step() returns completed_models = model_names wholesale
  # when auto_submit = FALSE, so a caller may legitimately pass a model whose
  # database status is not "completed". That must still be removable.
  st <- backward_state()
  st$search_database$status[st$search_database$model_name == "run20"] <- "created"

  ev <- evaluate_removal_impacts(
    search_state     = st,
    base_model       = "base",
    removal_models   = list(AGE = "run20"),
    completed_models = "run20",
    backward_p_value = 0.001
  )

  expect_equal(ev$covariate_to_remove, "AGE")
  expect_true(ev$removal_impacts$meets_threshold)
})


test_that("backward elimination keeps its sign when the action column is absent", {
  # `action` is not in update_model_status_from_files()'s self-heal list, so a
  # database can lack it and every row then reads as an addition. Deriving the
  # sign there while testing as a removal would invert the verdict, so backward
  # states its direction rather than looking it up.
  st <- backward_state()
  st$search_database$action <- NULL

  ev <- evaluate_removal_impacts(
    search_state     = st,
    base_model       = "base",
    removal_models   = list(AGE = "run20"),
    completed_models = "run20",
    backward_p_value = 0.001
  )

  db <- ev$search_state$search_database
  expect_equal(db$delta_ofv[db$model_name == "run20"], 2.0)
  expect_true(ev$removal_impacts$meets_threshold)
  expect_equal(ev$covariate_to_remove, "AGE")
})


test_that("a removal reaching select_best_model is rejected, not read as a gain", {
  # The mirror of the backward case. select_best_model back-fills a missing
  # delta_ofv and then tests with phase = "forward", so it must WRITE the
  # forward sign too. Deriving it would store this removal as model - parent
  # (+50, "the removal cost 50 points") and then read +50 as a 50-point
  # improvement, selecting it.
  st <- backward_state()
  st$search_database$delta_ofv <- NA_real_
  st$models_folder <- tempdir()

  sel <- suppressWarnings(suppressMessages(
    select_best_model(st, c("run21"), p_value = 0.05)
  ))

  db <- sel$search_state$search_database
  # parent - model = 144000 - 144050 = -50: a removal never looks like a gain.
  expect_equal(db$delta_ofv[db$model_name == "run21"], -50)
  expect_null(sel$best_model)

  # ...and the caller is told it passed the wrong kind of model.
  expect_warning(
    suppressMessages(select_best_model(st, c("run21"), p_value = 0.05)),
    "is a removal model"
  )
})


test_that("a non-removal handed to evaluate_removal_impacts is reported", {
  st <- backward_state()
  st$search_database$action[st$search_database$model_name == "run20"] <-
    "add_covariate"

  expect_warning(
    evaluate_removal_impacts(
      search_state     = st,
      base_model       = "base",
      removal_models   = list(AGE = "run20"),
      completed_models = "run20",
      backward_p_value = 0.001
    ),
    "not a removal"
  )
})


test_that("the backward ΔOFV write-back reaches the returned search_state", {
  st <- backward_state()
  st$search_database$delta_ofv <- NA_real_

  ev <- evaluate_removal_impacts(
    search_state     = st,
    base_model       = "base",
    removal_models   = list(AGE = "run20"),
    completed_models = "run20",
    backward_p_value = 0.001
  )

  db <- ev$search_state$search_database
  expect_equal(db$delta_ofv[db$model_name == "run20"], 2.0)
})


test_that("an unreadable RSE is rejected only when a covariance step is required", {
  # The two settings are one question asked twice. Requiring a covariance step
  # and then accepting a model whose precision could not be read is a
  # contradiction; not requiring one and rejecting for the absence of standard
  # errors punishes the model for something never asked of it.
  st <- criteria_state()
  st$search_database$rse_max <- c(NA_real_, 12, 15)

  strict <- st
  strict$search_config$require_cov_step <- TRUE
  ev <- .evaluate_model_criteria(strict, "run15", phase = "forward")
  expect_false(ev$meets_rse)
  expect_false(ev$meets_threshold)
  expect_match(ev$note, "RSE unavailable")
  expect_false(grepl("NA%", ev$note, fixed = TRUE))

  lax <- st
  lax$search_config$require_cov_step <- FALSE
  ev <- .evaluate_model_criteria(lax, "run15", phase = "forward")
  expect_true(ev$meets_rse)
  expect_true(ev$meets_threshold)

  # TRUE is the package default, so an absent setting must behave strictly.
  st$search_config$require_cov_step <- NULL
  expect_false(.evaluate_model_criteria(st, "run15", phase = "forward")$meets_rse)

  # No search_config at all is still strict, and must not error.
  bare <- st
  bare$search_config <- NULL
  expect_false(.evaluate_model_criteria(bare, "run15", phase = "forward")$meets_rse)

  # Fails CLOSED: only an explicit FALSE relaxes the rule. A malformed setting
  # is a configuration fault and must not silently switch the criterion off -
  # isTRUE() would send every one of these down the lenient branch.
  for (bad in list(NA, "yes", "FALSE", 0, c(FALSE, FALSE), character(0))) {
    st$search_config$require_cov_step <- bad
    expect_false(
      .evaluate_model_criteria(st, "run15", phase = "forward")$meets_rse,
      info = paste("malformed require_cov_step:", deparse(bad))
    )
  }
})


test_that("an unusable threshold never yields an NA verdict or an NA model name", {
  # x[NA] returns NA rather than dropping, so a single NA verdict becomes a
  # model name that does not exist and flows on into covariate lookup.
  st <- criteria_state()
  st$search_config$max_rse_threshold <- NA

  expect_equal(.resolve_rse_threshold(st), .DEFAULT_RSE_THRESHOLD)

  ev <- .evaluate_model_criteria(st, "run15", phase = "forward")
  expect_false(any(is.na(ev$meets_ofv)))
  expect_false(any(is.na(ev$meets_rse)))
  expect_false(any(is.na(ev$meets_threshold)))

  sig <- get_significant_models_from_step(st, step_number = 2, p_value = 0.05)
  expect_false(any(is.na(sig)))
})


test_that(".resolve_rse_threshold prefers the argument, then config, then the default", {
  st <- criteria_state(rse_threshold = 30)

  expect_equal(.resolve_rse_threshold(st, 12), 12)
  expect_equal(.resolve_rse_threshold(st), 30)

  # Asserted against the constant, not a literal, so changing the package
  # default does not require editing this test - the invariant is "the fallback
  # is the default", not "the fallback is 50".
  st$search_config$max_rse_threshold <- NULL
  expect_equal(.resolve_rse_threshold(st), .DEFAULT_RSE_THRESHOLD)
  expect_equal(.resolve_rse_threshold(list()), .DEFAULT_RSE_THRESHOLD)

  # A non-integer limit must survive; the display strings that report it used
  # "%d", which is a hard error in R for a fractional value.
  st$search_config$max_rse_threshold <- 33.3
  expect_equal(.resolve_rse_threshold(st), 33.3)
  expect_equal(sprintf("%g%%", .resolve_rse_threshold(st)), "33.3%")
})
