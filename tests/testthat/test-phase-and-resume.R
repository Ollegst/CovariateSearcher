# The phase column names the part of the search a model belongs to
# (forward_selection, redemption, backward_elimination); action names what was
# done to it (add_covariate, remove_covariate, retry). Resume reads both.

db_row <- function(model_name, step, phase, action = "add_covariate", status = "completed",
                   parent = "run1", cov = NA_character_, original = NA_character_) {
  data.frame(model_name = model_name, step_number = step, phase = phase, action = action,
             status = status, parent_model = parent, covariate_tested = cov,
             original_model = original, estimation_issue = NA_character_,
             excluded_from_step = FALSE, created_by_search = TRUE,
             ofv = NA_real_, delta_ofv = NA_real_, rse_max = NA_real_,
             stringsAsFactors = FALSE)
}

test_that("trailing redemption steps are counted, retries and old databases included", {
  db <- rbind(db_row("run1", 0L, "base", "base_model", parent = NA),
              db_row("run2", 1L, "forward_selection"),
              db_row("run3", 2L, "forward_selection"),
              db_row("run4", 3L, "redemption"),
              db_row("run5", 4L, "redemption"),
              db_row("run5001", 4L, "retry", "retry", original = "run5"))
  expect_equal(.trailing_redemption_steps(list(search_database = db)), 2L)
  db$phase[db$phase == "redemption"] <- "forward_selection"   # an older database
  expect_equal(.trailing_redemption_steps(list(search_database = db)), 0L)
  db <- rbind(db, db_row("run6", 5L, "backward_elimination", "remove_covariate"))
  expect_equal(.trailing_redemption_steps(list(search_database = db)), 0L)
})

test_that("a retry found on disk is registered with its original's step and phase", {
  # REC-03: run<N>001 folders were never registered on resume.
  mf <- tempfile("models")
  dir.create(mf)
  file.create(file.path(mf, c("run4001.yaml", "run9001.yaml")))
  db <- rbind(db_row("run1", 0L, "base", "base_model", parent = NA),
              db_row("run4", 3L, "redemption", status = "failed", parent = "run2",
                     cov = "beta_WT_CL"))
  invisible(capture.output(
    st <- .register_disk_retries(list(models_folder = mf, search_database = db))))
  r <- st$search_database[st$search_database$model_name == "run4001", ]
  expect_equal(nrow(r), 1)
  expect_equal(r$phase, "redemption")
  expect_equal(r$action, "retry")
  expect_equal(r$step_number, 3L)
  expect_equal(r$parent_model, "run2")
  expect_equal(r$original_model, "run4")
  expect_equal(r$status, "created")
  # run9001's original is not in the database, so it is left alone.
  expect_false("run9001" %in% st$search_database$model_name)
})

test_that("the text report calls a step redemption only when its phase says so", {
  st <- list(base_model = "run1", models_folder = tempdir(),
             search_config = list(forward_p_value = 0.05, max_rse_threshold = 50),
             covariate_search = data.frame(COVARIATE = "WT", STATUS = "con", FORMULA = "power",
                                           LEVELS = NA, INIT = NA, stringsAsFactors = FALSE),
             search_database = rbind(
               db_row("run1", 0L, "base", "base_model", parent = NA),
               db_row("run2", 1L, "forward_selection", cov = "beta_WT_CL"),
               db_row("run2001", 1L, "retry", "retry", cov = "beta_WT_CL", original = "run2")))
  st$search_database$ofv <- c(1000, 990, 985)
  st$search_database$delta_ofv <- c(NA, 10, 15)
  st$search_database$rse_max <- 10
  lines <- function(s) suppressWarnings(suppressMessages(capture.output(
    generate_scm_report(s, output_file = tempfile(fileext = ".txt"), print_console = TRUE))))
  # A forward step with a retry was reported as "REDEMPTION/FINAL TESTING".
  expect_false(any(grepl("REDEMPTION", lines(st))))
  st$search_database$phase[2:3] <- "redemption"
  expect_true(any(grepl("^REDEMPTION$", trimws(lines(st)))))
})

resume_state <- function(last_status = "completed", dir_exists = TRUE) {
  mf <- tempfile("models")
  dir.create(mf)
  if (dir_exists) dir.create(file.path(mf, "run3"))
  list(base_model = "run1", models_folder = mf, tags = list(beta_WT_CL = "WT_CL"),
       search_config = list(forward_p_value = 0.05, backward_p_value = 0.001,
                            max_rse_threshold = 50, scm_type = "selective"),
       search_database = rbind(
         db_row("run1", 0L, "base", "base_model", parent = NA),
         db_row("run2", 1L, "forward_selection", cov = "beta_WT_CL"),
         db_row("run3", 2L, "forward_selection", status = last_status, parent = "run2",
                cov = "beta_WT_CL")))
}

test_that("resuming a selective search always re-enters selective forward", {
  # SCM-03: with nothing significant in the last step, resume went straight to
  # backward, so redemption never ran.
  called <- character(0)
  local_mocked_bindings(
    update_model_status_from_files = function(search_state, ...) search_state,
    update_all_model_statuses = function(search_state, ...) search_state,
    get_step_models = function(...) list(best_model = NULL, base_model = "run2",
                                         significant_models = character(0)),
    run_scm_selective_forward = function(search_state, ...) {
      called <<- c(called, "selective"); list(search_state = search_state, final_best_model = "run2")
    },
    run_backward_elimination = function(search_state, ...) {
      called <<- c(called, "backward"); list(search_state = search_state, final_model = "run2")
    })
  invisible(capture.output(continue_search(resume_state(), scm_type = "selective")))
  expect_equal(called, c("selective", "backward"))
})

test_that("a model that never started is not counted as failed on resume", {
  # REC-01: a model with no run directory read "failed", the step counted as
  # finished, and its covariates were never tested.
  local_mocked_bindings(
    update_model_status_from_files = function(search_state, model_name, ...) {
      i <- search_state$search_database$model_name == model_name
      if (!dir.exists(file.path(search_state$models_folder, model_name)))
        search_state$search_database$status[i] <- "failed"
      search_state
    },
    update_all_model_statuses = function(search_state, ...) search_state,
    run_scm_selective_forward = function(...) stop("must not continue past an unrun model"))
  st <- resume_state(last_status = "created", dir_exists = FALSE)
  invisible(capture.output(res <- continue_search(st, scm_type = "selective",
                                                   auto_submit = FALSE)))
  expect_equal(res$status, "incomplete_step")
  expect_equal(res$pending_models, "run3")
  expect_equal(res$search_state$search_database$status[3], "created")

  submitted <- NULL
  local_mocked_bindings(submit_and_wait_for_step = function(search_state, model_names, ...) {
    submitted <<- model_names
    list(search_state = search_state, completed_models = model_names, failed_models = character(0))
  })
  invisible(capture.output(try(continue_search(st, scm_type = "selective", auto_submit = TRUE),
                               silent = TRUE)))
  expect_equal(submitted, "run3")
})
