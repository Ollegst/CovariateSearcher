# New model numbers must never land on a model that already exists.

test_that("the counter rises past run<N> models on disk but not in the database", {
  # REC-07: a skipped run5 folder was below nothing in the database, so the
  # next model could be named run5 and overwritten.
  mf <- tempfile("models")
  dir.create(mf)
  dir.create(file.path(mf, "run5"))
  file.create(file.path(mf, c("run3.ctl", "run2001.ctl", "run7.yaml", "notes.txt")))
  st <- list(models_folder = mf, base_model = "run1",
             search_database = data.frame(model_name = c("run1", "run3", "run3001")))
  invisible(capture.output(st <- update_model_counter(st)))
  expect_equal(st$model_counter, 7)

  unlink(file.path(mf, "run7.yaml"))
  invisible(capture.output(st <- update_model_counter(st)))
  expect_equal(st$model_counter, 5)
})

test_that("a non-run name in the database does not make the counter NA", {
  st <- list(models_folder = tempfile("none"), base_model = "run1",
             search_database = data.frame(model_name = c("run1", "base_alt", "run4")))
  invisible(capture.output(st <- update_model_counter(st)))
  expect_equal(st$model_counter, 4)
})

test_that("an initial-backward error stops the search instead of going forward", {
  # SCM-04: forward ran on the pre-phase state, reusing run numbers the
  # backward phase had already written.
  forward_called <- FALSE
  local_mocked_bindings(
    run_backward_elimination = function(...) stop("boom"),
    run_stepwise_covariate_modeling = function(...) { forward_called <<- TRUE; NULL },
    run_scm_selective_forward = function(...) { forward_called <<- TRUE; NULL }
  )
  st <- list(base_model = "run1", models_folder = tempfile("models"),
             search_config = list(),
             search_database = data.frame(model_name = "run1", step_number = 0))
  invisible(capture.output(
    res <- run_automated_scm_testing(st, starting_phase = "backward",
                                     save_checkpoints = FALSE)
  ))
  expect_equal(res$status, "failed")
  expect_equal(res$error_phase, "initial_backward")
  expect_match(res$error_message, "boom")
  expect_false(forward_called)
})


test_that("final backward runs after an unchanged forward only if untested covariates remain", {
  # SCM-02: with starting_phase = "forward", a starting model that already
  # carries covariates was never tested for removal when forward added
  # nothing. With no covariates there is nothing to remove, so it still skips.
  run_with <- function(model_covs) {
    backward_called <- FALSE
    local_mocked_bindings(
      run_stepwise_covariate_modeling = function(search_state, ...)
        list(search_state = search_state, final_model = "run1"),
      get_model_covariates_from_db = function(...) model_covs,
      run_backward_elimination = function(search_state, ...) {
        backward_called <<- TRUE
        list(search_state = search_state, final_model = "run1")
      },
      get_excluded_covariates = function(...) character(0)
    )
    st <- list(base_model = "run1", models_folder = tempfile("models"), search_config = list(),
               search_database = data.frame(model_name = "run1", step_number = 0))
    invisible(capture.output(try(
      run_automated_scm_testing(st, starting_phase = "forward", full_scm = TRUE,
                                save_checkpoints = FALSE), silent = TRUE)))
    backward_called
  }
  expect_true(run_with("WT_CL"))
  expect_false(run_with(character(0)))
})
