# require_cov_step is read one way everywhere; malformed settings stop at the
# entry points instead of being reinterpreted downstream.

test_that("require_cov_step reads as TRUE unless explicitly FALSE", {
  # XAR-05: NA was "completed" in one reader, an error in another, a rejection
  # in the evaluator.
  st <- function(x) list(search_config = list(require_cov_step = x))
  expect_true(.require_cov_step(st(TRUE)))
  expect_false(.require_cov_step(st(FALSE)))
  expect_true(.require_cov_step(st(NA)))
  expect_true(.require_cov_step(st("no")))
  expect_true(.require_cov_step(list(search_config = list())))
})

test_that("entry points reject a require_cov_step that is not TRUE or FALSE", {
  expect_error(.check_require_cov_step(NA), "TRUE or FALSE")
  expect_error(.check_require_cov_step(c(TRUE, FALSE)), "TRUE or FALSE")
  expect_error(initialize_search_config(list(), require_cov_step = NA), "TRUE or FALSE")
  expect_error(validate_base_model_for_search("run1", tempdir(), require_cov_step = "yes"),
               "TRUE or FALSE")
  expect_silent(.check_require_cov_step(FALSE))
})

test_that("a non-numeric rse_threshold stops instead of becoming 50", {
  # UTL-06: "10" passed and was silently replaced by the default.
  st <- list(search_config = list(max_rse_threshold = 50),
             search_database = data.frame(model_name = character(0)))
  expect_error(select_best_model(st, "run2", rse_threshold = "10"), "single positive number")
  expect_error(select_best_model(st, "run2", rse_threshold = NA), "single positive number")
  expect_error(select_best_model(st, "run2", rse_threshold = c(30, 40)), "single positive number")
  expect_error(continue_search(st, rse_threshold = "30"), "single positive number")
  expect_silent(.check_rse_threshold(NULL))
  expect_silent(.check_rse_threshold(30))
})
