# initialize_covariate_search(starting_model_number = ...): the override is
# applied after the counter has been set from the models already on disk.

test_that("an override below the highest existing model stops", {
  # run1 (base), run7, run12 on disk -> counter 12; run5 would reuse run6, run7.
  expect_error(
    .apply_starting_model_number(list(model_counter = 12L), 5, "run1"),
    "highest model already"
  )
})

test_that("an override at or above the highest existing model is applied", {
  out <- NULL
  invisible(capture.output(
    out <- .apply_starting_model_number(list(model_counter = 12L), 12, "run1")
  ))
  expect_equal(out$model_counter, 12L)

  invisible(capture.output(
    out <- .apply_starting_model_number(list(model_counter = 12L), 20, "run1")
  ))
  expect_equal(out$model_counter, 20L)
})

test_that("no override leaves the counter alone; the existing checks still apply", {
  expect_equal(.apply_starting_model_number(list(model_counter = 4L), NULL, "run1")$model_counter, 4L)
  expect_error(.apply_starting_model_number(list(model_counter = 1L), 0, "run1"), ">= 1")
  expect_error(.apply_starting_model_number(list(model_counter = 1L), 3, "run5"),
               "lower than base model number")
})
