# A run<N> folder with no run<N>.yaml beside it (a leftover output folder)
# must not stop resume.

test_that("model folders without a .yaml are not offered for registration", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  dir.create(file.path(models_dir, "run2"))
  writeLines("based_on: run1", file.path(models_dir, "run2.yaml"))
  dir.create(file.path(models_dir, "run9"))          # stray: no run9.yaml

  search_state <- list(
    models_folder = models_dir,
    search_database = data.frame(model_name = "run1", stringsAsFactors = FALSE)
  )

  out <- NULL
  printed <- capture.output(out <- .scm_unregistered_models(search_state))
  expect_identical(out, "run2")
  expect_match(paste(printed, collapse = "\n"), "run9")
})


test_that("reconstruct_step_from_disk does not stop on a stray folder", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  dir.create(file.path(models_dir, "run9"))

  search_state <- list(
    models_folder = models_dir,
    search_database = data.frame(
      model_name = "run1", step_number = 0L, phase = "base",
      action = "base_model", status = "completed", ofv = 1000,
      created_by_search = FALSE, stringsAsFactors = FALSE
    )
  )

  expect_no_error(invisible(capture.output(reconstruct_step_from_disk(search_state))))
})
