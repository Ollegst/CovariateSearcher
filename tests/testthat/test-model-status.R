# A finished NONMEM run on disk: listing, final estimates, covariance matrix.
setup_finished_run <- function(stop_line = "Sat 25 Jul 2026 06:50:30 AM EDT",
                               start_line = "Wed Jul 29 14:30:08 EDT 2026") {
  models_dir <- tempfile("models")
  dir.create(file.path(models_dir, "run1"), recursive = TRUE)

  writeLines(
    c(
      start_line,
      " #TERM:",
      "0MINIMIZATION SUCCESSFUL",
      " NO. OF FUNCTION EVALUATIONS USED:     1308",
      "1THERE ARE ERROR MESSAGES IN FILE PRDERR",
      "Stop Time:",
      stop_line
    ),
    file.path(models_dir, "run1", "run1.lst")
  )

  writeLines(
    c(
      "TABLE NO.     1: First Order Conditional Estimation",
      " ITERATION THETA1 THETA2 OBJ",
      " 0 1.0 2.0 -100.00",
      " -1000000000 1.10 2.20 -123.45"
    ),
    file.path(models_dir, "run1", "run1.ext")
  )

  writeLines("covariance", file.path(models_dir, "run1", "run1.cov"))

  models_dir
}


test_that("only a final answer counts as finished", {
  expect_true(all(.is_terminal_status(
    c("completed", "failed", "estimation_error")
  )))
  expect_false(any(.is_pending_status(
    c("completed", "failed", "estimation_error")
  )))
})


test_that("every unfinished spelling is pending, including ones nobody wrote yet", {
  expect_true(all(.is_pending_status(
    c("created", "submitted", "in_progress", "incomplete", "unknown")
  )))

  # The point of inverting the test: a status no one anticipated blocks rather
  # than being read as finished.
  expect_true(.is_pending_status("queued_on_grid"))
  expect_true(.is_pending_status(NA_character_))
})


test_that("a finished run with a covariance matrix is completed", {
  models_dir <- setup_finished_run()

  expect_equal(
    get_model_status_from_files(file.path(models_dir, "run1")),
    "completed"
  )
})


test_that("require_cov_step decides whether a missing .cov fails the run", {
  models_dir <- setup_finished_run()
  file.remove(file.path(models_dir, "run1", "run1.cov"))

  expect_equal(
    get_model_status_from_files(file.path(models_dir, "run1"), require_cov_step = TRUE),
    "failed"
  )
  expect_equal(
    get_model_status_from_files(file.path(models_dir, "run1"), require_cov_step = FALSE),
    "completed"
  )
})


test_that("the .cov is found when the model is given as its .lst path", {
  models_dir <- setup_finished_run()

  # read_nonmem_lst() accepts a path to the listing itself; deriving the run's
  # name with basename() would send the .cov check looking for run1.lst.cov.
  expect_equal(
    get_model_status_from_files(file.path(models_dir, "run1", "run1.lst")),
    "completed"
  )
})


test_that("a listing that exists but cannot be read is failed, not still running", {
  models_dir <- setup_finished_run()

  # A directory in the listing's place resolves and then fails to open, which is
  # the same shape as a listing R cannot read.
  unlink(file.path(models_dir, "run1", "run1.lst"))
  dir.create(file.path(models_dir, "run1", "run1.lst"))

  probe <- read_nonmem_lst(file.path(models_dir, "run1"))
  expect_true(probe$found)
  expect_equal(probe$status, "read_error")
  expect_false(is.null(probe$file))

  expect_equal(
    get_model_status_from_files(file.path(models_dir, "run1")),
    "failed"
  )
})


test_that("a listing carrying non-UTF-8 bytes is still classified", {
  models_dir <- setup_finished_run()
  lst <- file.path(models_dir, "run1", "run1.lst")

  con <- file(lst, open = "ab")
  writeBin(c(as.raw(0xFF), charToRaw("\n")), con)
  close(con)

  expect_silent(lines <- .read_listing_lines(lst))
  expect_true(length(lines) > 5)
  expect_equal(read_nonmem_lst(file.path(models_dir, "run1"))$status, "completed")
})


test_that("update_model_status_from_files records an unreadable listing as failed", {
  models_dir <- setup_finished_run()
  unlink(file.path(models_dir, "run1", "run1.lst"))
  dir.create(file.path(models_dir, "run1", "run1.lst"))

  search_state <- list(
    models_folder = models_dir,
    search_config = list(require_cov_step = TRUE),
    search_database = data.frame(
      model_name = "run1",
      status = "in_progress",
      ofv = NA_real_,
      rse_max = NA_real_,
      error_message = NA_character_,
      estimation_issue = NA_character_,
      completion_time = as.POSIXct(NA),
      stringsAsFactors = FALSE
    )
  )

  out <- update_model_status_from_files(search_state, "run1")

  # Never "in_progress": the monitoring loop waits on that and would not stop.
  expect_equal(out$search_database$status[1], "failed")
})


test_that("a valid UTF-8 listing keeps its characters", {
  models_dir <- setup_finished_run()
  lst <- file.path(models_dir, "run1", "run1.lst")

  writeLines(c(readLines(lst), " CONCENTRATION IN µg/mL AT 37°C"), lst)

  lines <- .read_listing_lines(lst)
  expect_true(any(grepl("µg/mL", lines, fixed = TRUE)))
  expect_true(any(grepl("37°C", lines, fixed = TRUE)))
})


test_that("both NONMEM timestamp layouts parse", {
  day_first <- setup_finished_run(
    start_line = "Wed 29 Jul 2026 02:30:08 PM EDT",
    stop_line  = "Sat 25 Jul 2026 06:50:30 AM EDT"
  )
  ts <- extract_nonmem_timestamps("run1", day_first)
  expect_false(is.na(ts$start_time))
  expect_false(is.na(ts$stop_time))

  month_first <- setup_finished_run(
    start_line = "Wed Jul 29 14:30:08 EDT 2026",
    stop_line  = "Sat Jul 25 06:50:30 EDT 2026"
  )
  ts <- extract_nonmem_timestamps("run1", month_first)
  expect_false(is.na(ts$start_time))
  expect_false(is.na(ts$stop_time))
})


test_that("timestamp parsing does not leave LC_TIME changed", {
  models_dir <- setup_finished_run()
  before <- Sys.getlocale("LC_TIME")

  extract_nonmem_timestamps("run1", models_dir)

  expect_equal(Sys.getlocale("LC_TIME"), before)
})


# A finished run inside `models_dir` whose .ext reports the given OFV.
write_finished_run <- function(models_dir, run, ofv) {
  dir.create(file.path(models_dir, run), recursive = TRUE, showWarnings = FALSE)

  writeLines(
    c(
      "Wed Jul 29 14:30:08 EDT 2026",
      " #TERM:",
      "0MINIMIZATION SUCCESSFUL",
      "Stop Time:",
      "Sat 25 Jul 2026 06:50:30 AM EDT"
    ),
    file.path(models_dir, run, paste0(run, ".lst"))
  )

  writeLines(
    c(
      "TABLE NO.     1: First Order Conditional Estimation",
      " ITERATION THETA1 THETA2 OBJ",
      " 0 1.0 2.0 -100.00",
      sprintf(" -1000000000 1.10 2.20 %.2f", ofv)
    ),
    file.path(models_dir, run, paste0(run, ".ext"))
  )

  writeLines("covariance", file.path(models_dir, run, paste0(run, ".cov")))
  invisible(models_dir)
}


# A base model plus three children whose ΔOFV land either side of the two
# thresholds (df=1: forward 3.84, backward 10.83): a removal clear of the
# backward one, an addition clear of the forward one, and a removal between them.
setup_step_report <- function() {
  models_dir <- tempfile("models")
  dir.create(models_dir, recursive = TRUE)

  write_finished_run(models_dir, "run2", -123.45)  # removal,  ΔOFV +76.55
  write_finished_run(models_dir, "run3", -223.45)  # addition, ΔOFV +23.45
  write_finished_run(models_dir, "run4", -195.00)  # removal,  ΔOFV  +5.00

  list(
    models_folder = models_dir,
    search_config = list(
      forward_p_value = 0.05,
      backward_p_value = 0.001,
      require_cov_step = TRUE
    ),
    search_database = data.frame(
      model_name = c("run1", "run2", "run3", "run4"),
      parent_model = c(NA_character_, "run1", "run1", "run1"),
      covariate_tested = c(NA_character_, "beta_SEX_CL",
                           "beta_AGE_CL", "beta_WT_V1"),
      action = c("base_model", "remove_covariate",
                 "add_covariate", "remove_covariate"),
      phase = c("base", "backward", "forward", "backward"),
      step_number = c(0L, 1L, 1L, 1L),
      status = c("completed", "in_progress", "in_progress", "in_progress"),
      ofv = c(-200, NA_real_, NA_real_, NA_real_),
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      error_message = NA_character_,
      estimation_issue = NA_character_,
      completion_time = as.POSIXct(NA),
      stringsAsFactors = FALSE
    )
  )
}


test_that("a significant removal is reported as retained, not as an improvement", {
  out <- capture.output(update_all_model_statuses(setup_step_report()))

  improved <- grep("New significant improvements", out)
  retained <- grep("Removal rejected", out)
  expect_length(improved, 1)
  expect_length(retained, 1)

  # Both read the same delta_ofv column: the addition bought 23.45 OFV points,
  # the removal cost 76.55 of them. Announcing the second as an improvement is
  # the inversion this guards against.
  expect_true(any(grepl("^   run3 \\(beta_AGE_CL\\): ΔOFV = 23.45$", out)))
  expect_true(any(grepl(
    "^   run2 \\(beta_SEX_CL\\): ΔOFV = 76.55 \\(cost of removal\\)$", out
  )))
  expect_gt(grep("^   run2 ", out), retained)
  expect_lt(grep("^   run3 ", out), retained)
})


test_that("a removal is judged at the backward p-value, not the forward one", {
  out <- capture.output(update_all_model_statuses(setup_step_report()))

  # ΔOFV 5.00 clears the forward threshold but not the backward one, so this
  # covariate is droppable and there is nothing significant to announce.
  expect_false(any(grepl("^   run4 ", out)))
})
