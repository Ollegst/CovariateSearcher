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
