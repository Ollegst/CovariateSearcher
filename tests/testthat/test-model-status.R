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


test_that("a listing with cp1252 bytes is read without a warning storm", {
  # NIO-03: readLines() returned the invalid lines without error, so the old
  # latin1 fallback never ran and every regex pass over them warned.
  models_dir <- setup_finished_run()
  lst <- file.path(models_dir, "run1", "run1.lst")
  con <- file(lst, open = "ab")
  writeBin(c(charToRaw(" conc in "), as.raw(0xB5), charToRaw("g/L\n")), con)
  close(con)

  expect_true(all(validUTF8(.read_listing_lines(lst))))
  expect_no_warning(probe <- read_nonmem_lst(file.path(models_dir, "run1")))
  expect_equal(probe$status, "completed")
})


test_that("a run is finished only once its listing has Stop Time", {
  # NIO-02: a listing with MINIMIZATION SUCCESSFUL but no Stop Time (still in
  # $COV, or between $EST steps) was "completed"/"failed" for one reader and
  # "in progress" for the other.
  models_dir <- setup_finished_run()
  lst <- file.path(models_dir, "run1", "run1.lst")
  lines <- readLines(lst)
  writeLines(c(lines[!grepl("^Stop Time:", lines)][1:4], "1NONMEM EXECUTION"), lst)
  expect_equal(read_nonmem_lst(file.path(models_dir, "run1"))$status, "incomplete")
  expect_equal(get_model_status_from_files(file.path(models_dir, "run1"),
                                           require_cov_step = FALSE), "incomplete")
  expect_equal(get_model_status_from_files(file.path(models_dir, "run1"),
                                           require_cov_step = TRUE), "incomplete")
  st <- list(models_folder = models_dir, search_config = list(),
             search_database = data.frame(model_name = "run1", status = "in_progress",
                                          stringsAsFactors = FALSE))
  invisible(capture.output(st <- update_model_status_from_files(st, "run1", force = TRUE)))
  expect_equal(st$search_database$status, "in_progress")

  # A termination line from an earlier $EST step does not end the run either.
  writeLines(c(" #TERM:", "0MINIMIZATION TERMINATED", "1NONMEM EXECUTION"), lst)
  expect_equal(read_nonmem_lst(file.path(models_dir, "run1"))$status, "incomplete")
})

test_that("an NM-TRAN error is a failure with or without Stop Time", {
  models_dir <- setup_finished_run()
  unlink(file.path(models_dir, "run1", c("run1.ext", "run1.cov")))
  lst <- file.path(models_dir, "run1", "run1.lst")
  for (with_stop in c(FALSE, TRUE)) {
    writeLines(c(" AN ERROR WAS FOUND IN THE CONTROL STATEMENTS.",
                 " (DATA ERROR) RECORD 1, DATA ITEM 3: NON-NUMERIC",
                 if (with_stop) c("Stop Time:", "Sat 25 Jul 2026 06:50:30 AM EDT")), lst)
    expect_equal(read_nonmem_lst(file.path(models_dir, "run1"))$status, "failed")
    expect_equal(get_model_status_from_files(file.path(models_dir, "run1")), "failed")
    st <- list(models_folder = models_dir, search_config = list(),
               search_database = data.frame(model_name = "run1", status = "in_progress",
                                            stringsAsFactors = FALSE))
    invisible(capture.output(st <- update_model_status_from_files(st, "run1", force = TRUE)))
    expect_equal(st$search_database$status, "failed")
  }
})

test_that("a finished run that ended on rounding errors still counts on OFV and .cov", {
  models_dir <- setup_finished_run()
  lst <- file.path(models_dir, "run1", "run1.lst")
  writeLines(c(" #TERM:", "0MINIMIZATION TERMINATED", " DUE TO ROUNDING ERRORS (ERROR=134)",
               "Stop Time:", "Sat 25 Jul 2026 06:50:30 AM EDT"), lst)
  expect_equal(get_model_status_from_files(file.path(models_dir, "run1")), "completed")
})

test_that("an NM-TRAN error is not overridden by result files from an earlier run", {
  # diff-auditor: a stale run1.ext and run1.cov made the rejected rerun read
  # as completed with the old OFV.
  models_dir <- setup_finished_run()
  lst <- file.path(models_dir, "run1", "run1.lst")
  writeLines(c(" AN ERROR WAS FOUND IN THE CONTROL STATEMENTS.",
               "Stop Time:", "Sat 25 Jul 2026 06:50:30 AM EDT"), lst)
  expect_equal(get_model_status_from_files(file.path(models_dir, "run1")), "failed")
  st <- list(models_folder = models_dir, search_config = list(),
             search_database = data.frame(model_name = "run1", status = "in_progress",
                                          stringsAsFactors = FALSE))
  invisible(capture.output(st <- update_model_status_from_files(st, "run1", force = TRUE)))
  expect_equal(st$search_database$status, "failed")
})

test_that("an unfinished run keeps its last termination line as the reason", {
  models_dir <- setup_finished_run()
  lst <- file.path(models_dir, "run1", "run1.lst")
  writeLines(c(" #TERM:", "0MINIMIZATION TERMINATED", " DUE TO ROUNDING ERRORS (ERROR=134)"), lst)
  probe <- read_nonmem_lst(file.path(models_dir, "run1"))
  expect_equal(probe$status, "incomplete")
  expect_match(probe$error_message, "ROUNDING ERRORS")
})

test_that("one status check reads the listing once", {
  # NIO-05: a check read the same .lst several times (find it, Stop Time,
  # timestamps).
  models_dir <- setup_finished_run()
  reads <- 0L
  real_reader <- .read_listing_lines
  local_mocked_bindings(.read_listing_lines = function(...) {
    reads <<- reads + 1L
    real_reader(...)
  })
  st <- list(models_folder = models_dir, search_config = list(require_cov_step = TRUE),
             search_database = data.frame(model_name = "run1", status = "in_progress",
                                          stringsAsFactors = FALSE))
  invisible(capture.output(st <- update_model_status_from_files(st, "run1", force = TRUE)))
  expect_equal(st$search_database$status, "completed")
  expect_equal(reads, 1L)
})

test_that("timestamps with a numeric time zone parse", {
  # NIO-07: "+03" / "-0500" left both times NA.
  for (z in c("+03", "-0500")) {
    md <- setup_finished_run(start_line = sprintf("Wed Jul 29 14:30:08 %s 2026", z),
                             stop_line  = sprintf("Sat 25 Jul 2026 06:50:30 AM %s", z))
    ts <- extract_nonmem_timestamps("run1", md)
    expect_false(is.na(ts$start_time))
    expect_false(is.na(ts$stop_time))
  }
})

test_that("self-heal adds missing columns with their real types", {
  # STA-08: a healed completion_time became character and step_number double.
  models_dir <- setup_finished_run()
  st <- list(models_folder = models_dir, search_config = list(),
             search_database = data.frame(model_name = "run1", status = "in_progress",
                                          stringsAsFactors = FALSE))
  invisible(capture.output(st <- update_model_status_from_files(st, "run1", force = TRUE)))
  db <- st$search_database
  expect_s3_class(db$submission_time, "POSIXct")
  expect_s3_class(db$completion_time, "POSIXct")
  expect_type(db$step_number, "integer")
  expect_type(db$ofv, "double")
  expect_identical(db$excluded_from_step, FALSE)
})

test_that("a finished model gets NONMEM's own start time from the status check", {
  # diff-auditor (4.8): the monitoring loop skips a model once its stop time
  # is set, so a model that finished between two checks kept the R-side
  # submission time. The status check now records the start time with the stop.
  models_dir <- setup_finished_run(start_line = "Wed Jul 29 14:30:08 EDT 2026")
  st <- list(models_folder = models_dir, search_config = list(),
             search_database = data.frame(model_name = "run1", status = "in_progress",
                                          submission_time = as.POSIXct("2000-01-01 00:00:00"),
                                          stringsAsFactors = FALSE))
  invisible(capture.output(st <- update_model_status_from_files(st, "run1", force = TRUE)))
  expect_equal(format(st$search_database$submission_time, "%Y-%m-%d %H:%M"), "2026-07-29 14:30")
  expect_false(is.na(st$search_database$completion_time))
})
