# The recovery helpers use the statuses the search actually writes.

test_that("the recovery report counts retries by action and failed by status", {
  # REC-06: failures were looked up as status "retry_failed", which is only
  # ever written to estimation_issue.
  db <- data.frame(
    model_name = c("run1", "run2", "run2001", "run3", "run3001"),
    status = c("completed", "failed", "failed", "failed", "completed"),
    action = c(NA, "add_single_covariate", "retry", "add_single_covariate", "retry"),
    estimation_issue = c(NA, "rounding", "retry_failed", "rounding", NA),
    excluded_from_step = FALSE, covariate_tested = NA_character_,
    stringsAsFactors = FALSE)
  out <- NULL
  invisible(capture.output(out <- generate_recovery_report(list(search_database = db))))
  expect_equal(out$retry_statistics$total_retries, 2)
  expect_equal(out$retry_statistics$successful_retries, 1)
  expect_equal(out$retry_statistics$failed_retries, 1)
})

test_that("detect_estimation_problems records the issue but not a status", {
  # REC-06: it stamped "estimation_error" from a possibly mid-run .ext.
  local_mocked_bindings(read_ext_file = function(search_state, model_name)
    list(has_estimation_issues = TRUE, issue_type = "high_ofv",
         current_ofv = 1e10, iterations = 3))
  st <- list(search_database = data.frame(model_name = "run2", status = "in_progress",
                                          estimation_issue = NA_character_,
                                          stringsAsFactors = FALSE))
  invisible(capture.output(res <- detect_estimation_problems(st, "run2")))
  expect_equal(res$search_state$search_database$status, "in_progress")
  expect_equal(res$search_state$search_database$estimation_issue, "high_ofv")
})
