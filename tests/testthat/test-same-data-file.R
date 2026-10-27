# The $DATA-name check in initialize_covariate_search().

test_that("an .rds dataset matches the CSV named on $DATA", {
  expect_true(.same_data_file("analysis.csv", "data/derived/analysis.rds"))
  expect_true(.same_data_file("../data/Analysis.CSV", "analysis.RDS"))
})

test_that("the CSV itself still matches, case-insensitively", {
  expect_true(.same_data_file("analysis.csv", "data/derived/ANALYSIS.csv"))
})

test_that("a different file still fails", {
  expect_false(.same_data_file("analysis.csv", "other.csv"))
  expect_false(.same_data_file("analysis.csv", "other.rds"))
  expect_false(.same_data_file("analysis.csv", "analysis.dat"))
})
