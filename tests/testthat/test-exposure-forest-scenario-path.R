# plot_exposure_forest(scenario = <path>) with a path that does not exist.

forest_metrics <- function() {
  data.frame(
    Scenario = rep(c("Typical subject", "Low WT", "High WT"), each = 20),
    ID = rep(1:20, 3),
    AUC = c(rep(100, 20), rep(80, 20), rep(120, 20)) * exp(rnorm(60, 0, 0.05))
  )
}

test_that("a mistyped scenario file path stops instead of emptying the plot", {
  set.seed(1)
  missing <- file.path(tempdir(), "no_such_scenario_table.rds")
  expect_error(
    plot_exposure_forest(forest_metrics(), metric = "AUC", scenario = missing),
    "scenario.*not found"
  )
})

test_that("a plain one-element character vector is still an axis order", {
  set.seed(1)
  expect_no_error(suppressWarnings(
    plot_exposure_forest(forest_metrics(), metric = "AUC", scenario = "Typical subject")
  ))
})
