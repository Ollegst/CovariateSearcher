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

test_that("exposure tables tabulate only the labelled metrics", {
  # SIM-07 (owner decision): an unlabelled numeric column stopped the call.
  set.seed(1)
  d <- forest_metrics()
  d$CMAX <- d$AUC / 10                 # a numeric column with no label
  scen <- data.frame(Scenario = c("Typical subject", "Low WT", "High WT"), WT = c(70, 50, 90))
  out <- tempfile("forest")
  dir.create(out)
  suppressWarnings(plot_exposure_forest(
    d, metric = "AUC", scenario = scen, filename = file.path(out, "forest.png"),
    metric_info = list(AUC = list(label = "AUC", unit = "h*ng/mL")), model = "run1",
    width = 6, height = 4))
  tbl <- readRDS(file.path(out, "run1-exposure-table-absolute.rds"))
  hdr <- paste(names(tbl), collapse = " ")
  expect_match(hdr, "AUC")
  expect_false(grepl("CMAX", hdr))

  expect_error(suppressWarnings(plot_exposure_forest(
    d, metric = "AUC", scenario = scen, filename = file.path(out, "forest.png"),
    metric_info = list(OTHER = list(label = "X", unit = "")), model = "run1",
    width = 6, height = 4)),
    "No numeric column of `data` has a label", fixed = TRUE)
})
