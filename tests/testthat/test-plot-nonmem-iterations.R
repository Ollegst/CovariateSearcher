# A minimal NONMEM run on disk: <models_dir>/<model>/<model>.ext holds the
# iteration table, <models_dir>/<model>.ctl carries the annotations that name
# each parameter and say which THETAs are on the log scale.
setup_iteration_model <- function(ctl_lines = NULL) {
  models_dir <- tempfile("models")
  dir.create(file.path(models_dir, "run1"), recursive = TRUE)

  writeLines(
    c(
      "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
      " ITERATION THETA1 THETA2 THETA3 SIGMA(1,1) OMEGA(1,1) OBJ",
      " 0 1.0 2.0 5.0 0.10 0.09 100.0",
      " 1 1.1 2.2 5.0 0.11 0.16 90.0",
      " 2 1.2 2.4 5.0 0.12 0.25 85.0"
    ),
    file.path(models_dir, "run1", "run1.ext")
  )

  writeLines(
    ctl_lines %||% c(
      "$THETA",
      "0.5 ; CL ; L/h ; LOG",
      "2.0 ; V1 ; L ; RATIO",
      "5.0 ; KA ; 1/hr ; FIX",
      "$OMEGA",
      "0.09 ; IIV_CL ; ; LOG",
      "$SIGMA",
      "0.10 ; prop ; ; LOG"
    ),
    file.path(models_dir, "run1.ctl")
  )

  models_dir
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Values for one facet, in iteration order.
facet_values <- function(p, label) {
  p$data$value[as.character(p$data$variable) == label]
}

panels <- function(p) levels(p$data$variable)


test_that("panels are named from the control stream", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = TRUE)

  expect_true(all(c("CL", "V1", "IIV_CL", "prop") %in% panels(p)))
  expect_false(any(grepl("^THETA|^OMEGA|^SIGMA", panels(p))))
})


test_that("a ;LOG theta is exponentiated and a ;RATIO theta is left alone", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = TRUE)

  expect_equal(facet_values(p, "CL"), exp(c(1.0, 1.1, 1.2)))
  expect_equal(facet_values(p, "V1"), c(2.0, 2.2, 2.4))
})


test_that("OMEGA and SIGMA keep the values NONMEM wrote", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = TRUE)

  expect_equal(facet_values(p, "IIV_CL"), c(0.09, 0.16, 0.25))
  expect_equal(facet_values(p, "prop"), c(0.10, 0.11, 0.12))
})


test_that("transform = FALSE plots the .ext exactly as written", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = FALSE)

  expect_equal(facet_values(p, "THETA1"), c(1.0, 1.1, 1.2))
  expect_equal(facet_values(p, "OMEGA.1.1."), c(0.09, 0.16, 0.25))
  expect_false(any(c("CL", "IIV_CL") %in% panels(p)))
})


test_that("a parameter that never moves is dropped, named or not", {
  models_dir <- setup_iteration_model()

  for (tr in c(TRUE, FALSE)) {
    p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = tr)
    expect_false(any(c("THETA3", "KA") %in% panels(p)))
  }
})


test_that("each record is named on its own, so one bad block costs only itself", {
  # Two annotated $THETA lines for three .ext THETA columns: the names after the
  # gap would land on the wrong parameter, so the whole record is left alone.
  # $OMEGA and $SIGMA still match their columns and are still named.
  models_dir <- setup_iteration_model(c(
    "$THETA",
    "0.5",
    "2.0 ; V1 ; L ; RATIO",
    "5.0 ; KA ; 1/hr ; FIX",
    "$OMEGA",
    "0.09 ; IIV_CL ; ; LOG",
    "$SIGMA",
    "0.10 ; prop ; ; LOG"
  ))

  expect_warning(
    p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = TRUE),
    "\\$THETA"
  )

  expect_true(all(c("THETA1", "THETA2") %in% panels(p)))
  expect_equal(facet_values(p, "THETA1"), c(1.0, 1.1, 1.2))
  expect_true(all(c("IIV_CL", "prop") %in% panels(p)))
})


test_that("obj_var selects the leading panel and is not repeated", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir,
                              obj_var = "THETA2", transform = TRUE)

  expect_equal(panels(p)[1], "V1")
  expect_equal(sum(panels(p) == "V1"), 1L)
  expect_equal(facet_values(p, "V1"), c(2.0, 2.2, 2.4))
})


test_that("a log-scale column promoted by obj_var is still rescaled and named", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir,
                              obj_var = "THETA1", transform = TRUE)

  expect_equal(panels(p)[1], "CL")
  expect_equal(facet_values(p, "CL"), exp(c(1.0, 1.1, 1.2)))
})


test_that("the default obj_var plots the objective first", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir)

  expect_equal(panels(p)[1], "OBJ")
  expect_equal(facet_values(p, "OBJ"), c(100.0, 90.0, 85.0))
})


test_that("an obj_var that is not a column errors and lists what is available", {
  models_dir <- setup_iteration_model()

  expect_error(
    plot_nonmem_iterations("run1", models_dir = models_dir, obj_var = "NOPE"),
    "obj_var 'NOPE' is not a column"
  )
  expect_error(
    plot_nonmem_iterations("run1", models_dir = models_dir, obj_var = "NOPE"),
    "THETA1"
  )
})


test_that("transform must be a single TRUE or FALSE", {
  models_dir <- setup_iteration_model()

  expect_error(
    plot_nonmem_iterations("run1", models_dir = models_dir, transform = NA),
    "transform must be TRUE or FALSE"
  )
})


test_that("an unreadable control stream warns and falls back to the .ext", {
  models_dir <- setup_iteration_model()
  file.remove(file.path(models_dir, "run1.ctl"))

  expect_warning(
    p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = TRUE),
    "could not be read"
  )

  expect_equal(facet_values(p, "THETA1"), c(1.0, 1.1, 1.2))
  expect_false(any(c("CL", "IIV_CL") %in% panels(p)))
})
