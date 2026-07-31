# A minimal NONMEM run on disk: <models_dir>/<model>/<model>.ext holds the
# iteration table, <models_dir>/<model>.ctl carries the ;LOG/;RATIO annotations
# that decide how each THETA is back-transformed.
setup_iteration_model <- function() {
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
    c(
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

# Values for one facet, in iteration order.
facet_values <- function(p, label) {
  p$data$value[as.character(p$data$variable) == label]
}


test_that("transform = TRUE exponentiates a ;LOG theta and leaves a ;RATIO theta alone", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = TRUE)

  expect_equal(facet_values(p, "THETA1 [natural]"), exp(c(1.0, 1.1, 1.2)))
  expect_equal(facet_values(p, "THETA2"), c(2.0, 2.2, 2.4))
})


test_that("transform = TRUE reports diagonal OMEGA and SIGMA as CV%", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = TRUE)

  expect_equal(
    facet_values(p, "OMEGA.1.1. [CV%]"),
    100 * sqrt(exp(c(0.09, 0.16, 0.25)) - 1)
  )
  expect_equal(
    facet_values(p, "SIGMA.1.1. [CV%]"),
    100 * sqrt(exp(c(0.10, 0.11, 0.12)) - 1)
  )
})


test_that("transform = FALSE plots the raw .ext values", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = FALSE)

  expect_equal(facet_values(p, "THETA1"), c(1.0, 1.1, 1.2))
  expect_equal(facet_values(p, "OMEGA.1.1."), c(0.09, 0.16, 0.25))
  expect_false(any(grepl("CV%", levels(p$data$variable), fixed = TRUE)))
})


test_that("a parameter that never moves is dropped, whatever the scale", {
  models_dir <- setup_iteration_model()

  for (tr in c(TRUE, FALSE)) {
    p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = tr)
    expect_false(any(grepl("THETA3", levels(p$data$variable), fixed = TRUE)))
  }
})


test_that("obj_var selects the leading panel and is not repeated as a parameter", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir, obj_var = "THETA2")

  expect_equal(levels(p$data$variable)[1], "THETA2")
  expect_equal(sum(levels(p$data$variable) == "THETA2"), 1L)
  expect_equal(facet_values(p, "THETA2"), c(2.0, 2.2, 2.4))
})


test_that("a rescalable column promoted by obj_var is still rescaled", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations(
    "run1", models_dir = models_dir, obj_var = "OMEGA.1.1.", transform = TRUE
  )

  expect_equal(levels(p$data$variable)[1], "OMEGA.1.1. [CV%]")
  expect_equal(
    facet_values(p, "OMEGA.1.1. [CV%]"),
    100 * sqrt(exp(c(0.09, 0.16, 0.25)) - 1)
  )
})


test_that("the default obj_var plots the objective first", {
  models_dir <- setup_iteration_model()

  p <- plot_nonmem_iterations("run1", models_dir = models_dir)

  expect_equal(levels(p$data$variable)[1], "OBJ")
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


test_that("a $THETA line without its annotation is refused rather than misaligned", {
  models_dir <- setup_iteration_model()

  # extract_params() drops an unannotated line, so THETA1's ';LOG' would
  # otherwise be read off THETA2's row and applied to the wrong column.
  writeLines(
    c(
      "$THETA",
      "0.5",
      "2.0 ; V1 ; L ; RATIO",
      "5.0 ; KA ; 1/hr ; FIX",
      "$OMEGA",
      "0.09 ; IIV_CL ; ; LOG",
      "$SIGMA",
      "0.10 ; prop ; ; LOG"
    ),
    file.path(models_dir, "run1.ctl")
  )

  expect_warning(
    p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = TRUE),
    "shown as estimated"
  )

  expect_equal(facet_values(p, "THETA1"), c(1.0, 1.1, 1.2))
  expect_false(any(grepl("natural", levels(p$data$variable), fixed = TRUE)))
  expect_equal(
    facet_values(p, "OMEGA.1.1. [CV%]"),
    100 * sqrt(exp(c(0.09, 0.16, 0.25)) - 1)
  )
})


test_that("an unreadable control stream warns and falls back to untransformed thetas", {
  models_dir <- setup_iteration_model()
  file.remove(file.path(models_dir, "run1.ctl"))

  expect_warning(
    p <- plot_nonmem_iterations("run1", models_dir = models_dir, transform = TRUE),
    "could not be read"
  )

  # THETA1 stays as estimated, but the variance rule needs no control stream.
  expect_equal(facet_values(p, "THETA1"), c(1.0, 1.1, 1.2))
  expect_equal(
    facet_values(p, "OMEGA.1.1. [CV%]"),
    100 * sqrt(exp(c(0.09, 0.16, 0.25)) - 1)
  )
})
