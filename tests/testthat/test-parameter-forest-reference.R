# plot_parameter_forests(reference = ...) must normalise the plot to the same
# scenario its relative table uses.

test_that("the forest plot uses the chosen reference scenario as its denominator", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROB forest reference", "$THETA",
    "-0.69 ; CL ; ; LOG", "2.30 ; V ; ; LOG",
    "0.75 ; beta_WT_CL", "0.5 ; beta_SEXN_CL",
    "$OMEGA 0.09", "$PK",
    "TVCL = THETA(1)+THETA(3)*LOG(WT/70)",
    "IF (SEXN.EQ.2) TVCL = TVCL + THETA(4)",
    "CL = EXP(TVCL+ETA(1))",
    "TVV = THETA(2)", "V = EXP(TVV+ETA(2))"
  ), file.path(models_dir, "cat2.ctl"))

  set.seed(1)
  thetas <- data.frame(ID = 1:200, THETA1 = rnorm(200, -0.69, 0.1), THETA2 = 2.3,
                       THETA3 = 0.75, THETA4 = rnorm(200, 0.5, 0.1))
  dat <- data.frame(ID = 1:100, WT = seq(40, 120, length.out = 100), SEXN = rep(1:2, 50))
  cs <- data.frame(COVARIATE = c("WT", "SEXN"), PARAMETER = c("CL", "CL"),
                   STATUS = c("con", "cat"), REFERENCE = c(70, 1),
                   LEVELS = c(NA, "1;2"), FORMULA = c("power", "linear"),
                   stringsAsFactors = FALSE)
  ps <- suppressWarnings(build_scenario_parameters("cat2", cs, thetas, dat,
                                                   models_folder = models_dir,
                                                   scenario_table_path = NULL))
  ref <- grep("SEXN", names(ps), value = TRUE)[1]

  out_dir <- tempfile("plots")
  plots <- suppressWarnings(plot_parameter_forests(
    ps, "cat2", output_plot_folder = out_dir, output_format = "png",
    combined_pdf = FALSE, models_folder = models_dir, verbose = FALSE,
    reference = ref
  ))

  # The reference scenario's ratio to itself is 1 for every draw.
  built <- ggplot2::ggplot_build(plots$CL)
  box <- Filter(function(d) "middle" %in% names(d), built$data)[[1]]
  axis_labels <- built$layout$panel_params[[1]]$y$get_labels()
  expect_equal(box$middle[box$x == which(axis_labels == ref)], 1, tolerance = 1e-8)
})
