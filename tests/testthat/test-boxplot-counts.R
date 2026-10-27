# Covariate boxplots: counts are subjects, bins are named for what they are,
# and the jittered points are reproducible.

build_boxplot <- function(dat, ...) {
  out <- tempfile("box")
  on.exit(unlink(out, recursive = TRUE))
  res <- create_covariate_boxplots(
    data = dat, spec = yspec::ys_help$spec(), con = "WT", type = "AUC",
    drug = "DrugX", param_info = list(AUC = list(label = "AUC", unit = "h")),
    total_panel = FALSE, output_folder = out, combined_pdf = FALSE,
    output_format = "png", verbose = FALSE, ...
  )
  res$AUC$WT
}

test_that("n counts subjects, not rows", {
  # SIM-10: three rows per subject were counted three times.
  set.seed(1)
  one <- data.frame(ID = 1:40, WT = runif(40, 50, 100), AUC = rlnorm(40, 7, 0.3))
  dat <- one[rep(seq_len(40), each = 3), ]
  p <- build_boxplot(dat)
  n_layer <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]
  expect_equal(sum(n_layer$data$Freq), 40)
})

test_that("tied quantiles do not produce 'Qk' labels", {
  dat <- data.frame(ID = 1:40, WT = rep(c(60, 60, 60, 90), 10), AUC = rlnorm(40, 7, 0.3))
  p <- build_boxplot(dat)
  lv <- levels(p$data$WT)
  expect_false(any(grepl("^Q[0-9]:", lv)))
})

test_that("the jitter has a fixed seed", {
  set.seed(2)
  dat <- data.frame(ID = 1:40, WT = runif(40, 50, 100), AUC = rlnorm(40, 7, 0.3))
  p <- build_boxplot(dat)
  jit <- Filter(function(l) inherits(l$geom, "GeomPoint"), p$layers)[[1]]
  expect_equal(jit$position$seed, 1)
})
