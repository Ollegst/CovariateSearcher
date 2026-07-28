test_that("a FIXED initial estimate is recognised in every written form", {
  expect_true(.theta_init_is_fixed("1 FIX"))
  expect_true(.theta_init_is_fixed("0.75 FIXED"))
  expect_true(.theta_init_is_fixed("(0, 0.75, 3) FIX"))
  expect_false(.theta_init_is_fixed("0.1"))
  expect_false(.theta_init_is_fixed("(0, 0.1, 3)"))
})

test_that("an unbounded initial estimate keeps the sign-flip behaviour", {
  expect_equal(.perturb_theta_init("0.1"), "-0.1")
  expect_equal(.perturb_theta_init("-0.1"), "0.1")
  expect_equal(.perturb_theta_init("1"), "-1")
  expect_equal(.perturb_theta_init("-2.5"), "2.5")
})

test_that("a bounded initial estimate moves to the midpoint of the wider side", {
  expect_equal(.perturb_theta_init("(0, 0.1, 3)"), "(0, 1.55, 3)")
  expect_equal(.perturb_theta_init("(-2, 0.5, 2)"), "(-2, -0.75, 2)")
  expect_equal(.perturb_theta_init("(0,0.1,3)"), "(0, 1.55, 3)")
})

test_that("the perturbed value stays strictly inside the bounds", {
  for (spec in c("(0, 0.1, 3)", "(-2, 0.5, 2)", "(0, 0.5, 1)", "(0, 0, 3)")) {
    bounds <- as.numeric(strsplit(gsub("[()]", "", spec), ",")[[1]])
    new_init <- as.numeric(strsplit(gsub("[()]", "", .perturb_theta_init(spec)), ",")[[1]])[2]
    expect_gt(new_init, bounds[1])
    expect_lt(new_init, bounds[3])
  }
})

test_that("a one-sided bound flips only when the flip stays legal", {
  # -0.1 would sit below the lower bound 0 -> midpoint of the bounded side
  expect_equal(.perturb_theta_init("(0, 0.1)"), "(0, 0.05)")
  # -0.5 is still above the lower bound -1 -> plain flip
  expect_equal(.perturb_theta_init("(-1, 0.5)"), "(-1, -0.5)")
})

test_that("a range with no room to move returns NA", {
  expect_true(is.na(.perturb_theta_init("(0.001, 0.001, 0.001)")))
  expect_true(is.na(.perturb_theta_init("")))
  expect_true(is.na(.perturb_theta_init("BLOCK")))
})

test_that("the new value is rendered without losing precision", {
  expect_equal(.format_theta_init_value(-0.1, "0.1"), "-0.1")
  expect_equal(.format_theta_init_value(-1, "1"), "-1")
  # the midpoint needs more decimals than the value it replaces
  expect_equal(.format_theta_init_value(0.05, "0.1"), "0.05")
  expect_equal(.format_theta_init_value(1.55, "0.1"), "1.55")
})
