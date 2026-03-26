# Tests for builder_config.R internal helpers and set_parallel()

# ---- .resolve_method() ----

test_that(".resolve_method returns correct integers for string aliases", {
  expect_equal(plsrri:::.resolve_method("task"),                  1L)
  expect_equal(plsrri:::.resolve_method("mean_centering"),        1L)
  expect_equal(plsrri:::.resolve_method("meancentering"),         1L)
  expect_equal(plsrri:::.resolve_method("task_nonrotated"),       2L)
  expect_equal(plsrri:::.resolve_method("nonrotated_task"),       2L)
  expect_equal(plsrri:::.resolve_method("behavior"),              3L)
  expect_equal(plsrri:::.resolve_method("behaviour"),             3L)
  expect_equal(plsrri:::.resolve_method("multiblock"),            4L)
  expect_equal(plsrri:::.resolve_method("behavior_nonrotated"),   5L)
  expect_equal(plsrri:::.resolve_method("behaviour_nonrotated"),  5L)
  expect_equal(plsrri:::.resolve_method("nonrotated_behavior"),   5L)
  expect_equal(plsrri:::.resolve_method("multiblock_nonrotated"), 6L)
  expect_equal(plsrri:::.resolve_method("nonrotated_multiblock"), 6L)
})

test_that(".resolve_method returns correct integers for integer inputs", {
  for (i in 1:6) {
    expect_equal(plsrri:::.resolve_method(i), as.integer(i))
  }
})

test_that(".resolve_method errors on unknown string", {
  expect_error(plsrri:::.resolve_method("unknown_method"), "Unknown method")
})

test_that(".resolve_method errors on out-of-range integer", {
  expect_error(plsrri:::.resolve_method(0), "method must be 1-6")
  expect_error(plsrri:::.resolve_method(7), "method must be 1-6")
})

test_that(".resolve_method errors on invalid type", {
  expect_error(plsrri:::.resolve_method(TRUE),   "method must be character or integer")
  expect_error(plsrri:::.resolve_method(list()),  "method must be character or integer")
})

# ---- .resolve_meancentering() ----

test_that(".resolve_meancentering returns correct integers for string aliases", {
  expect_equal(plsrri:::.resolve_meancentering("within_group"),    0L)
  expect_equal(plsrri:::.resolve_meancentering("within"),          0L)
  expect_equal(plsrri:::.resolve_meancentering("grand_condition"), 1L)
  expect_equal(plsrri:::.resolve_meancentering("condition"),       1L)
  expect_equal(plsrri:::.resolve_meancentering("grand_mean"),      2L)
  expect_equal(plsrri:::.resolve_meancentering("grand"),           2L)
  expect_equal(plsrri:::.resolve_meancentering("all_effects"),     3L)
  expect_equal(plsrri:::.resolve_meancentering("interaction"),     3L)
})

test_that(".resolve_meancentering returns correct integers for integer inputs", {
  for (i in 0:3) {
    expect_equal(plsrri:::.resolve_meancentering(i), as.integer(i))
  }
})

test_that(".resolve_meancentering errors on unknown string", {
  expect_error(plsrri:::.resolve_meancentering("bad_type"), "Unknown meancentering type")
})

test_that(".resolve_meancentering errors on out-of-range integer", {
  expect_error(plsrri:::.resolve_meancentering(-1), "meancentering must be 0-3")
  expect_error(plsrri:::.resolve_meancentering(4),  "meancentering must be 0-3")
})

test_that(".resolve_meancentering errors on invalid type", {
  expect_error(plsrri:::.resolve_meancentering(TRUE), "meancentering must be character or integer")
})

# ---- .resolve_cormode() ----

test_that(".resolve_cormode returns correct integers for string aliases", {
  expect_equal(plsrri:::.resolve_cormode("pearson"),     0L)
  expect_equal(plsrri:::.resolve_cormode("correlation"), 0L)
  expect_equal(plsrri:::.resolve_cormode("covariance"),  2L)
  expect_equal(plsrri:::.resolve_cormode("cov"),         2L)
  expect_equal(plsrri:::.resolve_cormode("cosine"),      4L)
  expect_equal(plsrri:::.resolve_cormode("dot"),         6L)
  expect_equal(plsrri:::.resolve_cormode("dot_product"), 6L)
})

test_that(".resolve_cormode returns correct integers for valid integer inputs", {
  expect_equal(plsrri:::.resolve_cormode(0), 0L)
  expect_equal(plsrri:::.resolve_cormode(2), 2L)
  expect_equal(plsrri:::.resolve_cormode(4), 4L)
  expect_equal(plsrri:::.resolve_cormode(6), 6L)
})

test_that(".resolve_cormode errors on invalid integer", {
  expect_error(plsrri:::.resolve_cormode(1), "cormode must be 0, 2, 4, or 6")
  expect_error(plsrri:::.resolve_cormode(3), "cormode must be 0, 2, 4, or 6")
  expect_error(plsrri:::.resolve_cormode(5), "cormode must be 0, 2, 4, or 6")
})

test_that(".resolve_cormode errors on unknown string", {
  expect_error(plsrri:::.resolve_cormode("euclidean"), "Unknown cormode")
})

test_that(".resolve_cormode errors on invalid type", {
  expect_error(plsrri:::.resolve_cormode(TRUE), "cormode must be character or integer")
})

# ---- .validate_method_requirements() ----
# cli::cli_alert_warning emits messages (not R warnings)

test_that(".validate_method_requirements messages when behavior data missing for method 3", {
  spec <- pls_spec()
  spec$method <- 3L
  spec$stacked_behavdata <- NULL
  expect_message(
    plsrri:::.validate_method_requirements(spec),
    "requires behavior data"
  )
})

test_that(".validate_method_requirements messages when behavior data missing for methods 4, 5, 6", {
  for (m in c(4L, 5L, 6L)) {
    spec <- pls_spec()
    spec$method <- m
    spec$stacked_behavdata <- NULL
    expect_message(
      plsrri:::.validate_method_requirements(spec),
      "requires behavior data"
    )
  }
})

test_that(".validate_method_requirements messages when design missing for method 2", {
  spec <- pls_spec()
  spec$method <- 2L
  spec$stacked_designdata <- NULL
  spec$.trial_raw <- NULL
  spec$trial_data <- NULL
  expect_message(
    plsrri:::.validate_method_requirements(spec),
    "requires design contrasts"
  )
})

test_that(".validate_method_requirements is silent for task method (1) with no data", {
  spec <- pls_spec()
  spec$method <- 1L
  expect_silent(plsrri:::.validate_method_requirements(spec))
})

# ---- set_parallel() ----

test_that("set_parallel sets backend and auto-detects workers", {
  spec <- pls_spec()
  spec2 <- set_parallel(spec)

  expect_true(is.list(spec2$.parallel))
  expect_equal(spec2$.parallel$backend, "future")
  expect_true(is.numeric(spec2$.parallel$workers))
  expect_true(spec2$.parallel$workers >= 1)
})

test_that("set_parallel accepts explicit workers count", {
  spec <- pls_spec()
  spec2 <- set_parallel(spec, workers = 4, backend = "future")

  expect_equal(spec2$.parallel$workers, 4)
  expect_equal(spec2$.parallel$backend, "future")
})

test_that("set_parallel accepts sequential backend", {
  spec <- pls_spec()
  spec2 <- set_parallel(spec, workers = 1, backend = "sequential")

  expect_equal(spec2$.parallel$backend, "sequential")
  expect_equal(spec2$.parallel$workers, 1)
})

test_that("set_parallel errors on invalid backend", {
  spec <- pls_spec()
  expect_error(set_parallel(spec, backend = "bad"))
})

test_that("set_parallel errors on non-pls_spec input", {
  expect_error(set_parallel(list()), "pls_spec")
})
