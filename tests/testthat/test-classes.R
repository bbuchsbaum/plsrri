# Tests for R/classes.R S3 class constructors and methods
#
# Note: print methods that use cli:: write to the message stream, so we
# capture them with capture.output(..., type = "message").
# summary.pls_result uses cat() and goes to stdout, so plain capture.output() works.

.cli_capture <- function(expr) {
  out <- capture.output(expr, type = "message")
  paste(out, collapse = "\n")
}

# ---------------------------------------------------------------------------
# pls_method_int_to_name()
# ---------------------------------------------------------------------------

test_that("pls_method_int_to_name maps all 6 valid integers", {
  expect_equal(pls_method_int_to_name(1L), "task")
  expect_equal(pls_method_int_to_name(2L), "task_nonrotated")
  expect_equal(pls_method_int_to_name(3L), "behavior")
  expect_equal(pls_method_int_to_name(4L), "multiblock")
  expect_equal(pls_method_int_to_name(5L), "behavior_nonrotated")
  expect_equal(pls_method_int_to_name(6L), "multiblock_nonrotated")
})

test_that("pls_method_int_to_name accepts numeric (non-integer) input", {
  expect_equal(pls_method_int_to_name(1), "task")
  expect_equal(pls_method_int_to_name(3), "behavior")
})

test_that("pls_method_int_to_name errors on 0 and 7", {
  expect_error(pls_method_int_to_name(0L), "Unknown PLS method")
  expect_error(pls_method_int_to_name(7L), "Unknown PLS method")
})

test_that("pls_method_int_to_name errors on NA", {
  expect_error(pls_method_int_to_name(NA_integer_), "Unknown PLS method")
})

# ---------------------------------------------------------------------------
# pls_method_label()
# ---------------------------------------------------------------------------

test_that("pls_method_label returns correct labels for integers 1-6", {
  expect_equal(pls_method_label(1L), "Mean-Centering Task PLS")
  expect_equal(pls_method_label(2L), "Non-Rotated Task PLS")
  expect_equal(pls_method_label(3L), "Regular Behavior PLS")
  expect_equal(pls_method_label(4L), "Regular Multiblock PLS")
  expect_equal(pls_method_label(5L), "Non-Rotated Behavior PLS")
  expect_equal(pls_method_label(6L), "Non-Rotated Multiblock PLS")
})

test_that("pls_method_label returns correct labels for canonical string names", {
  expect_equal(pls_method_label("task"),                  "Mean-Centering Task PLS")
  expect_equal(pls_method_label("task_nonrotated"),       "Non-Rotated Task PLS")
  expect_equal(pls_method_label("behavior"),              "Regular Behavior PLS")
  expect_equal(pls_method_label("multiblock"),            "Regular Multiblock PLS")
  expect_equal(pls_method_label("behavior_nonrotated"),   "Non-Rotated Behavior PLS")
  expect_equal(pls_method_label("multiblock_nonrotated"), "Non-Rotated Multiblock PLS")
})

test_that("pls_method_label returns 'Unknown' for invalid string", {
  expect_equal(pls_method_label("not_a_method"),  "Unknown")
  expect_equal(pls_method_label("totally_bogus"), "Unknown")
})

# ---------------------------------------------------------------------------
# pls_spec() constructor
# ---------------------------------------------------------------------------

test_that("pls_spec() creates an object with correct class and defaults", {
  spec <- pls_spec()

  expect_s3_class(spec, "pls_spec")
  expect_null(spec$mask)
  expect_equal(spec$datamat_lst, list())
  expect_equal(spec$num_subj_lst, integer(0))
  expect_null(spec$num_cond)
  expect_null(spec$stacked_behavdata)
  expect_null(spec$stacked_designdata)
  expect_equal(spec$method, 1L)
  expect_equal(spec$num_perm, 0L)
  expect_equal(spec$num_boot, 0L)
  expect_equal(spec$num_split, 0L)
  expect_equal(spec$clim, 95)
  expect_false(spec$is_struct)
  expect_null(spec$bscan)
  expect_equal(spec$meancentering_type, 0L)
  expect_equal(spec$cormode, 0L)
  expect_equal(spec$boot_type, "strat")
  expect_null(spec$groups)
  expect_null(spec$conditions)
  expect_false(spec$.trial_raw)
})

# ---------------------------------------------------------------------------
# print.pls_spec()
# ---------------------------------------------------------------------------

test_that("print.pls_spec() outputs expected strings for empty spec", {
  spec     <- pls_spec()
  combined <- .cli_capture(print(spec))

  expect_match(combined, "PLS Specification",       fixed = TRUE)
  expect_match(combined, "Mean-Centering Task PLS", fixed = TRUE)
  expect_match(combined, "Permutations",            fixed = TRUE)
  expect_match(combined, "Bootstraps",              fixed = TRUE)
  expect_match(combined, "No data loaded",          fixed = TRUE)
})

test_that("print.pls_spec() shows group/observation/voxel counts when data is loaded", {
  set.seed(42)
  spec <- pls_spec()
  spec$datamat_lst  <- list(matrix(rnorm(12 * 20), 12, 20))
  spec$num_subj_lst <- 6L
  spec$num_cond     <- 2L

  combined <- .cli_capture(print(spec))

  expect_match(combined, "Groups",             fixed = TRUE)
  expect_match(combined, "Total observations", fixed = TRUE)
  expect_match(combined, "Voxels",             fixed = TRUE)
  expect_match(combined, "Conditions",         fixed = TRUE)
})

test_that("print.pls_spec() returns the object invisibly", {
  spec <- pls_spec()
  # Suppress cli output; we only check the return value
  capture.output(rv <- withVisible(print(spec)), type = "message")
  expect_false(rv$visible)
  expect_identical(rv$value, spec)
})

# ---------------------------------------------------------------------------
# Helpers: build a minimal pls_result via quick_pls
# ---------------------------------------------------------------------------

.make_small_result <- function(nperm = 0, nboot = 0) {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 2L
  n_vox   <- 20L
  datamat <- matrix(rnorm(n_subj * n_cond * n_vox), n_subj * n_cond, n_vox)
  quick_pls(
    list(datamat),
    num_subj_lst = n_subj,
    num_cond     = n_cond,
    nperm        = nperm,
    nboot        = nboot,
    progress     = FALSE
  )
}

# ---------------------------------------------------------------------------
# print.pls_result()
# ---------------------------------------------------------------------------

test_that("print.pls_result() outputs expected strings", {
  result   <- .make_small_result()
  combined <- .cli_capture(print(result))

  expect_match(combined, "PLS Result",       fixed = TRUE)
  expect_match(combined, "Latent Variables", fixed = TRUE)
})

test_that("print.pls_result() mentions perm info when perm_result present", {
  result   <- .make_small_result(nperm = 5)
  combined <- .cli_capture(print(result))

  expect_match(combined, "Significant LVs", fixed = TRUE)
})

test_that("print.pls_result() mentions boot info when boot_result present", {
  result   <- .make_small_result(nboot = 5)
  combined <- .cli_capture(print(result))

  expect_match(combined, "Bootstrap samples", fixed = TRUE)
  expect_match(combined, "Confidence level",  fixed = TRUE)
})

test_that("print.pls_result() returns the object invisibly", {
  result <- .make_small_result()
  capture.output(rv <- withVisible(print(result)), type = "message")
  expect_false(rv$visible)
  expect_identical(rv$value, result)
})

# ---------------------------------------------------------------------------
# summary.pls_result()  -- uses cat(), captured on stdout
# ---------------------------------------------------------------------------

test_that("summary.pls_result() outputs variance explained table", {
  result   <- .make_small_result()
  combined <- paste(capture.output(summary(result)), collapse = "\n")

  expect_match(combined, "PLS Analysis Summary", fixed = TRUE)
  expect_match(combined, "Variance",             fixed = TRUE)
  expect_match(combined, "SingularValue",        fixed = TRUE)
})

test_that("summary.pls_result() includes p-values when perm_result present", {
  result   <- .make_small_result(nperm = 5)
  combined <- paste(capture.output(summary(result)), collapse = "\n")

  expect_match(combined, "pvalue", fixed = TRUE)
})

test_that("summary.pls_result() returns object invisibly", {
  result <- .make_small_result()
  rv     <- withVisible(summary(result))
  expect_false(rv$visible)
  expect_identical(rv$value, result)
})

# ---------------------------------------------------------------------------
# print.pls_perm_result()
# ---------------------------------------------------------------------------

test_that("print.pls_perm_result() outputs expected strings", {
  result <- .make_small_result(nperm = 5)
  expect_s3_class(result$perm_result, "pls_perm_result")

  # cli header goes to message stream; print(df) goes to stdout — capture both
  cli_out  <- .cli_capture(stdout_out <- capture.output(print(result$perm_result)))
  combined <- paste(c(cli_out, stdout_out), collapse = "\n")

  expect_match(combined, "Permutation", fixed = TRUE)
  expect_match(combined, "pvalue",      fixed = TRUE)
  expect_match(combined, "LV",          fixed = TRUE)
})

test_that("print.pls_perm_result() returns object invisibly", {
  result <- .make_small_result(nperm = 5)
  capture.output(rv <- withVisible(print(result$perm_result)), type = "message")
  expect_false(rv$visible)
  expect_identical(rv$value, result$perm_result)
})

# ---------------------------------------------------------------------------
# print.pls_boot_result()
# ---------------------------------------------------------------------------

test_that("print.pls_boot_result() outputs expected strings", {
  result <- .make_small_result(nboot = 5)
  expect_s3_class(result$boot_result, "pls_boot_result")

  combined <- .cli_capture(print(result$boot_result))

  expect_match(combined, "Bootstrap",         fixed = TRUE)
  expect_match(combined, "Bootstrap samples", fixed = TRUE)
  expect_match(combined, "Confidence level",  fixed = TRUE)
})

test_that("print.pls_boot_result() mentions bootstrap ratios when compare_u present", {
  result   <- .make_small_result(nboot = 5)
  combined <- .cli_capture(print(result$boot_result))

  expect_match(combined, "Bootstrap ratios", fixed = TRUE)
})

test_that("print.pls_boot_result() returns object invisibly", {
  result <- .make_small_result(nboot = 5)
  capture.output(rv <- withVisible(print(result$boot_result)), type = "message")
  expect_false(rv$visible)
  expect_identical(rv$value, result$boot_result)
})
