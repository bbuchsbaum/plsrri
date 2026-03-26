# Tests for statistical functions
#
# Tests percentile computation, confidence intervals, and p-value calculations.

# -----------------------------------------------------------------------------
# pls_percentile Tests
# -----------------------------------------------------------------------------

test_that("pls_percentile computes correct percentiles for vectors", {
  x <- 1:100

  expect_equal(pls_percentile(x, 50), 50.5, tolerance = 0.1)
  expect_equal(pls_percentile(x, 0), 1)
  expect_equal(pls_percentile(x, 100), 100)
})

test_that("pls_percentile handles multiple percentiles", {
  x <- 1:100

  result <- pls_percentile(x, c(25, 50, 75))

  expect_length(result, 3)
  expect_true(result[1] < result[2])
  expect_true(result[2] < result[3])
})

test_that("pls_percentile works with matrices", {
  # 100 samples x 3 variables
  set.seed(42)
  x <- matrix(rnorm(300), nrow = 100, ncol = 3)

  # Column percentiles (default)
  result_col <- pls_percentile(x, c(2.5, 97.5), margin = 1)
  expect_equal(nrow(result_col), 2)  # 2 percentiles
  expect_equal(ncol(result_col), 3)  # 3 variables

  # Row percentiles
  result_row <- pls_percentile(x, 50, margin = 2)
  expect_equal(length(result_row), 100)  # 100 rows
})

test_that("pls_percentile handles edge cases", {
  # Single element
  expect_equal(pls_percentile(5, 50), 5)

  # Two elements
  x <- c(1, 10)
  expect_true(pls_percentile(x, 50) >= 1 && pls_percentile(x, 50) <= 10)

  # With NA (should be excluded)
  x_na <- c(1:10, NA)
  expect_true(is.finite(pls_percentile(x_na, 50)))
})

test_that("pls_percentile matches quantile for standard cases", {
  set.seed(42)
  x <- rnorm(1000)

  # Compare with R's quantile (approximately)
  pls_p50 <- pls_percentile(x, 50)
  r_p50 <- quantile(x, 0.5, names = FALSE)

  expect_equal(pls_p50, r_p50, tolerance = 0.1)
})

# -----------------------------------------------------------------------------
# pls_boot_ci Tests
# -----------------------------------------------------------------------------

test_that("pls_boot_ci computes confidence intervals", {
  set.seed(42)

  # Note: pls_boot_ci expects (n_boot x n_elements) and uses margin=2 (row percentiles)
  # This means it computes percentiles across columns for each row
  # Result length = number of rows (bootstrap samples), not columns

  # Simulate bootstrap distribution: 100 samples x 5 elements
  boot_distrib <- matrix(rnorm(100 * 5), nrow = 100, ncol = 5)

  ci <- pls_boot_ci(boot_distrib, clim = 95)

  expect_true(is.list(ci))
  expect_equal(names(ci), c("lower", "upper", "clim"))
  expect_equal(ci$clim, 95)

  # The function uses margin=2 (row percentiles), returning one value per row
  expect_equal(length(ci$lower), nrow(boot_distrib))
  expect_equal(length(ci$upper), nrow(boot_distrib))
})

test_that("pls_boot_ci returns lower < upper", {
  set.seed(42)

  # With margin=2 behavior, test that lower is less than upper
  boot_distrib <- matrix(rnorm(50 * 10, mean = 0, sd = 1), nrow = 50, ncol = 10)

  ci <- pls_boot_ci(boot_distrib, clim = 95)

  # For most rows, lower should be less than upper
  # (unless the row has no variance, which is unlikely with random data)
  expect_true(mean(ci$lower < ci$upper) > 0.95)
})

# -----------------------------------------------------------------------------
# pls_pvalue Tests
# -----------------------------------------------------------------------------

test_that("pls_pvalue computes upper-tail p-values", {
  observed <- c(2, 3, 4)

  # Null distribution where observed is extreme
  null_distrib <- matrix(rnorm(1000 * 3, mean = 0, sd = 1), nrow = 1000, ncol = 3)

  p <- pls_pvalue(observed, null_distrib, tail = "upper")

  expect_equal(length(p), 3)
  expect_true(all(p >= 0 & p <= 1))

  # Larger observed values should have smaller upper-tail p-values
  expect_true(p[3] < p[1],
              info = "Larger observed should have smaller upper-tail p")
})

test_that("pls_pvalue computes lower-tail p-values", {
  observed <- c(-2, -3, -4)

  null_distrib <- matrix(rnorm(1000 * 3, mean = 0, sd = 1), nrow = 1000, ncol = 3)

  p <- pls_pvalue(observed, null_distrib, tail = "lower")

  expect_equal(length(p), 3)
  # More negative observed should have smaller lower-tail p-values
  expect_true(p[3] < p[1])
})

test_that("pls_pvalue computes two-tailed p-values", {
  observed <- c(0, 2, -2)

  null_distrib <- matrix(rnorm(1000 * 3, mean = 0, sd = 1), nrow = 1000, ncol = 3)

  p <- pls_pvalue(observed, null_distrib, tail = "two")

  expect_equal(length(p), 3)

  # Observed value at mean (0) should have larger p-value
  expect_true(p[1] > p[2],
              info = "Observed at mean should have larger two-tailed p")
  expect_true(p[1] > p[3],
              info = "Observed at mean should have larger two-tailed p")
})

test_that("pls_pvalue returns extreme values correctly", {
  observed <- c(100)  # Very extreme

  null_distrib <- matrix(rnorm(1000, mean = 0, sd = 1), nrow = 1000, ncol = 1)

  p_upper <- pls_pvalue(observed, null_distrib, tail = "upper")

  # Should be 0 or very small
  expect_true(p_upper <= 0.001,
              info = "Extreme observed should have p near 0")

  # P-value should never exceed 1
  expect_true(p_upper <= 1)
})

test_that("pls_pvalue requires matching dimensions", {
  observed <- c(1, 2, 3)
  null_distrib <- matrix(rnorm(100 * 2), nrow = 100, ncol = 2)  # Wrong columns

  expect_error(
    pls_pvalue(observed, null_distrib),
    "columns must match"
  )
})

# -----------------------------------------------------------------------------
# pls_is_low_variability Tests
# -----------------------------------------------------------------------------

test_that("pls_is_low_variability detects constant columns", {
  # Matrix with one constant column
  behavdata <- matrix(c(
    1, 2, 3,
    1, 4, 5,
    1, 6, 7,
    1, 8, 9
  ), nrow = 4, ncol = 3, byrow = TRUE)

  expect_true(pls_is_low_variability(behavdata),
              info = "Should detect constant first column")
})

test_that("pls_is_low_variability returns FALSE for variable data", {
  set.seed(42)
  behavdata <- matrix(rnorm(100), nrow = 25, ncol = 4)

  expect_false(pls_is_low_variability(behavdata),
               info = "Normal random data should have variability")
})

test_that("pls_is_low_variability respects threshold", {
  # Very low variability but not zero
  behavdata <- matrix(c(
    1.0000, 2,
    1.0001, 3,
    1.0002, 4,
    1.0003, 5
  ), nrow = 4, ncol = 2, byrow = TRUE)

  # With default threshold (1e-10), first column has variability
  expect_false(pls_is_low_variability(behavdata, threshold = 1e-10))

  # With higher threshold, it's considered low variability
  expect_true(pls_is_low_variability(behavdata, threshold = 0.01))
})

# -----------------------------------------------------------------------------
# Integration Tests: Statistics with PLS Results
# -----------------------------------------------------------------------------

test_that("significance() uses pls_pvalue correctly", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_perm = 100,
    progress = FALSE
  )

  pvals <- significance(result)

  # Should return p-values for each LV
  expect_equal(length(pvals), n_lv(result))
  expect_true(all(pvals >= 0 & pvals <= 1))
})

test_that("confidence() returns salience CI information", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_boot = 50,
    progress = FALSE
  )

  # confidence() doesn't take clim - uses what was set in bootstrap
  ci <- confidence(result, lv = 1)

  expect_true(is.list(ci) || is.matrix(ci) || is.numeric(ci))
})
