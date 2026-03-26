# Tests for pls_seed_data.R helpers that do NOT need neuroim2

# ---- .drop_zero_var_columns() ----

test_that(".drop_zero_var_columns keeps all columns when no zero-variance", {
  set.seed(42)
  x <- matrix(rnorm(30), nrow = 5, ncol = 6)
  result <- plsrri:::.drop_zero_var_columns(x)
  expect_equal(dim(result), c(5, 6))
})

test_that(".drop_zero_var_columns drops constant columns", {
  set.seed(42)
  x <- matrix(rnorm(20), nrow = 5, ncol = 4)
  x[, 2] <- 5.0   # constant column
  x[, 4] <- 0.0   # constant column
  expect_message(
    result <- plsrri:::.drop_zero_var_columns(x),
    "zero/NA variance"
  )
  expect_equal(ncol(result), 2L)
})

test_that(".drop_zero_var_columns returns unchanged matrix when ncol=0", {
  x <- matrix(numeric(0), nrow = 5, ncol = 0)
  result <- plsrri:::.drop_zero_var_columns(x)
  expect_equal(dim(result), c(5, 0))
})

test_that(".drop_zero_var_columns returns input unchanged when not a matrix", {
  v <- c(1, 2, 3)
  result <- plsrri:::.drop_zero_var_columns(v)
  expect_identical(result, v)
})

test_that(".drop_zero_var_columns respects tol argument", {
  set.seed(42)
  # col 1: small but nonzero variance (values differ slightly)
  col1 <- c(0.001, 0.002, 0.001, 0.002, 0.001)  # var = 2.5e-7
  col2 <- rnorm(5) * 10                           # large variance
  x <- matrix(c(col1, col2), nrow = 5, ncol = 2)
  col1_var <- var(col1)
  expect_true(col1_var > 0)

  # with tol below col1 variance: both columns kept (no message)
  result_strict <- plsrri:::.drop_zero_var_columns(x, tol = 0)
  expect_equal(ncol(result_strict), 2L)

  # with tol above col1 variance: col1 dropped (message emitted)
  expect_message(
    result_loose <- plsrri:::.drop_zero_var_columns(x, tol = col1_var + 1e-10),
    "zero/NA variance"
  )
  expect_equal(ncol(result_loose), 1L)
})

# ---- .seed_colnames() ----

test_that(".seed_colnames returns roi_labels unchanged when lag_labels is NULL", {
  result <- plsrri:::.seed_colnames(c("ROI1", "ROI2"), lag_labels = NULL)
  expect_equal(result, c("ROI1", "ROI2"))
})

test_that(".seed_colnames returns roi_labels unchanged when lag_labels has length 1", {
  result <- plsrri:::.seed_colnames(c("ROI1", "ROI2"), lag_labels = 0L)
  expect_equal(result, c("ROI1", "ROI2"))
})

test_that(".seed_colnames generates crossed ROI x lag names", {
  result <- plsrri:::.seed_colnames(c("R1", "R2"), lag_labels = c(0L, 1L, 2L))
  expect_equal(length(result), 6L)
  expect_equal(result, c("R1_lag0", "R1_lag1", "R1_lag2",
                          "R2_lag0", "R2_lag1", "R2_lag2"))
})

test_that(".seed_colnames coerces roi and lag labels to character", {
  result <- plsrri:::.seed_colnames(1:2, lag_labels = c(0L, 1L))
  expect_equal(result, c("1_lag0", "1_lag1", "2_lag0", "2_lag1"))
})

# ---- .infer_seed_feature_layout() ----

# We need a mock mask-like object. We use a simple 3D array wrapped in a
# class attribute so the function's as.array() path is exercised.

make_mock_mask <- function(dims, n_nonzero = NULL) {
  arr <- array(0, dim = dims)
  if (is.null(n_nonzero)) n_nonzero <- prod(dims)
  arr[seq_len(n_nonzero)] <- 1
  # Minimal class so inherits() checks for NeuroVol pass if needed,
  # but .infer_seed_feature_layout() only calls as.array() and dim().
  structure(arr, class = c("mock_vol", "array"))
}

test_that(".infer_seed_feature_layout returns existing layout when kind is set", {
  existing <- list(kind = "voxel", n_voxels = 10L)
  dm <- list(matrix(1, 5, 10))
  mask <- make_mock_mask(c(2, 2, 3), n_nonzero = 10)
  result <- plsrri:::.infer_seed_feature_layout(dm, mask, feature_layout = existing)
  expect_identical(result, existing)
})

test_that(".infer_seed_feature_layout infers voxel layout when cols == n_voxels", {
  # 2x2x3 mask with all 12 voxels nonzero; data has 12 columns
  mask <- make_mock_mask(c(2, 2, 3))
  dm <- list(matrix(rnorm(5 * 12), 5, 12))
  result <- plsrri:::.infer_seed_feature_layout(dm, mask)
  expect_equal(result$kind, "voxel")
  expect_equal(result$n_voxels, 12L)
  expect_equal(result$n_lags, 1L)
})

test_that(".infer_seed_feature_layout infers voxel_lag layout when cols == n_voxels * n_lags", {
  # 4 voxels, 3 lags -> 12 columns
  mask <- make_mock_mask(c(2, 2, 1), n_nonzero = 4)
  dm <- list(matrix(rnorm(5 * 12), 5, 12))
  result <- plsrri:::.infer_seed_feature_layout(dm, mask)
  expect_equal(result$kind, "voxel_lag")
  expect_equal(result$n_voxels, 4L)
  expect_equal(result$n_lags, 3L)
  expect_equal(result$lag_labels, c(0L, 1L, 2L))
})

test_that(".infer_seed_feature_layout errors when cols not divisible by n_voxels", {
  mask <- make_mock_mask(c(2, 2, 1), n_nonzero = 4)
  dm <- list(matrix(rnorm(5 * 7), 5, 7))  # 7 cols, 4 voxels: not divisible
  expect_error(
    plsrri:::.infer_seed_feature_layout(dm, mask),
    "Cannot infer feature layout"
  )
})

test_that(".infer_seed_feature_layout errors when datamat_lst is not a list of matrices", {
  mask <- make_mock_mask(c(2, 2, 2))
  expect_error(
    plsrri:::.infer_seed_feature_layout(list("not_a_matrix"), mask),
    "non-empty list of matrices"
  )
})

test_that(".infer_seed_feature_layout errors when mask has zero voxels", {
  mask <- array(0, dim = c(2, 2, 2))  # all zeros
  dm <- list(matrix(1, 5, 8))
  expect_error(
    plsrri:::.infer_seed_feature_layout(dm, mask),
    "zero voxels"
  )
})

test_that(".infer_seed_feature_layout errors when groups have different column counts", {
  mask <- make_mock_mask(c(2, 2, 2))  # 8 voxels
  dm <- list(matrix(1, 5, 8), matrix(1, 5, 16))  # different cols
  expect_error(
    plsrri:::.infer_seed_feature_layout(dm, mask),
    "same number of columns"
  )
})
