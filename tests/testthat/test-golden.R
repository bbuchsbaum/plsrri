# Golden tests for computational correctness
#
# These tests verify mathematical properties that must hold for correct PLS
# implementations. They serve as regression tests and algorithmic verification.
#
# Key properties tested:
# 1. SVD properties (orthogonality, reconstruction)
# 2. Mean-centering correctness
# 3. Score computation consistency
# 4. Bootstrap ratio properties
# 5. Variance explained summation

# -----------------------------------------------------------------------------
# SVD Properties Tests
# -----------------------------------------------------------------------------

test_that("SVD components are orthonormal", {
  set.seed(42)

  datamat1 <- matrix(rnorm(60 * 100), 60, 100)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 20,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  u <- result$u
  v <- result$v

  # u columns should be orthonormal
  u_inner <- crossprod(u)
  expect_equal(diag(u_inner), rep(1, ncol(u)), tolerance = 1e-10,
               info = "u columns should have unit norm")

  # Off-diagonal should be near zero (orthogonal)
  off_diag <- u_inner - diag(diag(u_inner))
  expect_true(all(abs(off_diag) < 1e-10),
              info = "u columns should be orthogonal")

  # v columns should be orthonormal
  v_inner <- crossprod(v)
  expect_equal(diag(v_inner), rep(1, ncol(v)), tolerance = 1e-10,
               info = "v columns should have unit norm")

  off_diag_v <- v_inner - diag(diag(v_inner))
  expect_true(all(abs(off_diag_v) < 1e-10),
              info = "v columns should be orthogonal")
})

test_that("singular values are positive and sorted", {
  set.seed(42)

  datamat1 <- matrix(rnorm(60 * 100), 60, 100)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 20,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  s <- result$s

  expect_true(all(s >= 0), info = "Singular values must be non-negative")
  expect_equal(s, sort(s, decreasing = TRUE),
               info = "Singular values must be sorted descending")
})

test_that("SVD reconstruction approximates original cross-product", {
  set.seed(42)

  n_subj <- 15
  n_cond <- 3
  n_features <- 50

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 1,
    progress = FALSE
  )

  # Reconstruct the cross-product matrix
  # For task PLS: crossblock = design' * data (after mean-centering)
  u <- result$u
  s <- result$s
  v <- result$v

  # u * diag(s) * v' should approximate the original cross-product
  reconstructed <- u %*% diag(s) %*% t(v)

  # Get original cross-product
  covcor <- pls_get_covcor(
    method = 1,
    stacked_datamat = datamat1,
    num_groups = 1,
    num_subj_lst = n_subj,
    num_cond = n_cond,
    meancentering_type = 0
  )

  # The reconstruction should match the original
  expect_equal(reconstructed, t(covcor$datamatsvd), tolerance = 1e-10,
               info = "SVD should reconstruct the original cross-product")
})

# -----------------------------------------------------------------------------
# Mean-Centering Tests
# -----------------------------------------------------------------------------

test_that("mean-centering type 0 (grand mean) centers correctly", {
  set.seed(42)

  n_subj <- 10
  n_cond <- 3
  n_features <- 20

  # Create data with known mean
  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features, mean = 100), n_subj * n_cond, n_features)

  # Apply mean-centering using list interface
  centered <- pls_meancentering(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    meancentering_type = 0
  )

  # Grand mean should be approximately zero for each feature
  centered_data <- centered$centered[[1]]
  grand_mean <- colMeans(centered_data)
  expect_true(all(abs(grand_mean) < 1e-10))
})

test_that("mean-centering type 1 removes grand condition mean", {
  set.seed(42)

  n_subj <- c(8, 12)  # Two groups
  n_cond <- 2
  n_features <- 15

  datamat1 <- matrix(rnorm(n_subj[1] * n_cond * n_features, mean = 50), n_subj[1] * n_cond, n_features)
  datamat2 <- matrix(rnorm(n_subj[2] * n_cond * n_features, mean = 100), n_subj[2] * n_cond, n_features)

  centered <- pls_meancentering(
    datamat_lst = list(datamat1, datamat2),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    meancentering_type = 1
  )

  # pls_meancentering returns condition means, not full data
  # For type 1, it removes grand condition mean from each group's condition means
  # So the average across groups for each condition should be zero

  # Average the two groups' condition means
  combined <- (centered$centered[[1]] + centered$centered[[2]]) / 2
  combined_mean <- colMeans(combined)

  expect_true(all(abs(combined_mean) < 1e-10))
})

# -----------------------------------------------------------------------------
# Score Computation Tests
# -----------------------------------------------------------------------------

test_that("brain scores have correct dimensions", {
  set.seed(42)

  n_subj <- 10
  n_cond <- 3
  n_obs <- n_subj * n_cond

  datamat1 <- matrix(rnorm(n_obs * 50), n_obs, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 1,
    progress = FALSE
  )

  # Brain scores should have one row per observation
  stored_brain_scores <- result$usc

  expect_equal(nrow(stored_brain_scores), n_obs)
  expect_equal(ncol(stored_brain_scores), n_cond)  # One column per LV
})

test_that("design scores correlate with brain scores for first LV", {
  set.seed(42)

  datamat1 <- matrix(rnorm(60 * 100), 60, 100)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 20,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  brain_sc <- scores(result, type = "brain")
  design_sc <- scores(result, type = "design")

  # For task PLS, first LV design and brain scores should be highly correlated
  # Later LVs may have weaker relationships with random data
  cor_val <- cor(brain_sc[, 1], design_sc[, 1])
  expect_true(abs(cor_val) > 0.5)
})

# -----------------------------------------------------------------------------
# Variance Explained Tests
# -----------------------------------------------------------------------------

test_that("variance explained sums to 100%", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  var_exp <- (result$s^2) / sum(result$s^2) * 100

  expect_equal(sum(var_exp), 100, tolerance = 1e-10,
               info = "Variance explained should sum to 100%")
})

test_that("variance_explained accessor matches manual calculation", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  accessor_var <- singular_values(result, normalize = TRUE)
  manual_var <- (result$s^2) / sum(result$s^2) * 100

  # Compare values (ignore names)
  expect_equal(as.numeric(accessor_var), as.numeric(manual_var), tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# Bootstrap Ratio Tests
# -----------------------------------------------------------------------------

test_that("bootstrap ratio has finite values", {
  set.seed(42)

  n_subj <- 15
  n_cond <- 3
  n_features <- 50

  # Create data with some structure
  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)

  # Add condition-specific signal to first features
  for (i in 1:n_subj) {
    row_indices <- (i - 1) * n_cond + 1:n_cond
    datamat1[row_indices[1], 1:10] <- datamat1[row_indices[1], 1:10] + 5
    datamat1[row_indices[2], 1:10] <- datamat1[row_indices[2], 1:10] - 5
  }

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 1,
    num_boot = 50,
    progress = FALSE
  )

  bsr_vals <- bsr(result, lv = 1)

  # BSR values should be finite
  expect_true(all(is.finite(bsr_vals)))

  # Some BSR values should be non-zero
  expect_true(any(abs(bsr_vals) > 0.1))
})

test_that("BSR = salience / SE", {
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

  # BSR should equal compare_u from boot_result
  bsr_accessor <- bsr(result, lv = 1)
  bsr_direct <- result$boot_result$compare_u[, 1]

  # Compare as vectors (bsr() may return matrix or vector)
  expect_equal(as.numeric(bsr_accessor), as.numeric(bsr_direct), tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# Permutation Test Properties
# -----------------------------------------------------------------------------

test_that("permutation p-values are bounded [0, 1]", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_perm = 50,
    progress = FALSE
  )

  pvals <- result$perm_result$sprob

  expect_true(all(pvals >= 0), info = "P-values should be >= 0")
  expect_true(all(pvals <= 1), info = "P-values should be <= 1")
})

test_that("permutation test produces valid p-values", {
  set.seed(123)

  n_subj <- 15
  n_cond <- 3
  n_features <- 40

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 1,
    num_perm = 50,
    progress = FALSE
  )

  pvals <- result$perm_result$sprob

  # All p-values should be valid (between 0 and 1)
  expect_true(all(pvals >= 0 & pvals <= 1))

  # Should have one p-value per LV
  expect_equal(length(pvals), n_lv(result))

  # Permutation count should match
  expect_equal(result$perm_result$num_perm, 50)
})

# -----------------------------------------------------------------------------
# Cross-Correlation Mode Tests
# -----------------------------------------------------------------------------

test_that("different correlation modes produce different results", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50, mean = 10, sd = 5), 30, 50)
  behavdata <- matrix(rnorm(30 * 2), 30, 2)

  # Pearson correlation (cormode = 0)
  result_cor <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 3,
    stacked_behavdata = behavdata,
    cormode = 0,
    progress = FALSE
  )

  # Covariance (cormode = 2)
  result_cov <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 3,
    stacked_behavdata = behavdata,
    cormode = 2,
    progress = FALSE
  )

  # Results should differ
  expect_false(isTRUE(all.equal(result_cor$s, result_cov$s)),
               info = "Different cormode should produce different singular values")
})

# -----------------------------------------------------------------------------
# Determinism Tests
# -----------------------------------------------------------------------------

test_that("analysis is deterministic without resampling", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result1 <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_perm = 0,
    num_boot = 0,
    progress = FALSE
  )

  result2 <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_perm = 0,
    num_boot = 0,
    progress = FALSE
  )

  expect_equal(result1$s, result2$s)
  expect_equal(result1$u, result2$u)
  expect_equal(result1$v, result2$v)
})

# -----------------------------------------------------------------------------
# Fixture-Based Golden Tests (for future MATLAB comparison)
# -----------------------------------------------------------------------------

# These tests would compare against pre-computed MATLAB results
# Placeholder for when fixtures are available

test_that("MATLAB comparison: task PLS singular values", {
  skip("MATLAB reference data not yet available")

  # When available:
  # matlab_result <- readRDS("fixtures/matlab_task_pls.rds")
  # expect_equal(result$s, matlab_result$s, tolerance = 1e-8)
})

test_that("MATLAB comparison: behavior PLS lvcorrs", {
  skip("MATLAB reference data not yet available")

  # When available:
  # matlab_result <- readRDS("fixtures/matlab_behav_pls.rds")
  # expect_equal(result$lvcorrs, matlab_result$lvcorrs, tolerance = 1e-8)
})
