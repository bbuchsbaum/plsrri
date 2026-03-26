# Tests for all 6 PLS methods
#
# Ensures each method runs correctly and produces expected output structure.

# -----------------------------------------------------------------------------
# Method 1: Mean-Centering Task PLS (already tested in test-pls_analysis.R)
# -----------------------------------------------------------------------------

# (See test-pls_analysis.R for method 1 tests)

# -----------------------------------------------------------------------------
# Method 2: Non-Rotated Task PLS
# -----------------------------------------------------------------------------

test_that("method 2 (non-rotated task) requires design contrasts", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  expect_error(
    pls_analysis(
      datamat_lst = list(datamat1),
      num_subj_lst = 10,
      num_cond = 3,
      method = 2,
      progress = FALSE
    ),
    "stacked_designdata is required"
  )
})

test_that("method 2 (non-rotated task) works with design contrasts", {
  set.seed(42)

  n_subj <- 10
  n_cond <- 3
  n_features <- 50
  n_groups <- 1

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)

  # Design contrasts: num_groups * num_cond rows
  # e.g., condition 1 vs conditions 2+3
  design <- matrix(c(2, -1, -1), nrow = n_groups * n_cond, ncol = 1)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 2,
    stacked_designdata = design,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(result$method, 2L)
  expect_true(length(result$s) >= 1)
  expect_equal(nrow(result$u), n_features)
})

test_that("method 2 works with multiple contrasts", {
  set.seed(42)

  n_subj <- 10
  n_cond <- 3
  n_features <- 50
  n_groups <- 1

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)

  # Two orthogonal contrasts: num_groups * num_cond rows = 3
  design <- cbind(
    c(1, -1, 0),  # Condition 1 vs 2
    c(1, 1, -2)   # Conditions 1+2 vs 3
  )

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 2,
    stacked_designdata = design,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(ncol(result$u), 2)
})

# -----------------------------------------------------------------------------
# Method 3: Behavior PLS (already tested in test-pls_analysis.R)
# -----------------------------------------------------------------------------

# (See test-pls_analysis.R for method 3 tests)

# Additional method 3 tests
test_that("method 3 produces valid lvcorrs", {
  set.seed(42)

  n_subj <- 15
  n_cond <- 2
  n_features <- 50
  n_behav <- 3

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), n_subj * n_cond, n_behav)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 3,
    stacked_behavdata = behavdata,
    progress = FALSE
  )

  expect_s3_class(result, "pls_behavior")
  expect_true(!is.null(result$lvcorrs))
  # lvcorrs should be correlations between LV scores and behavior
  expect_true(all(abs(result$lvcorrs) <= 1))
})

test_that("method 3 works with different correlation modes", {
  set.seed(42)

  n_subj <- 12
  n_cond <- 2
  n_features <- 40
  n_behav <- 2

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), n_subj * n_cond, n_behav)

  # Test correlation mode (default)
  result_cor <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 3,
    stacked_behavdata = behavdata,
    cormode = 0,  # Pearson correlation
    progress = FALSE
  )

  expect_s3_class(result_cor, "pls_result")

  # Test covariance mode
  result_cov <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 3,
    stacked_behavdata = behavdata,
    cormode = 2,  # Covariance
    progress = FALSE
  )

  expect_s3_class(result_cov, "pls_result")

  # Results should differ
  expect_false(isTRUE(all.equal(result_cor$s, result_cov$s)))
})

# -----------------------------------------------------------------------------
# Method 4: Multiblock PLS
# -----------------------------------------------------------------------------

test_that("method 4 (multiblock) requires behavior data", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  expect_error(
    pls_analysis(
      datamat_lst = list(datamat1),
      num_subj_lst = 10,
      num_cond = 3,
      method = 4,
      progress = FALSE
    ),
    "stacked_behavdata is required"
  )
})

test_that("method 4 (multiblock) works with behavior data", {
  set.seed(42)

  n_subj <- 12
  n_cond <- 3
  n_features <- 50
  n_behav <- 2

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), n_subj * n_cond, n_behav)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 4,
    stacked_behavdata = behavdata,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_s3_class(result, "pls_multiblock")
  expect_equal(result$method, 4L)
  expect_true(!is.null(result$usc))
  expect_true(!is.null(result$vsc))
  expect_true(!is.null(result$TBv))
  expect_true(!is.null(result$TBusc))
  expect_true(!is.null(result$TBvsc))

  # Multiblock returns stacked task + behavior score blocks
  expect_equal(ncol(result$usc), length(result$s))
  expect_equal(ncol(result$vsc), length(result$s))
})

# -----------------------------------------------------------------------------
# Method 5: Non-Rotated Behavior PLS
# -----------------------------------------------------------------------------

test_that("method 5 (non-rotated behavior) requires design and behavior", {
  set.seed(42)

  n_subj <- 10
  n_cond <- 3
  n_features <- 50

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)
  behavdata <- matrix(rnorm(n_subj * n_cond * 2), n_subj * n_cond, 2)

  # Missing design
  expect_error(
    pls_analysis(
      datamat_lst = list(datamat1),
      num_subj_lst = n_subj,
      num_cond = n_cond,
      method = 5,
      stacked_behavdata = behavdata,
      progress = FALSE
    ),
    "stacked_designdata is required"
  )
})

test_that("method 5 (non-rotated behavior) works with design and behavior", {
  set.seed(42)

  n_subj <- 10
  n_cond <- 3
  n_features <- 50
  n_behav <- 2
  n_groups <- 1

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), n_subj * n_cond, n_behav)

  # Design for method 5: num_groups * num_cond * ncol(behavdata) rows = 1 * 3 * 2 = 6
  design <- matrix(c(1, -1, 0, 1, 0, -1), nrow = n_groups * n_cond * n_behav, ncol = 1)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 5,
    stacked_behavdata = behavdata,
    stacked_designdata = design,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(result$method, 5L)
})

# -----------------------------------------------------------------------------
# Method 6: Non-Rotated Multiblock PLS
# -----------------------------------------------------------------------------

test_that("method 6 (non-rotated multiblock) requires design and behavior", {
  set.seed(42)

  n_subj <- 10
  n_cond <- 3
  n_features <- 50

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)
  behavdata <- matrix(rnorm(n_subj * n_cond * 2), n_subj * n_cond, 2)

  # Missing design
  expect_error(
    pls_analysis(
      datamat_lst = list(datamat1),
      num_subj_lst = n_subj,
      num_cond = n_cond,
      method = 6,
      stacked_behavdata = behavdata,
      progress = FALSE
    ),
    "stacked_designdata is required"
  )
})

test_that("method 6 (non-rotated multiblock) works", {
  set.seed(42)

  n_subj <- 10
  n_cond <- 3
  n_features <- 50
  n_behav <- 2
  n_groups <- 1

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), n_subj * n_cond, n_behav)

  # Design for method 6: num_groups * num_cond + num_groups * length(bscan) * ncol(behavdata)
  # With default bscan = 1:num_cond = 1:3, that's 1*3 + 1*3*2 = 3 + 6 = 9 rows
  design <- matrix(1:9, nrow = 9, ncol = 1)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = n_subj,
    num_cond = n_cond,
    method = 6,
    stacked_behavdata = behavdata,
    stacked_designdata = design,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(result$method, 6L)
  expect_true(!is.null(result$usc))
  expect_true(!is.null(result$vsc))
  expect_true(!is.null(result$TBv))
  expect_true(!is.null(result$TBusc))
  expect_true(!is.null(result$TBvsc))
})

# -----------------------------------------------------------------------------
# Cross-Method Consistency Tests
# -----------------------------------------------------------------------------

test_that("all methods produce valid S3 structure", {
  set.seed(42)

  n_subj <- 8
  n_cond <- 2
  n_features <- 30
  n_behav <- 2
  n_groups <- 1

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), n_subj * n_cond, n_behav)

  # Design matrices with correct dimensions for each method
  design_m2 <- matrix(c(1, -1), nrow = n_groups * n_cond, ncol = 1)                    # 2 rows
  design_m5 <- matrix(1:4, nrow = n_groups * n_cond * n_behav, ncol = 1)               # 2*2=4 rows
  design_m6 <- matrix(1:6, nrow = n_groups * n_cond + n_groups * n_cond * n_behav, ncol = 1)  # 2+4=6 rows

  methods <- list(
    list(method = 1, behav = NULL, design = NULL),
    list(method = 2, behav = NULL, design = design_m2),
    list(method = 3, behav = behavdata, design = NULL),
    list(method = 4, behav = behavdata, design = NULL),
    list(method = 5, behav = behavdata, design = design_m5),
    list(method = 6, behav = behavdata, design = design_m6)
  )

  for (m in methods) {
    result <- pls_analysis(
      datamat_lst = list(datamat1),
      num_subj_lst = n_subj,
      num_cond = n_cond,
      method = m$method,
      stacked_behavdata = m$behav,
      stacked_designdata = m$design,
      progress = FALSE
    )

    expect_s3_class(result, "pls_result")
    expect_true(all(result$s >= 0))
    expect_equal(nrow(result$u), n_features)
  }
})

test_that("methods produce reproducible results with set.seed", {
  set.seed(123)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  set.seed(456)
  result1 <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_perm = 5,
    progress = FALSE
  )

  set.seed(456)
  result2 <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_perm = 5,
    progress = FALSE
  )

  # SVD is deterministic given same input
  expect_equal(result1$s, result2$s)
  expect_equal(result1$u, result2$u)

  # Permutation results depend on RNG
  expect_equal(result1$perm_result$sprob, result2$perm_result$sprob)
})
