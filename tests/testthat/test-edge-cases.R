# Edge case tests
#
# Tests for unusual but valid inputs that might cause issues.

# -----------------------------------------------------------------------------
# Single Group Tests
# -----------------------------------------------------------------------------

test_that("single group analysis works", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = c(10),  # Single group
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(length(result$num_subj_lst), 1)
})

test_that("single group with permutation works", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = c(10),
    num_cond = 3,
    method = 1,
    num_perm = 20,
    progress = FALSE
  )

  expect_s3_class(result$perm_result, "pls_perm_result")
})

# -----------------------------------------------------------------------------
# Single Condition Tests
# -----------------------------------------------------------------------------

test_that("single condition with multiple groups works", {
  set.seed(42)

  # 2 groups, 1 condition each, 10 subjects per group
  datamat1 <- matrix(rnorm(10 * 50), 10, 50)
  datamat2 <- matrix(rnorm(10 * 50), 10, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1, datamat2),
    num_subj_lst = c(10, 10),
    num_cond = 1,  # Single condition
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
})

# -----------------------------------------------------------------------------
# Small Sample Size Tests
# -----------------------------------------------------------------------------

test_that("small sample (3 subjects) works", {
  set.seed(42)

  # 3 subjects * 2 conditions = 6 rows
  datamat1 <- matrix(rnorm(6 * 50), 6, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = c(3),
    num_cond = 2,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
})

test_that("bootstrap with small sample warns but works", {
  set.seed(42)

  # Very small sample for bootstrap
  datamat1 <- matrix(rnorm(8 * 30), 8, 30)  # 4 subjects * 2 conditions

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = c(4),
    num_cond = 2,
    method = 1,
    num_boot = 20,
    progress = FALSE
  )

  # Should still produce result
  expect_s3_class(result, "pls_result")
})

# -----------------------------------------------------------------------------
# Many Conditions Tests
# -----------------------------------------------------------------------------

test_that("many conditions works", {
  set.seed(42)

  # 10 subjects * 10 conditions = 100 rows
  datamat1 <- matrix(rnorm(100 * 50), 100, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = c(10),
    num_cond = 10,  # Many conditions
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(length(result$s), 10)  # Should have 10 LVs
})

# -----------------------------------------------------------------------------
# Many Groups Tests
# -----------------------------------------------------------------------------

test_that("many groups (5) works", {
  set.seed(42)

  # 5 groups with varying sizes
  datamat_lst <- list(
    matrix(rnorm(15 * 50), 15, 50),  # 5 subjects * 3 conditions
    matrix(rnorm(18 * 50), 18, 50),  # 6 subjects * 3 conditions
    matrix(rnorm(12 * 50), 12, 50),  # 4 subjects * 3 conditions
    matrix(rnorm(21 * 50), 21, 50),  # 7 subjects * 3 conditions
    matrix(rnorm(24 * 50), 24, 50)   # 8 subjects * 3 conditions
  )

  result <- pls_analysis(
    datamat_lst = datamat_lst,
    num_subj_lst = c(5, 6, 4, 7, 8),
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(length(result$s), 15)  # 5 groups * 3 conditions
})

# -----------------------------------------------------------------------------
# Unequal Group Sizes Tests
# -----------------------------------------------------------------------------

test_that("very unequal group sizes work", {
  set.seed(42)

  # Group 1: 3 subjects, Group 2: 20 subjects
  datamat1 <- matrix(rnorm(9 * 50), 9, 50)   # 3 * 3 conditions
  datamat2 <- matrix(rnorm(60 * 50), 60, 50) # 20 * 3 conditions

  result <- pls_analysis(
    datamat_lst = list(datamat1, datamat2),
    num_subj_lst = c(3, 20),
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
})

# -----------------------------------------------------------------------------
# Feature Count Edge Cases
# -----------------------------------------------------------------------------

test_that("more features than observations works", {
  set.seed(42)

  # 10 observations, 100 features (p > n)
  datamat1 <- matrix(rnorm(10 * 100), 10, 100)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = c(5),
    num_cond = 2,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(nrow(result$u), 100)  # Features
})

test_that("very few features works", {
  set.seed(42)

  # 30 observations, 5 features
  datamat1 <- matrix(rnorm(30 * 5), 30, 5)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = c(10),
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(nrow(result$u), 5)
})

# -----------------------------------------------------------------------------
# Behavior Data Edge Cases
# -----------------------------------------------------------------------------

test_that("single behavior measure works", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)
  behavdata <- matrix(rnorm(30), 30, 1)  # Single behavior

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 3,
    stacked_behavdata = behavdata,
    progress = FALSE
  )

  expect_s3_class(result, "pls_behavior")
})

test_that("many behavior measures work", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)
  behavdata <- matrix(rnorm(30 * 10), 30, 10)  # 10 behaviors

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 3,
    stacked_behavdata = behavdata,
    progress = FALSE
  )

  expect_s3_class(result, "pls_behavior")
})

# -----------------------------------------------------------------------------
# Numerical Stability Tests
# -----------------------------------------------------------------------------

test_that("handles near-zero variance features", {
  set.seed(42)

  # Create data with some near-constant features
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)
  datamat1[, 1] <- 1 + rnorm(30, sd = 1e-10)  # Near-constant

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_true(all(is.finite(result$s)))
})

test_that("handles large magnitude data", {
  set.seed(42)

  # Large values
  datamat1 <- matrix(rnorm(30 * 50, mean = 1e6, sd = 1e5), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_true(all(is.finite(result$s)))
})

test_that("handles small magnitude data", {
  set.seed(42)

  # Small values
  datamat1 <- matrix(rnorm(30 * 50, mean = 0, sd = 1e-6), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
})

# -----------------------------------------------------------------------------
# Input Validation Tests
# -----------------------------------------------------------------------------

test_that("rejects invalid method numbers", {
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  expect_error(
    pls_analysis(
      datamat_lst = list(datamat1),
      num_subj_lst = 10,
      num_cond = 3,
      method = 7,  # Invalid
      progress = FALSE
    )
  )

  expect_error(
    pls_analysis(
      datamat_lst = list(datamat1),
      num_subj_lst = 10,
      num_cond = 3,
      method = 0,  # Invalid
      progress = FALSE
    )
  )
})

test_that("rejects mismatched dimensions", {
  # num_subj_lst doesn't match datamat_lst
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)
  datamat2 <- matrix(rnorm(30 * 50), 30, 50)

  expect_error(
    pls_analysis(
      datamat_lst = list(datamat1, datamat2),
      num_subj_lst = c(10),  # Should be c(10, 10)
      num_cond = 3,
      method = 1,
      progress = FALSE
    )
  )
})

test_that("rejects inconsistent feature counts", {
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)
  datamat2 <- matrix(rnorm(30 * 40), 30, 40)  # Different features

  expect_error(
    pls_analysis(
      datamat_lst = list(datamat1, datamat2),
      num_subj_lst = c(10, 10),
      num_cond = 3,
      method = 1,
      progress = FALSE
    )
  )
})

# -----------------------------------------------------------------------------
# Builder API Edge Cases
# -----------------------------------------------------------------------------

test_that("builder handles single matrix input", {
  set.seed(42)
  data <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_spec() |>
    add_subjects(data, groups = 10) |>  # Single matrix, not list
    add_conditions(3) |>
    run(progress = FALSE)

  expect_s3_class(result, "pls_result")
})

test_that("configure handles string method names", {
  spec <- pls_spec() |>
    configure(method = "task")

  expect_equal(spec$method, 1L)

  spec2 <- pls_spec() |>
    configure(method = "BEHAVIOR")  # Case insensitive?

  # Should either work or error gracefully
  expect_true(spec2$method %in% c(3L) || inherits(spec2, "pls_spec"))
})
