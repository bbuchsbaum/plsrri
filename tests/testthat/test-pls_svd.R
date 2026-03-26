# Tests for pls_svd and misssvd

test_that("pls_svd returns u, d, v components for square matrix", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  result <- pls_svd(X)

  expect_named(result, c("u", "d", "v"))
  expect_true(is.matrix(result$u))
  expect_true(is.numeric(result$d))
  expect_true(is.matrix(result$v))
})

test_that("pls_svd dimensions are correct for square matrix", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  result <- pls_svd(X)

  expect_equal(dim(result$u), c(10, 10))
  expect_equal(length(result$d), 10)
  expect_equal(dim(result$v), c(10, 10))
})

test_that("pls_svd u columns are orthonormal", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  result <- pls_svd(X)

  UtU <- t(result$u) %*% result$u
  expect_equal(UtU, diag(10), tolerance = 1e-10)
})

test_that("pls_svd v columns are orthonormal", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  result <- pls_svd(X)

  VtV <- t(result$v) %*% result$v
  expect_equal(VtV, diag(10), tolerance = 1e-10)
})

test_that("pls_svd singular values are non-negative and sorted descending", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  result <- pls_svd(X)

  expect_true(all(result$d >= 0))
  expect_equal(result$d, sort(result$d, decreasing = TRUE))
})

test_that("pls_svd reconstructs original matrix", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  result <- pls_svd(X)

  X_reconstructed <- result$u %*% diag(result$d) %*% t(result$v)
  expect_equal(X_reconstructed, X, tolerance = 1e-10)
})

test_that("pls_svd works with tall rectangular matrix (rows > cols)", {
  set.seed(42)
  X <- matrix(rnorm(50 * 10), 50, 10)
  result <- pls_svd(X)

  # Economy SVD: min(50, 10) = 10
  expect_equal(dim(result$u), c(50, 10))
  expect_equal(length(result$d), 10)
  expect_equal(dim(result$v), c(10, 10))
})

test_that("pls_svd works with wide rectangular matrix (cols > rows)", {
  set.seed(42)
  X <- matrix(rnorm(6 * 40), 6, 40)
  result <- pls_svd(X)

  # Economy SVD: min(6, 40) = 6
  expect_equal(dim(result$u), c(6, 6))
  expect_equal(length(result$d), 6)
  expect_equal(dim(result$v), c(40, 6))
})

test_that("pls_svd orthonormality holds for wide matrix", {
  set.seed(42)
  X <- matrix(rnorm(6 * 40), 6, 40)
  result <- pls_svd(X)

  UtU <- t(result$u) %*% result$u
  expect_equal(UtU, diag(6), tolerance = 1e-10)

  VtV <- t(result$v) %*% result$v
  expect_equal(VtV, diag(6), tolerance = 1e-10)
})

test_that("pls_svd requires a matrix", {
  expect_error(pls_svd(c(1, 2, 3)))
  expect_error(pls_svd(data.frame(a = 1:3, b = 4:6)))
})

test_that("misssvd returns u, d, v for matrix without NAs", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  result <- misssvd(X)

  expect_named(result, c("u", "d", "v"))
  expect_equal(dim(result$u), c(10, 10))
  expect_equal(length(result$d), 10)
  expect_equal(dim(result$v), c(10, 10))
})

test_that("misssvd handles matrix with NA values", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  X[sample(100, 8)] <- NA
  result <- misssvd(X)

  expect_named(result, c("u", "d", "v"))
  expect_equal(dim(result$u), c(10, 10))
  expect_equal(length(result$d), 10)
  expect_equal(dim(result$v), c(10, 10))
  # No NAs in output
  expect_false(anyNA(result$u))
  expect_false(anyNA(result$d))
  expect_false(anyNA(result$v))
})

test_that("misssvd handles matrix with NaN values", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  X[3, 2] <- NaN
  X[7, 5] <- NaN
  result <- misssvd(X)

  expect_named(result, c("u", "d", "v"))
  expect_false(anyNA(result$u))
  expect_false(anyNA(result$d))
  expect_false(anyNA(result$v))
})

test_that("misssvd singular values are non-negative", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  X[sample(100, 5)] <- NA
  result <- misssvd(X)

  expect_true(all(result$d >= 0))
})

test_that("pls_svd delegates to misssvd when NAs present and handle_missing=TRUE", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  X[1, 1] <- NA

  result <- pls_svd(X, handle_missing = TRUE)
  expect_named(result, c("u", "d", "v"))
  expect_false(anyNA(result$u))
  expect_false(anyNA(result$d))
  expect_false(anyNA(result$v))
})

test_that("misssvd result u has orthonormal columns", {
  set.seed(42)
  X <- matrix(rnorm(100), 10, 10)
  X[sample(100, 6)] <- NA
  result <- misssvd(X)

  UtU <- t(result$u) %*% result$u
  expect_equal(UtU, diag(10), tolerance = 1e-8)
})
