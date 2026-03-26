# Tests for cross-correlation functions

test_that("pls_xcor Pearson correlation", {
  set.seed(42)

  design <- matrix(rnorm(20), 10, 2)
  datamat <- matrix(rnorm(50), 10, 5)

  xcor <- pls_xcor(design, datamat, cormode = 0)

  expect_equal(dim(xcor), c(2, 5))
  expect_true(all(abs(xcor) <= 1))  # Correlations bounded by [-1, 1]
})

test_that("pls_xcor covariance mode", {
  design <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 10, 1)
  datamat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 10, 1)

  # Covariance of identical vectors = variance
  xcor <- pls_xcor(design, datamat, cormode = 2)
  expected_cov <- var(1:10)

  expect_equal(xcor[1, 1], expected_cov, tolerance = 1e-10)
})

test_that("pls_xcor dot product mode", {
  design <- matrix(c(1, 2, 3), 3, 1)
  datamat <- matrix(c(4, 5, 6), 3, 1)

  xcor <- pls_xcor(design, datamat, cormode = 6)

  # Dot product: 1*4 + 2*5 + 3*6 = 32
  expect_equal(xcor[1, 1], 32)
})

test_that("pls_xcor handles zero variance columns", {
  design <- matrix(c(1, 1, 1, 1, 2, 3), 3, 2)  # First column constant
  datamat <- matrix(rnorm(9), 3, 3)

  # Should not error, zero variance column gets zeroed
  xcor <- pls_xcor(design, datamat, cormode = 0)

  expect_equal(dim(xcor), c(2, 3))
  expect_true(all(xcor[1, ] == 0))  # First row (constant column) should be 0
})

test_that("pls_xcor input validation", {
  design <- matrix(rnorm(10), 5, 2)
  datamat <- matrix(rnorm(20), 10, 2)  # Different number of rows

  expect_error(
    pls_xcor(design, datamat),
    "same number of rows"
  )
})

test_that("pls_xcor cosine angle mode returns correct dimensions", {
  set.seed(42)
  design  <- matrix(rnorm(30), 10, 3)
  datamat <- matrix(rnorm(50), 10, 5)

  xcor <- pls_xcor(design, datamat, cormode = 4)

  expect_equal(dim(xcor), c(3, 5))
  expect_true(is.matrix(xcor))
})

test_that("pls_xcor invalid cormode errors", {
  design  <- matrix(rnorm(20), 10, 2)
  datamat <- matrix(rnorm(30), 10, 3)

  expect_error(pls_xcor(design, datamat, cormode = 99), "cormode must be")
})

# --- missing-data variants ---

test_that("pls_xcor dispatches to missing-data path when NAs present (Pearson)", {
  set.seed(42)
  design  <- matrix(rnorm(30), 10, 3)
  datamat <- matrix(rnorm(50), 10, 5)
  datamat[2, 3] <- NA  # inject one NA

  # Should not error; returns same shape
  xcor <- pls_xcor(design, datamat, cormode = 0)
  expect_equal(dim(xcor), c(3, 5))
})

test_that("pls_xcor dispatches to missing-data path with NAs in covariance mode", {
  set.seed(42)
  design  <- matrix(rnorm(30), 10, 3)
  datamat <- matrix(rnorm(50), 10, 5)
  design[1, 2] <- NA

  xcor <- pls_xcor(design, datamat, cormode = 2)
  expect_equal(dim(xcor), c(3, 5))
})

test_that("pls_xcor dispatches to missing-data path with NAs in cosine mode", {
  set.seed(42)
  design  <- matrix(rnorm(20), 10, 2)
  datamat <- matrix(rnorm(40), 10, 4)
  datamat[5, 1] <- NaN

  xcor <- pls_xcor(design, datamat, cormode = 4)
  expect_equal(dim(xcor), c(2, 4))
})

test_that("pls_xcor dispatches to missing-data path with NAs in dot-product mode", {
  set.seed(42)
  design  <- matrix(rnorm(20), 10, 2)
  datamat <- matrix(rnorm(40), 10, 4)
  datamat[3, 2] <- Inf

  xcor <- pls_xcor(design, datamat, cormode = 6)
  expect_equal(dim(xcor), c(2, 4))
})

test_that("pls_xcor clean data matches missing-data path when no NAs", {
  set.seed(42)
  design  <- matrix(rnorm(30), 10, 3)
  datamat <- matrix(rnorm(50), 10, 5)

  xcor_clean   <- pls_xcor(design, datamat, cormode = 0)
  xcor_missna  <- plsrri:::.pls_xcor_missnk(design, datamat, cormode = 0)

  expect_equal(xcor_clean, xcor_missna, tolerance = 1e-10)
})

# --- .pls_misssum ---

test_that(".pls_misssum returns weighted column sums", {
  X <- matrix(c(1, NA, 3, 4, 5, 6), nrow = 3, ncol = 2)
  # col 1: values 1, NA, 3 -> n_real=2, weight=3, out = 3*(1+3)/2 = 6
  out <- plsrri:::.pls_misssum(X)
  expect_equal(out[1], 6.0)
  # col 2: all present -> out = 3*(4+5+6)/3 = 15
  expect_equal(out[2], 15.0)
})

test_that(".pls_misssum returns NA when all values missing", {
  X <- matrix(NA_real_, nrow = 3, ncol = 2)
  out <- plsrri:::.pls_misssum(X)
  expect_true(is.na(out[1]))
  expect_true(is.na(out[2]))
})

test_that(".pls_misssum handles vector input", {
  x <- c(1, NA, 3)
  # n_real=2, weight=3, out = 3*(1+3)/2 = 6
  out <- plsrri:::.pls_misssum(x)
  expect_equal(out, 6.0)
})

# --- .pls_missmean ---

test_that(".pls_missmean ignores NA in column means", {
  X <- matrix(c(1, NA, 3, 4, 5, 6), nrow = 3, ncol = 2)
  out <- plsrri:::.pls_missmean(X, margin = 1L)
  expect_equal(out[1], mean(c(1, 3)))
  expect_equal(out[2], mean(c(4, 5, 6)))
})

test_that(".pls_missmean margin=2 gives row means ignoring NA", {
  X <- matrix(c(1, 2, NA, 4), nrow = 2, ncol = 2)
  out <- plsrri:::.pls_missmean(X, margin = 2L)
  expect_equal(out[1], mean(c(1, NA), na.rm = TRUE))
  expect_equal(out[2], mean(c(2, 4)))
})

# --- .pls_missmult ---

test_that(".pls_missmult matches standard product when no NAs", {
  set.seed(42)
  A <- matrix(rnorm(12), 3, 4)
  B <- matrix(rnorm(8),  4, 2)

  out_miss  <- plsrri:::.pls_missmult(A, B)
  out_clean <- A %*% B
  expect_equal(out_miss, out_clean, tolerance = 1e-10)
})

test_that(".pls_missmult handles NA entries without error", {
  A <- matrix(c(1, NA, 3, 4), 2, 2)
  B <- matrix(c(1, 0, 0, 1), 2, 2)

  out <- plsrri:::.pls_missmult(A, B)
  expect_equal(dim(out), c(2, 2))
})

test_that(".pls_missmult errors on incompatible dimensions", {
  A <- matrix(rnorm(6), 2, 3)
  B <- matrix(rnorm(4), 2, 2)
  expect_error(plsrri:::.pls_missmult(A, B), "incompatible")
})
