# Tests for pls_covariance.R

# --- pls_xcor helpers (used by covariance functions) ---

# Helper: small balanced design
make_datamat <- function(n_subj = 6, n_cond = 2, n_vox = 20, seed = 42) {
  set.seed(seed)
  matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)
}

make_behav <- function(n_subj = 6, n_cond = 2, n_behav = 2, seed = 42) {
  set.seed(seed)
  matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)
}

# --- pls_corr_maps ---

test_that("pls_corr_maps returns correct dimensions (single n_subj)", {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 2L
  n_vox   <- 20L
  n_behav <- 3L

  datamat   <- matrix(rnorm(n_subj * n_cond * n_vox),   nrow = n_subj * n_cond, ncol = n_vox)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)

  result <- pls_corr_maps(behavdata, datamat, n_subj, n_cond, cormode = 0L)

  expect_equal(nrow(result), n_behav * n_cond)
  expect_equal(ncol(result), n_vox)
})

test_that("pls_corr_maps returns correct dimensions with covariance mode", {
  set.seed(42)
  n_subj  <- 8L
  n_cond  <- 3L
  n_vox   <- 15L
  n_behav <- 2L

  datamat   <- matrix(rnorm(n_subj * n_cond * n_vox),   nrow = n_subj * n_cond, ncol = n_vox)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)

  result <- pls_corr_maps(behavdata, datamat, n_subj, n_cond, cormode = 2L)

  expect_equal(nrow(result), n_behav * n_cond)
  expect_equal(ncol(result), n_vox)
  expect_true(is.matrix(result))
})

test_that("pls_corr_maps works with vector n_subj (ssb design)", {
  set.seed(42)
  n_subj_vec <- c(5L, 7L)
  n_cond     <- 2L
  n_vox      <- 10L
  n_behav    <- 2L
  total_rows <- sum(n_subj_vec)

  datamat   <- matrix(rnorm(total_rows * n_vox),   nrow = total_rows, ncol = n_vox)
  behavdata <- matrix(rnorm(total_rows * n_behav), nrow = total_rows, ncol = n_behav)

  result <- pls_corr_maps(behavdata, datamat, n_subj_vec, n_cond, cormode = 0L)

  expect_equal(nrow(result), n_behav * n_cond)
  expect_equal(ncol(result), n_vox)
})

test_that("pls_corr_maps errors when n_subj vector length != num_cond", {
  set.seed(42)
  datamat   <- matrix(rnorm(20), 10, 2)
  behavdata <- matrix(rnorm(20), 10, 2)

  expect_error(
    pls_corr_maps(behavdata, datamat, n_subj = c(5L, 5L, 5L), num_cond = 2L),
    "n_subj must have length num_cond"
  )
})

test_that("pls_corr_maps errors when row counts mismatch (ssb)", {
  set.seed(42)
  datamat   <- matrix(rnorm(20), 10, 2)
  behavdata <- matrix(rnorm(20), 10, 2)

  # n_subj_vec sums to 12 but datamat has 10 rows
  expect_error(
    pls_corr_maps(behavdata, datamat, n_subj = c(6L, 6L), num_cond = 2L),
    "sum\\(n_subj\\) must equal"
  )
})

# --- pls_corr_maps_notall ---

test_that("pls_corr_maps_notall returns correct dimensions for subset of conditions", {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 3L
  n_vox   <- 20L
  n_behav <- 2L
  bscan   <- c(1L, 3L)  # Skip condition 2

  datamat   <- matrix(rnorm(n_subj * n_cond * n_vox),   nrow = n_subj * n_cond, ncol = n_vox)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)

  result <- pls_corr_maps_notall(behavdata, datamat, n_subj, bscan, cormode = 0L)

  expect_equal(nrow(result), n_behav * length(bscan))
  expect_equal(ncol(result), n_vox)
})

test_that("pls_corr_maps_notall with single condition subset", {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 3L
  n_vox   <- 10L
  n_behav <- 1L
  bscan   <- 2L

  datamat   <- matrix(rnorm(n_subj * n_cond * n_vox),   nrow = n_subj * n_cond, ncol = n_vox)
  behavdata <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)

  result <- pls_corr_maps_notall(behavdata, datamat, n_subj, bscan, cormode = 0L)

  expect_equal(nrow(result), n_behav)
  expect_equal(ncol(result), n_vox)
})

test_that("pls_corr_maps_notall works with ssb design", {
  set.seed(42)
  n_subj_vec <- c(5L, 6L, 4L)
  bscan      <- c(1L, 3L)
  n_vox      <- 10L
  n_behav    <- 2L
  total_rows <- sum(n_subj_vec)

  datamat   <- matrix(rnorm(total_rows * n_vox),   nrow = total_rows, ncol = n_vox)
  behavdata <- matrix(rnorm(total_rows * n_behav), nrow = total_rows, ncol = n_behav)

  result <- pls_corr_maps_notall(behavdata, datamat, n_subj_vec, bscan, cormode = 0L)

  expect_equal(nrow(result), n_behav * length(bscan))
  expect_equal(ncol(result), n_vox)
})

# --- pls_get_covcor (method 1: task PLS) ---

test_that("pls_get_covcor method=1 returns matrix with correct dimensions", {
  set.seed(42)
  n_subj <- 6L
  n_cond <- 2L
  n_vox  <- 20L

  stacked <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)

  result <- pls_get_covcor(
    method          = 1L,
    stacked_datamat = stacked,
    num_groups      = 1L,
    num_subj_lst    = n_subj,
    num_cond        = n_cond
  )

  expect_named(result, c("datamatsvd", "datamatsvd_unnorm", "datamatcorrs_lst", "stacked_smeanmat"))
  # datamatsvd should be n_cond x n_vox (one row per condition per group)
  expect_equal(nrow(result$datamatsvd), n_cond)
  expect_equal(ncol(result$datamatsvd), n_vox)
  expect_null(result$datamatsvd_unnorm)
})

test_that("pls_get_covcor method=1 with two groups stacks output", {
  set.seed(42)
  n_subj <- 6L
  n_cond <- 2L
  n_vox  <- 20L

  stacked <- matrix(rnorm(n_subj * 2 * n_cond * n_vox), nrow = n_subj * 2 * n_cond, ncol = n_vox)

  result <- pls_get_covcor(
    method          = 1L,
    stacked_datamat = stacked,
    num_groups      = 2L,
    num_subj_lst    = c(n_subj, n_subj),
    num_cond        = n_cond
  )

  # Two groups, each contributing n_cond rows
  expect_equal(nrow(result$datamatsvd), n_cond * 2L)
  expect_equal(ncol(result$datamatsvd), n_vox)
})

test_that("pls_get_covcor method=1 compute_smeanmat=TRUE populates stacked_smeanmat", {
  set.seed(42)
  n_subj <- 6L
  n_cond <- 2L
  n_vox  <- 15L

  stacked <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)

  result <- pls_get_covcor(
    method            = 1L,
    stacked_datamat   = stacked,
    num_groups        = 1L,
    num_subj_lst      = n_subj,
    num_cond          = n_cond,
    compute_smeanmat  = TRUE
  )

  expect_false(is.null(result$stacked_smeanmat))
  expect_equal(nrow(result$stacked_smeanmat), n_subj * n_cond)
  expect_equal(ncol(result$stacked_smeanmat), n_vox)
})

test_that("pls_get_covcor method=1 meancentering_type=1 runs without error", {
  set.seed(42)
  n_subj <- 6L
  n_cond <- 2L
  n_vox  <- 10L

  stacked <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)

  result <- pls_get_covcor(
    method              = 1L,
    stacked_datamat     = stacked,
    num_groups          = 1L,
    num_subj_lst        = n_subj,
    num_cond            = n_cond,
    meancentering_type  = 1L
  )

  expect_equal(nrow(result$datamatsvd), n_cond)
  expect_equal(ncol(result$datamatsvd), n_vox)
})

test_that("pls_get_covcor method=1 meancentering_type=2 runs without error", {
  set.seed(42)
  n_subj <- 6L
  n_cond <- 3L
  n_vox  <- 10L

  stacked <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)

  result <- pls_get_covcor(
    method              = 1L,
    stacked_datamat     = stacked,
    num_groups          = 1L,
    num_subj_lst        = n_subj,
    num_cond            = n_cond,
    meancentering_type  = 2L
  )

  expect_equal(nrow(result$datamatsvd), n_cond)
})

test_that("pls_get_covcor method=1 meancentering_type=3 runs without error", {
  set.seed(42)
  n_subj <- 6L
  n_cond <- 2L
  n_vox  <- 10L

  stacked <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)

  result <- pls_get_covcor(
    method              = 1L,
    stacked_datamat     = stacked,
    num_groups          = 1L,
    num_subj_lst        = n_subj,
    num_cond            = n_cond,
    meancentering_type  = 3L
  )

  expect_equal(nrow(result$datamatsvd), n_cond)
})

# --- pls_get_covcor (method 3: behavior PLS) ---

test_that("pls_get_covcor method=3 returns dimensions (n_behav * n_cond) x n_vox", {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 2L
  n_vox   <- 20L
  n_behav <- 3L

  stacked       <- matrix(rnorm(n_subj * n_cond * n_vox),   nrow = n_subj * n_cond, ncol = n_vox)
  stacked_behav <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)

  result <- pls_get_covcor(
    method               = 3L,
    stacked_datamat      = stacked,
    stacked_behavdata    = stacked_behav,
    num_groups           = 1L,
    num_subj_lst         = n_subj,
    num_cond             = n_cond
  )

  expect_equal(nrow(result$datamatsvd), n_behav * n_cond)
  expect_equal(ncol(result$datamatsvd), n_vox)
})

test_that("pls_get_covcor method=3 populates datamatcorrs_lst", {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 2L
  n_vox   <- 15L
  n_behav <- 2L

  stacked       <- matrix(rnorm(n_subj * n_cond * n_vox),   nrow = n_subj * n_cond, ncol = n_vox)
  stacked_behav <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)

  result <- pls_get_covcor(
    method               = 3L,
    stacked_datamat      = stacked,
    stacked_behavdata    = stacked_behav,
    num_groups           = 1L,
    num_subj_lst         = n_subj,
    num_cond             = n_cond
  )

  expect_equal(length(result$datamatcorrs_lst), 1L)
  expect_equal(dim(result$datamatcorrs_lst[[1]]), c(n_behav * n_cond, n_vox))
})

test_that("pls_get_covcor method=3 two groups stacks output", {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 2L
  n_vox   <- 10L
  n_behav <- 2L

  stacked       <- matrix(rnorm(n_subj * 2 * n_cond * n_vox),   nrow = n_subj * 2 * n_cond, ncol = n_vox)
  stacked_behav <- matrix(rnorm(n_subj * 2 * n_cond * n_behav), nrow = n_subj * 2 * n_cond, ncol = n_behav)

  result <- pls_get_covcor(
    method               = 3L,
    stacked_datamat      = stacked,
    stacked_behavdata    = stacked_behav,
    num_groups           = 2L,
    num_subj_lst         = c(n_subj, n_subj),
    num_cond             = n_cond
  )

  expect_equal(nrow(result$datamatsvd), n_behav * n_cond * 2L)
  expect_equal(length(result$datamatcorrs_lst), 2L)
})

test_that("pls_get_covcor errors on invalid method", {
  set.seed(42)
  stacked <- matrix(rnorm(60), 6, 10)
  expect_error(
    pls_get_covcor(
      method          = 7L,
      stacked_datamat = stacked,
      num_groups      = 1L,
      num_subj_lst    = 3L,
      num_cond        = 2L
    )
  )
})
