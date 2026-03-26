# Tests for pls_covariance.R untested branches in pls_get_covcor()

# Helper to build a simple stacked data matrix and behavior matrix
make_stacked <- function(n_subj, n_cond, n_feat, seed = 42) {
  set.seed(seed)
  matrix(rnorm(n_subj * n_cond * n_feat), nrow = n_subj * n_cond, ncol = n_feat)
}

make_behav <- function(n_subj, n_cond, n_behav, seed = 99) {
  set.seed(seed)
  matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)
}

# ---- Method 4 (Multiblock PLS) ----

test_that("pls_get_covcor method 4 returns normalized stacked matrix", {
  set.seed(42)
  n_subj <- 8; n_cond <- 3; n_feat <- 10; n_behav <- 2
  dm <- make_stacked(n_subj, n_cond, n_feat)
  bm <- make_behav(n_subj, n_cond, n_behav)

  result <- pls_get_covcor(
    method             = 4L,
    stacked_datamat    = dm,
    stacked_behavdata  = bm,
    num_groups         = 1L,
    num_subj_lst       = n_subj,
    num_cond           = n_cond,
    bscan              = seq_len(n_cond),
    meancentering_type = 0L,
    cormode            = 0L
  )

  expect_true(!is.null(result$datamatsvd))
  # Task block rows: n_cond; behavior block rows: n_behav * length(bscan)
  expect_equal(nrow(result$datamatsvd), n_cond + n_behav * n_cond)
  expect_equal(ncol(result$datamatsvd), n_feat)
  # datamatcorrs_lst should have one entry (behavior block)
  expect_equal(length(result$datamatcorrs_lst), 1L)
})

test_that("pls_get_covcor method 4 with meancentering_type=1", {
  set.seed(42)
  n_subj <- 8; n_cond <- 3; n_feat <- 10; n_behav <- 2
  dm <- make_stacked(n_subj, n_cond, n_feat)
  bm <- make_behav(n_subj, n_cond, n_behav)

  result <- pls_get_covcor(
    method             = 4L,
    stacked_datamat    = dm,
    stacked_behavdata  = bm,
    num_groups         = 1L,
    num_subj_lst       = n_subj,
    num_cond           = n_cond,
    bscan              = seq_len(n_cond),
    meancentering_type = 1L,
    cormode            = 0L
  )

  expect_true(!is.null(result$datamatsvd))
})

test_that("pls_get_covcor method 4 with meancentering_type=2", {
  set.seed(42)
  n_subj <- 8; n_cond <- 3; n_feat <- 10; n_behav <- 2
  dm <- make_stacked(n_subj, n_cond, n_feat)
  bm <- make_behav(n_subj, n_cond, n_behav)

  result <- pls_get_covcor(
    method             = 4L,
    stacked_datamat    = dm,
    stacked_behavdata  = bm,
    num_groups         = 1L,
    num_subj_lst       = n_subj,
    num_cond           = n_cond,
    bscan              = seq_len(n_cond),
    meancentering_type = 2L,
    cormode            = 0L
  )

  expect_true(!is.null(result$datamatsvd))
})

test_that("pls_get_covcor method 4 with meancentering_type=3", {
  set.seed(42)
  n_subj <- 8; n_cond <- 3; n_feat <- 10; n_behav <- 2
  dm <- make_stacked(n_subj, n_cond, n_feat)
  bm <- make_behav(n_subj, n_cond, n_behav)

  result <- pls_get_covcor(
    method             = 4L,
    stacked_datamat    = dm,
    stacked_behavdata  = bm,
    num_groups         = 1L,
    num_subj_lst       = n_subj,
    num_cond           = n_cond,
    bscan              = seq_len(n_cond),
    meancentering_type = 3L,
    cormode            = 0L
  )

  expect_true(!is.null(result$datamatsvd))
})

# ---- Method 5 (Non-Rotated Behavior PLS) ----

test_that("pls_get_covcor method 5 returns behavior correlation matrix", {
  set.seed(42)
  n_subj <- 8; n_cond <- 3; n_feat <- 10; n_behav <- 2
  dm <- make_stacked(n_subj, n_cond, n_feat)
  bm <- make_behav(n_subj, n_cond, n_behav)

  result <- pls_get_covcor(
    method             = 5L,
    stacked_datamat    = dm,
    stacked_behavdata  = bm,
    num_groups         = 1L,
    num_subj_lst       = n_subj,
    num_cond           = n_cond,
    meancentering_type = 0L,
    cormode            = 0L
  )

  expect_true(!is.null(result$datamatsvd))
  # Same as method 3: n_behav * n_cond rows
  expect_equal(nrow(result$datamatsvd), n_behav * n_cond)
  expect_equal(length(result$datamatcorrs_lst), 1L)
})

# ---- Method 6 (Non-Rotated Multiblock PLS) ----

test_that("pls_get_covcor method 6 returns combined task+behavior matrix", {
  set.seed(42)
  n_subj <- 8; n_cond <- 3; n_feat <- 10; n_behav <- 2
  dm <- make_stacked(n_subj, n_cond, n_feat)
  bm <- make_behav(n_subj, n_cond, n_behav)

  result <- pls_get_covcor(
    method             = 6L,
    stacked_datamat    = dm,
    stacked_behavdata  = bm,
    num_groups         = 1L,
    num_subj_lst       = n_subj,
    num_cond           = n_cond,
    bscan              = seq_len(n_cond),
    meancentering_type = 0L,
    cormode            = 0L
  )

  expect_true(!is.null(result$datamatsvd))
  expect_equal(nrow(result$datamatsvd), n_cond + n_behav * n_cond)
  expect_equal(length(result$datamatcorrs_lst), 1L)
})

# ---- SSB (list num_subj_lst) path ----

test_that("pls_get_covcor method 1 with list num_subj_lst (SSB design)", {
  set.seed(42)
  # Two groups with different subjects per condition:
  # group 1: c(5, 5, 5) subjects per condition => 15 rows
  # group 2: c(4, 4, 4) subjects per condition => 12 rows
  # Total: 27 rows
  n_feat <- 8; n_cond <- 3
  g1 <- c(5L, 5L, 5L)
  g2 <- c(4L, 4L, 4L)
  dm <- matrix(rnorm(27 * n_feat), nrow = 27, ncol = n_feat)

  result <- pls_get_covcor(
    method             = 1L,
    stacked_datamat    = dm,
    stacked_behavdata  = NULL,
    num_groups         = 2L,
    num_subj_lst       = list(g1, g2),
    num_cond           = n_cond,
    meancentering_type = 0L,
    cormode            = 0L
  )

  expect_true(!is.null(result$datamatsvd))
  # Two groups, each contributing n_cond rows to task mean matrix
  expect_equal(nrow(result$datamatsvd), n_cond * 2L)
})

test_that("pls_get_covcor method 1 SSB with meancentering_type=1", {
  set.seed(42)
  n_feat <- 8; n_cond <- 3
  g1 <- c(5L, 5L, 5L)
  g2 <- c(4L, 4L, 4L)
  dm <- matrix(rnorm(27 * n_feat), nrow = 27, ncol = n_feat)

  result <- pls_get_covcor(
    method             = 1L,
    stacked_datamat    = dm,
    stacked_behavdata  = NULL,
    num_groups         = 2L,
    num_subj_lst       = list(g1, g2),
    num_cond           = n_cond,
    meancentering_type = 1L,
    cormode            = 0L
  )

  expect_true(!is.null(result$datamatsvd))
})

test_that("pls_get_covcor method 3 with list num_subj_lst (SSB behavior)", {
  set.seed(42)
  n_feat <- 8; n_cond <- 3; n_behav <- 2
  g1 <- c(5L, 5L, 5L)
  g2 <- c(4L, 4L, 4L)
  total <- sum(unlist(list(g1, g2)))
  dm <- matrix(rnorm(total * n_feat), nrow = total, ncol = n_feat)
  bm <- matrix(rnorm(total * n_behav), nrow = total, ncol = n_behav)

  result <- pls_get_covcor(
    method             = 3L,
    stacked_datamat    = dm,
    stacked_behavdata  = bm,
    num_groups         = 2L,
    num_subj_lst       = list(g1, g2),
    num_cond           = n_cond,
    meancentering_type = 0L,
    cormode            = 0L
  )

  expect_true(!is.null(result$datamatsvd))
  expect_equal(length(result$datamatcorrs_lst), 2L)
})

# ---- compute_smeanmat paths ----

test_that("pls_get_covcor method 1 compute_smeanmat=TRUE returns stacked_smeanmat", {
  set.seed(42)
  n_subj <- 8; n_cond <- 3; n_feat <- 10
  dm <- make_stacked(n_subj, n_cond, n_feat)

  result <- pls_get_covcor(
    method             = 1L,
    stacked_datamat    = dm,
    stacked_behavdata  = NULL,
    num_groups         = 1L,
    num_subj_lst       = n_subj,
    num_cond           = n_cond,
    meancentering_type = 0L,
    cormode            = 0L,
    compute_smeanmat   = TRUE
  )

  expect_true(!is.null(result$stacked_smeanmat))
  expect_equal(dim(result$stacked_smeanmat), c(n_subj * n_cond, n_feat))
})

test_that("pls_get_covcor method 2 compute_smeanmat=TRUE returns stacked_smeanmat", {
  set.seed(42)
  n_subj <- 8; n_cond <- 3; n_feat <- 10
  dm <- make_stacked(n_subj, n_cond, n_feat)

  result <- pls_get_covcor(
    method             = 2L,
    stacked_datamat    = dm,
    stacked_behavdata  = NULL,
    num_groups         = 1L,
    num_subj_lst       = n_subj,
    num_cond           = n_cond,
    meancentering_type = 0L,
    cormode            = 0L,
    compute_smeanmat   = TRUE
  )

  expect_true(!is.null(result$stacked_smeanmat))
})
