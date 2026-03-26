# Tests for R/pls_scores.R

# --- pls_brain_scores ---

test_that("pls_brain_scores returns correct dimensions", {
  set.seed(42)
  datamat  <- matrix(rnorm(20 * 50), nrow = 20, ncol = 50)
  salience <- matrix(rnorm(50 * 3), nrow = 50, ncol = 3)
  result <- pls_brain_scores(datamat, salience)
  expect_equal(dim(result), c(20L, 3L))
})

test_that("pls_brain_scores equals matrix multiply", {
  set.seed(42)
  datamat  <- matrix(rnorm(10 * 20), nrow = 10, ncol = 20)
  salience <- matrix(rnorm(20 * 2), nrow = 20, ncol = 2)
  expect_equal(pls_brain_scores(datamat, salience), datamat %*% salience)
})

test_that("pls_brain_scores errors when dimensions mismatch", {
  datamat  <- matrix(1, nrow = 5, ncol = 10)
  salience <- matrix(1, nrow = 8, ncol = 2)   # wrong row count
  expect_error(pls_brain_scores(datamat, salience))
})

test_that("pls_brain_scores errors on non-matrix input", {
  expect_error(pls_brain_scores(data.frame(a = 1:5), matrix(1, 5, 2)))
  expect_error(pls_brain_scores(matrix(1, 5, 5), as.data.frame(matrix(1, 5, 2))))
})

# --- pls_design_scores ---

test_that("pls_design_scores returns correct dimensions", {
  set.seed(42)
  design <- matrix(rnorm(6 * 4), nrow = 6, ncol = 4)
  v      <- matrix(rnorm(4 * 2), nrow = 4, ncol = 2)
  result <- pls_design_scores(design, v)
  expect_equal(dim(result), c(6L, 2L))
})

test_that("pls_design_scores equals matrix multiply", {
  set.seed(42)
  design <- matrix(rnorm(8 * 3), nrow = 8, ncol = 3)
  v      <- matrix(rnorm(3 * 2), nrow = 3, ncol = 2)
  expect_equal(pls_design_scores(design, v), design %*% v)
})

test_that("pls_design_scores errors on non-matrix inputs", {
  expect_error(pls_design_scores(1:10, matrix(1, 10, 2)))
})

# --- pls_behav_scores ---

test_that("pls_behav_scores returns matrix with correct dimensions", {
  set.seed(42)
  num_groups   <- 1L
  num_subj_lst <- 6L
  num_cond     <- 2L
  n_lv         <- 2L
  n_behav      <- 3L
  total_rows   <- num_subj_lst * num_cond

  stacked_behavdata <- matrix(rnorm(total_rows * n_behav), nrow = total_rows, ncol = n_behav)
  lvcorrs           <- matrix(rnorm(num_groups * num_cond * n_behav * n_lv),
                               nrow = num_groups * num_cond * n_behav, ncol = n_lv)

  result <- pls_behav_scores(stacked_behavdata, lvcorrs,
                              num_groups    = num_groups,
                              num_subj_lst  = num_subj_lst,
                              num_cond      = num_cond,
                              n_lv          = n_lv)
  expect_equal(dim(result), c(total_rows, n_lv))
})

test_that("pls_behav_scores errors on non-matrix behavdata", {
  expect_error(
    pls_behav_scores(data.frame(a = 1:6), matrix(0, 6, 2),
                     num_groups = 1, num_subj_lst = 3, num_cond = 2, n_lv = 2)
  )
})

# --- pls_lv_corrs ---

test_that("pls_lv_corrs returns matrix with correct dimensions", {
  set.seed(42)
  num_groups   <- 1L
  num_subj_lst <- 8L
  num_cond     <- 2L
  n_behav      <- 3L
  n_lv         <- 2L
  total_rows   <- num_subj_lst * num_cond

  stacked_behavdata <- matrix(rnorm(total_rows * n_behav), nrow = total_rows, ncol = n_behav)
  brain_scores      <- matrix(rnorm(total_rows * n_lv),    nrow = total_rows, ncol = n_lv)

  result <- pls_lv_corrs(stacked_behavdata, brain_scores,
                          num_groups   = num_groups,
                          num_subj_lst = num_subj_lst,
                          num_cond     = num_cond)
  expect_equal(dim(result), c(num_groups * num_cond * n_behav, n_lv))
})

test_that("pls_lv_corrs errors on non-matrix inputs", {
  expect_error(pls_lv_corrs(1:10, matrix(1, 10, 2),
                             num_groups = 1, num_subj_lst = 5,
                             num_cond = 2))
})

# --- pls_split_multiblock_v ---

test_that("pls_split_multiblock_v returns task_v and behav_v components", {
  set.seed(42)
  num_groups <- 1L
  num_cond   <- 3L
  n_behav    <- 2L
  bscan      <- c(1L, 2L)
  n_bscan    <- length(bscan)
  n_lv       <- 2L

  group_block <- num_cond + n_bscan * n_behav  # 3 + 4 = 7 rows per group
  total_rows  <- num_groups * group_block
  v <- matrix(rnorm(total_rows * n_lv), nrow = total_rows, ncol = n_lv)

  result <- pls_split_multiblock_v(v, num_groups, num_cond, n_behav, bscan)
  expect_true(is.list(result))
  expect_true(!is.null(result$task_v))
  expect_true(!is.null(result$behav_v))
  expect_equal(nrow(result$task_v),  num_groups * num_cond)
  expect_equal(nrow(result$behav_v), num_groups * n_bscan * n_behav)
  expect_equal(ncol(result$task_v),  n_lv)
  expect_equal(ncol(result$behav_v), n_lv)
})

test_that("pls_split_multiblock_v with two groups returns correct row counts", {
  set.seed(42)
  num_groups <- 2L
  num_cond   <- 2L
  n_behav    <- 3L
  bscan      <- c(1L)
  n_bscan    <- length(bscan)
  n_lv       <- 2L

  group_block <- num_cond + n_bscan * n_behav  # 2 + 3 = 5 rows per group
  total_rows  <- num_groups * group_block       # 10
  v <- matrix(rnorm(total_rows * n_lv), nrow = total_rows, ncol = n_lv)

  result <- pls_split_multiblock_v(v, num_groups, num_cond, n_behav, bscan)
  expect_equal(nrow(result$task_v),  num_groups * num_cond)     # 4
  expect_equal(nrow(result$behav_v), num_groups * n_bscan * n_behav)  # 6
})

test_that("pls_split_multiblock_v errors on non-matrix v", {
  expect_error(pls_split_multiblock_v(1:10, 1L, 2L, 2L, c(1L)))
})
