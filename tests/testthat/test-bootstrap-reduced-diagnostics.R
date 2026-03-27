bootstrap_task_reduced_diagnostic <- function(dat, bootsamp, b, observed) {
  cov_full <- pls_get_covcor(
    method = 1L,
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 2L,
    datamat_reorder = bootsamp[, b],
    compute_smeanmat = TRUE
  )

  xsvd <- pls_svd(dat, handle_missing = FALSE)
  scores <- xsvd$u %*% diag(xsvd$d, nrow = length(xsvd$d))
  loadings <- xsvd$v

  cov_red <- pls_get_covcor(
    method = 1L,
    stacked_datamat = scores,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 2L,
    datamat_reorder = bootsamp[, b],
    compute_smeanmat = TRUE
  )

  r <- nrow(cov_full$datamatsvd)
  c <- ncol(cov_full$datamatsvd)
  n_keep <- min(ncol(observed$u), min(r, c))

  svd_full <- svd(t(cov_full$datamatsvd), nu = n_keep, nv = n_keep)
  pu_full <- svd_full$u[, seq_len(n_keep), drop = FALSE]
  pv_full <- svd_full$v[, seq_len(n_keep), drop = FALSE]
  d_full <- svd_full$d[seq_len(n_keep)]
  rot_full <- pls_bootprocrust(observed$v[, seq_len(n_keep), drop = FALSE], pv_full)

  u_full_scaled <- matrix(0, nrow = nrow(observed$u), ncol = ncol(observed$u))
  u_full_scaled[, seq_len(n_keep)] <-
    pu_full %*% diag(d_full, nrow = n_keep) %*% rot_full

  svd_red <- svd(t(cov_red$datamatsvd), nu = n_keep, nv = n_keep)
  pu_red <- svd_red$u[, seq_len(n_keep), drop = FALSE]
  pv_red <- svd_red$v[, seq_len(n_keep), drop = FALSE]
  d_red <- svd_red$d[seq_len(n_keep)]
  rot_red <- pls_bootprocrust(observed$v[, seq_len(n_keep), drop = FALSE], pv_red)

  u_red_scaled <- matrix(0, nrow = ncol(loadings), ncol = ncol(observed$u))
  u_red_scaled[, seq_len(n_keep)] <-
    pu_red %*% diag(d_red, nrow = n_keep) %*% rot_red

  u_lifted <- loadings %*% u_red_scaled

  usc2_full <- cov_full$stacked_smeanmat %*% normalize_rows(u_full_scaled, margin = 2L)
  usc2_red <- cov_red$stacked_smeanmat %*% normalize_rows(u_red_scaled, margin = 2L)

  list(
    cov_full = cov_full,
    cov_red = cov_red,
    u_full_scaled = u_full_scaled,
    u_red_scaled = u_red_scaled,
    u_lifted = u_lifted,
    usc2_full = usc2_full,
    usc2_red = usc2_red,
    d_full = d_full,
    d_red = d_red
  )
}

test_that("reduced task bootstrap preserves linear operator and lifted salience", {
  set.seed(2468)
  dat <- matrix(rnorm(12 * 8), 12, 8)
  bootsamp <- cbind(
    1:12,
    c(2:12, 1),
    c(1, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  )
  observed <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 1L,
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  diag <- bootstrap_task_reduced_diagnostic(dat, bootsamp, b = 2L, observed = observed)

  expect_equal(
    diag$cov_full$datamatsvd,
    diag$cov_red$datamatsvd %*% t(pls_svd(dat, handle_missing = FALSE)$v),
    tolerance = 1e-12
  )
  expect_equal(
    diag$cov_full$stacked_smeanmat,
    diag$cov_red$stacked_smeanmat %*% t(pls_svd(dat, handle_missing = FALSE)$v),
    tolerance = 1e-12
  )
  expect_equal(diag$u_lifted, diag$u_full_scaled, tolerance = 1e-12)
})

test_that("reduced task bootstrap CI drift is confined to near-null columns", {
  set.seed(2468)
  dat <- matrix(rnorm(12 * 8), 12, 8)
  bootsamp <- cbind(
    1:12,
    c(2:12, 1),
    c(1, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  )
  observed <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 1L,
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  diag <- bootstrap_task_reduced_diagnostic(dat, bootsamp, b = 2L, observed = observed)

  # Stable LV is exact.
  expect_equal(diag$usc2_red[, 1, drop = FALSE], diag$usc2_full[, 1, drop = FALSE], tolerance = 1e-12)

  # Drift appears only because the second bootstrap singular value is numerically null,
  # so column normalization amplifies arbitrary null-space completion.
  expect_lt(abs(diag$d_full[2]), 1e-12)
  expect_lt(abs(diag$d_red[2]), 1e-12)
  expect_gt(max(abs(diag$usc2_red[, 2, drop = FALSE] - diag$usc2_full[, 2, drop = FALSE])), 1e-6)
})

test_that("zeroing null bootstrap columns restores exact reduced-space CI agreement", {
  set.seed(2468)
  dat <- matrix(rnorm(12 * 8), 12, 8)
  bootsamp <- cbind(
    1:12,
    c(2:12, 1),
    c(1, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  )
  observed <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 1L,
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  cov_full <- pls_get_covcor(
    method = 1L,
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 2L,
    datamat_reorder = bootsamp[, 2L],
    compute_smeanmat = TRUE
  )

  xsvd <- pls_svd(dat, handle_missing = FALSE)
  scores <- xsvd$u %*% diag(xsvd$d, nrow = length(xsvd$d))
  loadings <- xsvd$v

  cov_red <- pls_get_covcor(
    method = 1L,
    stacked_datamat = scores,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 2L,
    datamat_reorder = bootsamp[, 2L],
    compute_smeanmat = TRUE
  )

  n_keep <- 2L
  svd_full <- svd(t(cov_full$datamatsvd), nu = n_keep, nv = n_keep)
  svd_red <- svd(t(cov_red$datamatsvd), nu = n_keep, nv = n_keep)
  tol <- 1e-12 * svd_full$d[1]
  keep_full <- which(svd_full$d > tol)
  keep_red <- which(svd_red$d > tol)

  rot_full <- pls_bootprocrust(observed$v[, keep_full, drop = FALSE], svd_full$v[, keep_full, drop = FALSE])
  rot_red <- pls_bootprocrust(observed$v[, keep_red, drop = FALSE], svd_red$v[, keep_red, drop = FALSE])

  u_full_zeroed <- matrix(0, nrow = nrow(observed$u), ncol = ncol(observed$u))
  u_full_zeroed[, keep_full] <-
    svd_full$u[, keep_full, drop = FALSE] %*%
    diag(svd_full$d[keep_full], nrow = length(keep_full)) %*%
    rot_full

  u_red_zeroed <- matrix(0, nrow = ncol(loadings), ncol = ncol(observed$u))
  u_red_zeroed[, keep_red] <-
    svd_red$u[, keep_red, drop = FALSE] %*%
    diag(svd_red$d[keep_red], nrow = length(keep_red)) %*%
    rot_red

  u_lifted <- loadings %*% u_red_zeroed
  usc2_full <- cov_full$stacked_smeanmat %*% normalize_rows(u_full_zeroed, margin = 2L)
  usc2_red <- cov_red$stacked_smeanmat %*% normalize_rows(u_red_zeroed, margin = 2L)

  expect_equal(u_lifted, u_full_zeroed, tolerance = 1e-12)
  expect_equal(usc2_red, usc2_full, tolerance = 1e-12)
})
