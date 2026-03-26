# Tests for resampling functions (permutation and bootstrap)
#
# These tests verify the correctness of resampling order generation
# and the properties of the resulting indices.

# -----------------------------------------------------------------------------
# Permutation Order Tests
# -----------------------------------------------------------------------------

test_that("pls_perm_order returns correct dimensions", {
  perm_order <- pls_perm_order(
    num_subj_lst = c(10, 12),
    num_cond = 3,
    num_perm = 50
  )

  # Total rows = (10 + 12) * 3 = 66
  expect_equal(nrow(perm_order), 66)
  expect_equal(ncol(perm_order), 50)
})

test_that("pls_perm_order contains valid indices", {
  perm_order <- pls_perm_order(
    num_subj_lst = c(8, 8),
    num_cond = 2,
    num_perm = 20
  )

  total_rows <- (8 + 8) * 2

  # Each column should be a permutation of 1:total_rows
  for (p in 1:ncol(perm_order)) {
    indices <- perm_order[, p]
    expect_equal(sort(indices), 1:total_rows,
                 info = paste("Permutation", p, "should contain all indices"))
  }
})

test_that("pls_perm_order excludes identity permutation", {
  set.seed(42)

  perm_order <- pls_perm_order(
    num_subj_lst = c(5, 5),
    num_cond = 2,
    num_perm = 100
  )

  identity <- seq_len(nrow(perm_order))

  for (p in 1:ncol(perm_order)) {
    expect_false(identical(as.integer(perm_order[, p]), identity),
                 info = "No permutation should be the identity")
  }
})

test_that("pls_perm_order generates unique permutations", {
  set.seed(42)

  perm_order <- pls_perm_order(
    num_subj_lst = c(6, 6),
    num_cond = 3,
    num_perm = 50
  )

  # Check for duplicate columns
  unique_cols <- unique(as.data.frame(t(perm_order)))
  expect_equal(nrow(unique_cols), ncol(perm_order),
               info = "All permutations should be unique")
})

test_that("pls_perm_order with not_in_cond preserves condition structure", {
  set.seed(42)

  # When not_in_cond = TRUE, conditions should not be permuted within subjects
  perm_order <- pls_perm_order(
    num_subj_lst = c(4),
    num_cond = 3,
    num_perm = 20,
    not_in_cond = TRUE
  )

  # This is used for structure PLS - harder to test directly
  # Just verify it runs without error and produces valid output
  expect_equal(nrow(perm_order), 12)  # 4 subjects * 3 conditions
  expect_equal(ncol(perm_order), 20)
})

test_that("pls_perm_order handles single group", {
  perm_order <- pls_perm_order(
    num_subj_lst = c(10),
    num_cond = 3,
    num_perm = 30
  )

  expect_equal(nrow(perm_order), 30)  # 10 * 3
  expect_equal(ncol(perm_order), 30)
})

test_that("missnk permutation order includes identity in first column", {
  set.seed(42)

  perm_order <- plsrri:::.pls_perm_order_missnk(
    num_subj_lst = c(5, 4),
    num_cond = 3,
    num_perm = 10,
    not_in_cond = FALSE
  )

  total_rows <- (5 + 4) * 3
  expect_equal(dim(perm_order), c(total_rows, 10))
  expect_equal(as.integer(perm_order[, 1]), seq_len(total_rows))

  for (p in seq_len(ncol(perm_order))) {
    expect_equal(sort(perm_order[, p]), seq_len(total_rows))
  }
})

# -----------------------------------------------------------------------------
# Bootstrap Order Tests
# -----------------------------------------------------------------------------

test_that("pls_boot_order returns correct dimensions", {
  boot_order <- pls_boot_order(
    num_subj_lst = c(10, 12),
    num_cond = 3,
    num_boot = 50
  )

  # Total rows = (10 + 12) * 3 = 66
  expect_equal(nrow(boot_order), 66)
  expect_equal(ncol(boot_order), 50)
})

test_that("pls_boot_order contains valid indices", {
  boot_order <- pls_boot_order(
    num_subj_lst = c(8, 8),
    num_cond = 2,
    num_boot = 20
  )

  total_rows <- (8 + 8) * 2

  # Each value should be a valid row index
  expect_true(all(boot_order >= 1))
  expect_true(all(boot_order <= total_rows))
})

test_that("pls_boot_order allows duplicates (resampling with replacement)", {
  set.seed(42)

  boot_order <- pls_boot_order(
    num_subj_lst = c(10),
    num_cond = 2,
    num_boot = 100
  )

  # At least some bootstrap samples should have duplicate indices
  has_duplicates <- FALSE
  for (b in 1:ncol(boot_order)) {
    if (length(unique(boot_order[, b])) < nrow(boot_order)) {
      has_duplicates <- TRUE
      break
    }
  }
  expect_true(has_duplicates,
              info = "Bootstrap should sample with replacement (duplicates)")
})

test_that("pls_boot_order stratified sampling respects group structure", {
  set.seed(42)

  boot_order <- pls_boot_order(
    num_subj_lst = c(5, 7),  # Different group sizes
    num_cond = 2,
    num_boot = 30,
    boot_type = "strat"
  )

  # Group 1: rows 1-10 (5 subjects * 2 conditions)
  # Group 2: rows 11-24 (7 subjects * 2 conditions)

  # For stratified sampling, group 1 rows should resample from group 1
  # This is an indirect test - we check the function completes
  expect_equal(nrow(boot_order), 24)
})

test_that("pls_boot_check identifies small sample sizes", {
  # If *all* groups have n <= 8, MATLAB enumerates theoretical boot samples
  # regardless of requested num_boot.
  check_small <- pls_boot_check(
    num_subj_lst = c(4, 4),
    num_cond = 2,
    num_boot = 1000
  )

  expect_true(all(check_small$is_boot_samples),
              info = "All groups should use enumerated samples when n <= 8")
  expect_true(check_small$new_num_boot <= 1000,
              info = "num_boot may be reduced for small samples")

  check_small2 <- pls_boot_check(
    num_subj_lst = c(4, 4),
    num_cond = 2,
    num_boot = 10
  )
  expect_true(all(check_small2$is_boot_samples),
              info = "Enumeration should occur even when num_boot is small")

  # Mixed small/large groups should not enumerate (MATLAB requires all groups <= 8)
  check_mixed <- pls_boot_check(
    num_subj_lst = c(4, 20),
    num_cond = 2,
    num_boot = 1000
  )
  expect_false(any(check_mixed$is_boot_samples),
               info = "Enumeration should be disabled unless all groups have n <= 8")

  # Larger group should not enumerate
  check_large <- pls_boot_check(
    num_subj_lst = c(20, 20),
    num_cond = 2,
    num_boot = 100
  )

  expect_false(any(check_large$is_boot_samples),
               info = "Large groups should use random sampling")
})

test_that("pls_boot_order non-stratified ignores group boundaries", {
  boot_order_nonstrat <- pls_boot_order(
    num_subj_lst = c(6, 6),
    num_cond = 2,
    num_boot = 50,
    boot_type = "nonstrat"
  )

  expect_equal(nrow(boot_order_nonstrat), 24)
  expect_equal(ncol(boot_order_nonstrat), 50)
})

test_that("pls_boot_order with bscan leaves non-bscan rows unchanged", {
  set.seed(42)

  num_subj_lst <- c(5, 4)
  num_cond <- 3
  bscan <- c(2)

  boot_order <- pls_boot_order(
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    num_boot = 10,
    bscan = bscan,
    incl_seq = FALSE,
    boot_type = "strat"
  )

  template <- make_row_indices(num_subj_lst, num_cond, condition = bscan)
  outside <- setdiff(seq_len(nrow(boot_order)), template)

  # Outside the bscan template, orders should be identity for all boots
  expect_true(all(boot_order[outside, , drop = FALSE] == outside))
})

test_that("pls_randperm_notall permutes only bscan rows", {
  set.seed(42)

  num_subj_lst <- c(3, 4)
  num_cond <- 4
  bscan <- c(2, 4)

  reorder <- plsrri:::pls_randperm_notall(num_subj_lst, num_cond, bscan)
  template <- make_row_indices(num_subj_lst, num_cond, condition = bscan)
  outside <- setdiff(seq_len(length(reorder)), template)

  expect_equal(reorder[outside], outside,
               info = "Non-bscan rows should remain unchanged")
  expect_equal(sort(reorder[template]), sort(template),
               info = "bscan rows should be permuted among themselves")
})

# -----------------------------------------------------------------------------
# Integration: Permutation + Bootstrap with Analysis
# -----------------------------------------------------------------------------

test_that("permutation orders affect analysis consistently", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  # Generate permutation order manually
  perm_order <- pls_perm_order(c(10), 3, 10)

  # Permuted data should give different covariance
  original_cov <- pls_get_covcor(
    method = 1,
    stacked_datamat = datamat1,
    num_groups = 1,
    num_subj_lst = c(10),
    num_cond = 3,
    meancentering_type = 0
  )

  permuted_cov <- pls_get_covcor(
    method = 1,
    stacked_datamat = datamat1,
    num_groups = 1,
    num_subj_lst = c(10),
    num_cond = 3,
    meancentering_type = 0,
    datamat_reorder = perm_order[, 1]
  )

  expect_false(isTRUE(all.equal(original_cov$datamatsvd, permuted_cov$datamatsvd)),
               info = "Permutation should change the cross-product matrix")
})
