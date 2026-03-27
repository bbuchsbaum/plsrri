# Parity-focused tests for non-GUI MATLAB features:
# - SSB (unequal per-condition n) support
# - Distribution-based bootstrap CIs (ul/ll + adjusted)
# - Split-half permutation validation structure
# - Missing-data (missnk) cross-correlation path

test_that("make_row_indices preserves bscan order for ssb designs", {
  num_cond <- 3L
  num_subj_lst <- list(c(2L, 3L, 1L), c(1L, 2L, 2L))

  idx <- plsrri:::make_row_indices(num_subj_lst, num_cond, condition = c(3L, 1L))
  expect_equal(idx, c(6L, 1:2, 10:11, 7L))
})

test_that("pls_xcor uses missnk logic when inputs contain missing values", {
  design <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 3, ncol = 2)
  datamat <- matrix(c(1, 2, 3, 4, NA, 6), nrow = 3, ncol = 2)

  xcor <- pls_xcor(design, datamat, cormode = 0)
  expect_equal(dim(xcor), c(2, 2))
  expect_true(all(is.finite(xcor)))

  # If an entire column is missing, missmult-style results are missing too
  datamat_all_missing <- datamat
  datamat_all_missing[, 2] <- NA_real_
  xcor2 <- pls_xcor(design, datamat_all_missing, cormode = 0)
  expect_true(all(!is.finite(xcor2[, 2])))
})

test_that("bootstrap CIs are computed for task and behavior PLS", {
  set.seed(123)

  n_subj <- 6L
  n_cond <- 2L
  n_features <- 20L

  datamat <- matrix(rnorm(n_subj * n_cond * n_features), nrow = n_subj * n_cond, ncol = n_features)

  # Method 1: task PLS should produce usc2 + CI matrices
  res_task <- pls_analysis(
    datamat_lst = list(datamat),
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 1L,
    num_boot = 5L,
    progress = FALSE
  )

  br <- res_task$boot_result
  expect_s3_class(br, "pls_boot_result")
  expect_true(!is.null(br$usc2))
  expect_true(!is.null(br$orig_usc))
  expect_true(!is.null(br$llusc))
  expect_true(!is.null(br$ulusc))
  expect_true(!is.null(br$llusc_adj))
  expect_true(!is.null(br$ulusc_adj))
  expect_equal(dim(br$llusc), dim(br$orig_usc))
  expect_equal(dim(br$ulusc), dim(br$orig_usc))

  # Method 3: behavior PLS should produce correlation CIs
  behav <- matrix(rnorm(n_subj * n_cond * 2L), nrow = n_subj * n_cond, ncol = 2L)
  res_beh <- pls_analysis(
    datamat_lst = list(datamat),
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 3L,
    stacked_behavdata = behav,
    num_boot = 5L,
    progress = FALSE
  )

  brb <- res_beh$boot_result
  expect_true(!is.null(brb$orig_corr))
  expect_true(!is.null(brb$llcorr))
  expect_true(!is.null(brb$ulcorr))
  expect_true(!is.null(brb$llcorr_adj))
  expect_true(!is.null(brb$ulcorr_adj))
  expect_equal(dim(brb$ulcorr), dim(brb$orig_corr))
})

test_that("multiblock bootstrap stores task distrib and task CIs", {
  set.seed(99)

  n_subj <- 6L
  n_cond <- 3L
  n_features <- 20L
  n_behav <- 2L

  datamat <- matrix(rnorm(n_subj * n_cond * n_features), nrow = n_subj * n_cond, ncol = n_features)
  behav <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)

  res <- pls_analysis(
    datamat_lst = list(datamat),
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 4L,
    stacked_behavdata = behav,
    bscan = c(1L, 3L),
    num_boot = 5L,
    progress = FALSE
  )

  br <- res$boot_result
  expect_true(!is.null(br$Tdistrib))
  expect_true(!is.null(br$Tprop))
  expect_true(!is.null(br$llusc))
  expect_true(!is.null(br$ulusc))
  expect_equal(dim(br$llusc), dim(br$ulusc))
  expect_equal(dim(br$Tdistrib)[3], br$num_boot + 1L)
})

test_that("pls_analysis supports ssb num_subj_lst list (task + multiblock)", {
  set.seed(42)

  num_cond <- 3L
  num_subj_lst <- list(c(3L, 4L, 3L), c(4L, 3L, 5L))
  n_features <- 10L
  n_behav <- 2L

  datamat_lst <- list(
    matrix(rnorm(sum(num_subj_lst[[1]]) * n_features), nrow = sum(num_subj_lst[[1]]), ncol = n_features),
    matrix(rnorm(sum(num_subj_lst[[2]]) * n_features), nrow = sum(num_subj_lst[[2]]), ncol = n_features)
  )

  total_rows <- sum(vapply(num_subj_lst, sum, integer(1)))

  res_task <- pls_analysis(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    method = 1L,
    num_perm = 2L,
    num_boot = 3L,
    progress = FALSE
  )

  expect_equal(nrow(res_task$usc), total_rows)
  expect_equal(nrow(res_task$vsc), total_rows)
  expect_true(!is.null(res_task$boot_result$ulusc))

  behav <- matrix(rnorm(total_rows * n_behav), nrow = total_rows, ncol = n_behav)
  res_mb <- pls_analysis(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    method = 4L,
    stacked_behavdata = behav,
    bscan = c(1L, 3L),
    num_perm = 2L,
    num_boot = 3L,
    progress = FALSE
  )

  expect_true(!is.null(res_mb$boot_result$ulcorr))
  expect_true(!is.null(res_mb$boot_result$Tdistrib))
})

test_that("split-half returns outer-permutation distributions and p-values", {
  set.seed(7)

  n_subj <- 8L
  n_cond <- 2L
  dat <- matrix(rnorm(n_subj * n_cond * 12L), nrow = n_subj * n_cond, ncol = 12L)

  res <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 1L,
    num_perm = 2L,
    num_split = 3L,
    progress = FALSE
  )

  sh <- res$splithalf_result
  expect_s3_class(sh, "pls_splithalf_result")
  expect_equal(sh$num_outer_perm, 3L) # num_perm + 1 (identity)
  expect_equal(length(sh$orig_ucorr), length(sh$ucorr_prob))
  expect_true(all(sh$ucorr_prob >= 0 & sh$ucorr_prob <= 1, na.rm = TRUE))
})

test_that("split-half row indexing matches MATLAB reshape semantics", {
  num_subj_lst <- c(4L, 3L)
  num_cond <- 2L

  rows <- plsrri:::.pls_splithalf_rows(num_subj_lst, num_cond)
  expect_equal(rows$num_subj_lst1, c(2L, 2L))
  expect_equal(rows$num_subj_lst2, c(2L, 1L))

  # MATLAB reference:
  # g1: reshape(1:8,4,2) => [1 5;2 6;3 7;4 8]; tmp1(:)=[1;2;5;6]
  # g2 offset=8: reshape(1:6,3,2)=>[1 4;2 5;3 6]; tmp1(:)=[1;2;4;5] + 8
  expect_equal(rows$rows1, c(1L, 2L, 5L, 6L, 9L, 10L, 12L, 13L))
  expect_equal(rows$rows2, c(3L, 4L, 7L, 8L, 11L, 14L))
})

test_that("split-half in_reorder matches MATLAB offset + n*(cond-1) + gperm", {
  num_subj_lst <- c(4L, 3L)
  num_cond <- 2L
  gperms <- list(c(4L, 1L, 3L, 2L), c(2L, 3L, 1L))

  in_reorder <- plsrri:::.pls_splithalf_in_reorder(num_subj_lst, num_cond, gperms)

  # Group 1 offset 0:
  # cond1: 4,1,3,2; cond2: 8,5,7,6
  # Group 2 offset 8:
  # cond1: 10,11,9; cond2: 13,14,12
  expect_equal(in_reorder, c(4L, 1L, 3L, 2L, 8L, 5L, 7L, 6L, 10L, 11L, 9L, 13L, 14L, 12L))
})

test_that("split-half respects outer_reorder override", {
  set.seed(1)

  n_subj <- 6L
  n_cond <- 2L
  total_rows <- n_subj * n_cond

  dat <- matrix(rnorm(total_rows * 8L), nrow = total_rows, ncol = 8L)
  outer_reorder <- cbind(seq_len(total_rows), rev(seq_len(total_rows)))

  inner_perms <- list(
    list(list(seq_len(n_subj))),
    list(list(seq_len(n_subj)))
  )

  sh <- pls_splithalf_test(
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 1L,
    num_split = 1L,
    num_outer_perm = 99L,
    outer_reorder = outer_reorder,
    inner_subject_perms = inner_perms,
    progress = FALSE
  )

  expect_equal(sh$num_outer_perm, 2L)
})

test_that("split-half uses provided inner subject permutations", {
  set.seed(2)

  n_subj <- 12L
  n_cond <- 2L
  total_rows <- n_subj * n_cond

  dat <- matrix(rnorm(total_rows * 40L), nrow = total_rows, ncol = 40L)
  outer_reorder <- matrix(seq_len(total_rows), ncol = 1L)

  # Permutations must shuffle subjects so different subjects land in each half
  perms_a <- list(list(list(seq_len(n_subj))))
  perms_b <- list(list(list(c(2L, 5L, 1L, 11L, 8L, 3L, 12L, 4L, 10L, 6L, 9L, 7L))))

  sh_a <- pls_splithalf_test(
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 1L,
    num_split = 1L,
    num_outer_perm = 1L,
    outer_reorder = outer_reorder,
    inner_subject_perms = perms_a,
    progress = FALSE
  )

  sh_b <- pls_splithalf_test(
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 1L,
    num_split = 1L,
    num_outer_perm = 1L,
    outer_reorder = outer_reorder,
    inner_subject_perms = perms_b,
    progress = FALSE
  )

  # Different inner permutations assign different subjects to each half,
  # producing different split-half correlations.
  expect_false(isTRUE(all.equal(sh_a$orig_ucorr, sh_b$orig_ucorr)))
})

test_that("nonrotated_boot is auto-enabled when num_split > 0 (MATLAB parity)", {
  set.seed(101)

  n_subj <- 8L
  n_cond <- 2L
  n_features <- 30L

  dat <- matrix(rnorm(n_subj * n_cond * n_features),
                nrow = n_subj * n_cond, ncol = n_features)
  des <- diag(n_cond)

  # Baseline: method 2 without split-half uses unnormalized u (= crossblock')
  res0 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 2L,
    stacked_designdata = des,
    num_boot = 3L,
    num_split = 0L,
    progress = FALSE
  )
  expect_s3_class(res0$boot_result, "pls_boot_result")
  expect_false(isTRUE(res0$boot_result$nonrotated_boot))
  norms0 <- sqrt(colSums(res0$u^2))
  expect_false(all(abs(norms0 - 1) < 1e-10))

  # MATLAB behavior: requesting split-half enables nonrotated_boot, which
  # normalizes u for methods 2/5/6 and changes bootstrap behavior.
  res1 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 2L,
    stacked_designdata = des,
    num_boot = 3L,
    num_split = 1L,
    progress = FALSE
  )
  expect_s3_class(res1$boot_result, "pls_boot_result")
  expect_true(isTRUE(res1$boot_result$nonrotated_boot))
  norms1 <- sqrt(colSums(res1$u^2))
  expect_true(all(abs(norms1 - 1) < 1e-10))
})

test_that("behavior bootstrap resamples when condition SD is zero (badbeh)", {
  set.seed(202)

  n_subj <- 10L  # > 8 so bootstrap is random (no enumeration)
  n_cond <- 2L
  n_features <- 25L

  dat <- matrix(rnorm(n_subj * n_cond * n_features),
                nrow = n_subj * n_cond, ncol = n_features)

  # Binary behavior with rare 1s; some bootstrap draws will have SD=0 in a
  # condition and should trigger resampling.
  beh <- c(
    c(rep(0, n_subj - 1L), 1), # cond 1: subject 10 is 1
    c(1, rep(0, n_subj - 1L))  # cond 2: subject 1 is 1
  )
  beh <- matrix(beh, ncol = 1)

  res <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(n_subj),
    num_cond = n_cond,
    method = 3L,
    stacked_behavdata = beh,
    num_boot = 5L,
    progress = FALSE
  )

  br <- res$boot_result
  expect_s3_class(br, "pls_boot_result")
  expect_true(isTRUE(br$countnewtotal >= 1L))
  expect_true(is.list(br$badbeh) && length(br$badbeh) == 1L)
  expect_true(is.integer(br$num_LowVariability_behav_boots) || is.numeric(br$num_LowVariability_behav_boots))
  expect_equal(length(br$num_LowVariability_behav_boots), 1L)

  has_matrix <- any(vapply(br$badbeh[[1]], is.matrix, logical(1)))
  expect_true(has_matrix)
})
