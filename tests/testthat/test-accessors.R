# Tests for result accessor functions

test_that("salience extractor works", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  sal <- salience(result)
  expect_equal(dim(sal), c(50, 3))

  sal_lv1 <- salience(result, lv = 1)
  expect_equal(dim(sal_lv1), c(50, 1))

  sal_multi <- salience(result, lv = c(1, 2))
  expect_equal(dim(sal_multi), c(50, 2))
})

test_that("bsr requires bootstrap", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_boot = 0,
    progress = FALSE
  )

  expect_error(bsr(result), "No bootstrap result")
})

test_that("bsr works with bootstrap", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_boot = 10,
    progress = FALSE
  )

  bsr_vals <- bsr(result)
  expect_equal(dim(bsr_vals), c(50, 3))

  bsr_thresh <- bsr(result, threshold = 2)
  expect_true(all(bsr_thresh == 0 | abs(bsr_thresh) >= 2))
})

test_that("scores extractor works", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  brain_sc <- scores(result, type = "brain")
  expect_equal(nrow(brain_sc), 30)  # Total observations
  expect_equal(ncol(brain_sc), 3)   # Number of LVs

  design_sc <- scores(result, type = "design")
  expect_equal(nrow(design_sc), 30)
})

test_that("significance extractor works", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_perm = 10,
    progress = FALSE
  )

  pvals <- significance(result)
  expect_equal(length(pvals), 3)
  expect_true(all(pvals >= 0 & pvals <= 1))

  sig_df <- significance(result, threshold = 0.05)
  expect_true(is.data.frame(sig_df))
  expect_true("significant" %in% names(sig_df))
})

test_that("singular_values extractor works", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  sv <- singular_values(result)
  expect_equal(length(sv), 3)
  expect_true(all(sv >= 0))

  var_exp <- singular_values(result, normalize = TRUE)
  expect_equal(sum(var_exp), 100, tolerance = 1e-10)
})

test_that("n_lv and n_features work", {
  set.seed(42)
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  expect_equal(n_lv(result), 3)
  expect_equal(n_features(result), 50)
})

# ---- quick_pls-based fixture for S3 method coverage ----

test_that("quick_pls result has correct class and S3 methods dispatch", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 10, nboot = 10, progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(n_lv(result), 3)
  expect_equal(n_features(result), 10)
})

test_that("salience via quick_pls: all LVs and single LV", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 10, nboot = 10, progress = FALSE
  )

  sal_all <- salience(result)
  expect_equal(dim(sal_all), c(10, 3))

  sal_lv1 <- salience(result, lv = 1)
  expect_equal(dim(sal_lv1), c(10, 1))
  expect_equal(sal_lv1[, 1], sal_all[, 1])
})

test_that("salience errors on out-of-range lv", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 10, nboot = 10, progress = FALSE
  )

  expect_error(salience(result, lv = 99), "out of range")
})

test_that("bsr via quick_pls: lv=1 and threshold", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 10, nboot = 10, progress = FALSE
  )

  bsr_lv1 <- bsr(result, lv = 1)
  expect_equal(dim(bsr_lv1), c(10, 1))

  bsr_thresh <- bsr(result, threshold = 2)
  expect_true(all(bsr_thresh == 0 | abs(bsr_thresh) >= 2))
})

test_that("scores via quick_pls: lv subsetting", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 10, nboot = 10, progress = FALSE
  )

  sc_lv1 <- scores(result, type = "brain", lv = 1)
  expect_equal(ncol(sc_lv1), 1L)
  expect_equal(nrow(sc_lv1), 12L)

  sc_design <- scores(result, type = "design")
  expect_equal(ncol(sc_design), 3L)
})

test_that("loadings via quick_pls returns design loadings", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 10, nboot = 10, progress = FALSE
  )

  ld <- loadings(result, type = "design")
  expect_true(is.matrix(ld))
  expect_equal(ncol(ld), 3L)

  ld_lv1 <- loadings(result, type = "design", lv = 1)
  expect_equal(ncol(ld_lv1), 1L)
})

test_that("significance via quick_pls: vector and data.frame forms", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 10, nboot = 10, progress = FALSE
  )

  pv <- significance(result)
  expect_equal(length(pv), 3L)
  expect_true(all(pv >= 0 & pv <= 1))
  expect_equal(names(pv), c("LV1", "LV2", "LV3"))

  sig_df <- significance(result, threshold = 0.05)
  expect_true(is.data.frame(sig_df))
  expect_true(all(c("lv", "pvalue", "significant", "var_explained") %in% names(sig_df)))

  pv_lv1 <- significance(result, lv = 1)
  expect_equal(length(pv_lv1), 1L)
})

test_that("significance errors when no permutation result", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 0, nboot = 0, progress = FALSE
  )

  expect_error(significance(result), "No permutation result")
})

test_that("confidence via quick_pls: salience CIs", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 10, nboot = 10, progress = FALSE
  )

  ci <- confidence(result, what = "salience")
  expect_named(ci, c("lower", "upper", "clim"))
  expect_equal(dim(ci$lower), c(10, 3))
  expect_equal(dim(ci$upper), c(10, 3))
})

test_that("confidence errors when no bootstrap result", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 0, nboot = 0, progress = FALSE
  )

  expect_error(confidence(result, what = "salience"), "No bootstrap result")
})

test_that("bsr errors when no bootstrap result", {
  set.seed(42)
  result <- quick_pls(
    list(matrix(rnorm(120), 12, 10)),
    4, 3,
    nperm = 0, nboot = 0, progress = FALSE
  )

  expect_error(bsr(result), "No bootstrap result")
})
