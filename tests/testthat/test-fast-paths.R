local_options <- function(...) {
  old <- options(...)
  withr::defer(options(old), testthat::teardown_env())
  invisible(old)
}

test_that("default fast-path option enables xcor/bootstrap but not permutation", {
  local_options(plsrri.fast_paths = NULL)

  expect_true(plsrri:::.plsrri_fast_paths_enabled("xcor"))
  expect_true(plsrri:::.plsrri_fast_paths_enabled("bootstrap"))
  expect_false(plsrri:::.plsrri_fast_paths_enabled("permutation"))
})

test_that("finite pls_xcor fast path matches reference implementation", {
  set.seed(42)
  design <- matrix(rnorm(40), nrow = 10, ncol = 4)
  datamat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  datamat[, 6] <- 1

  for (mode in c(0L, 2L, 4L, 6L)) {
    local_options(plsrri.fast_paths = FALSE)
    expected <- pls_xcor(design, datamat, cormode = mode)

    local_options(plsrri.fast_paths = TRUE)
    actual <- pls_xcor(design, datamat, cormode = mode)

    expect_equal(actual, expected, tolerance = 1e-12)
  }
})

test_that("balanced task permutation fast path matches slow path exactly", {
  set.seed(123)
  dat <- matrix(rnorm(12 * 9), nrow = 12, ncol = 9)
  permsamp <- cbind(
    c(2:12, 1),
    c(3:12, 1:2),
    c(12, 1:11)
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

  local_options(plsrri.fast_paths = FALSE)
  slow <- pls_permutation_test(
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 1L,
    num_perm = ncol(permsamp),
    observed_s = observed$s,
    observed_v = observed$v,
    meancentering_type = 0L,
    permsamp = permsamp,
    progress = FALSE
  )

  local_options(plsrri.fast_paths = TRUE)
  fast <- pls_permutation_test(
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 1L,
    num_perm = ncol(permsamp),
    observed_s = observed$s,
    observed_v = observed$v,
    meancentering_type = 0L,
    permsamp = permsamp,
    progress = FALSE
  )

  expect_equal(fast$sp, slow$sp, tolerance = 0)
  expect_equal(fast$sprob, slow$sprob, tolerance = 0)
  expect_equal(fast$permsamp, slow$permsamp, tolerance = 0)
})

test_that("balanced task permutation parallel path matches sequential path exactly", {
  skip_if_not_installed("future")

  set.seed(456)
  dat <- matrix(rnorm(18 * 7), nrow = 18, ncol = 7)
  permsamp <- cbind(
    c(2:18, 1),
    c(3:18, 1:2),
    c(18, 1:17),
    c(1, 3:18, 2)
  )

  observed <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 3L,
    method = 1L,
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  local_options(plsrri.fast_paths = TRUE)
  expected <- pls_permutation_test(
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 3L,
    method = 1L,
    num_perm = ncol(permsamp),
    observed_s = observed$s,
    observed_v = observed$v,
    meancentering_type = 0L,
    permsamp = permsamp,
    parallel_config = list(backend = "sequential", workers = 1L),
    progress = FALSE
  )

  actual <- pls_permutation_test(
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 3L,
    method = 1L,
    num_perm = ncol(permsamp),
    observed_s = observed$s,
    observed_v = observed$v,
    meancentering_type = 0L,
    permsamp = permsamp,
    parallel_config = list(backend = "future", workers = 2L),
    progress = FALSE
  )

  expect_equal(actual$sp, expected$sp, tolerance = 0)
  expect_equal(actual$sprob, expected$sprob, tolerance = 0)
  expect_equal(actual$permsamp, expected$permsamp, tolerance = 0)
})

test_that("task bootstrap parallel path matches sequential path exactly", {
  skip_if_not_installed("future")

  set.seed(789)
  dat <- matrix(rnorm(12 * 8), nrow = 12, ncol = 8)
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

  expected <- pls_bootstrap_test(
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 1L,
    num_boot = ncol(bootsamp),
    observed_u = observed$u,
    observed_v = observed$v,
    observed_s = observed$s,
    bootsamp = bootsamp,
    clim = 95,
    parallel_config = list(backend = "sequential", workers = 1L),
    progress = FALSE
  )

  actual <- pls_bootstrap_test(
    stacked_datamat = dat,
    num_groups = 1L,
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 1L,
    num_boot = ncol(bootsamp),
    observed_u = observed$u,
    observed_v = observed$v,
    observed_s = observed$s,
    bootsamp = bootsamp,
    clim = 95,
    parallel_config = list(backend = "future", workers = 2L),
    progress = FALSE
  )

  expect_equal(actual$compare_u, expected$compare_u, tolerance = 1e-12)
  expect_equal(actual$u_se, expected$u_se, tolerance = 1e-12)
  expect_equal(actual$distrib, expected$distrib, tolerance = 1e-12)
  expect_equal(actual$ulusc, expected$ulusc, tolerance = 1e-12)
  expect_equal(actual$llusc, expected$llusc, tolerance = 1e-12)
  expect_equal(actual$bootsamp, expected$bootsamp, tolerance = 0)
})

test_that("balanced task bootstrap reduced fast path matches full path exactly", {
  set.seed(24601)
  dat1 <- matrix(rnorm(8 * 9), nrow = 8, ncol = 9)
  dat2 <- matrix(rnorm(10 * 9), nrow = 10, ncol = 9)
  bootsamp <- pls_boot_order(
    num_subj_lst = c(4L, 5L),
    num_cond = 2L,
    num_boot = 4L,
    incl_seq = TRUE,
    boot_type = "strat"
  )

  observed <- pls_analysis(
    datamat_lst = list(dat1, dat2),
    num_subj_lst = c(4L, 5L),
    num_cond = 2L,
    method = 1L,
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  stacked <- rbind(dat1, dat2)

  local_options(plsrri.fast_paths = FALSE)
  expected <- pls_bootstrap_test(
    stacked_datamat = stacked,
    num_groups = 2L,
    num_subj_lst = c(4L, 5L),
    num_cond = 2L,
    method = 1L,
    num_boot = ncol(bootsamp),
    observed_u = observed$u,
    observed_v = observed$v,
    observed_s = observed$s,
    bootsamp = bootsamp,
    clim = 95,
    parallel_config = list(backend = "sequential", workers = 1L),
    progress = FALSE
  )

  local_options(plsrri.fast_paths = TRUE)
  actual <- pls_bootstrap_test(
    stacked_datamat = stacked,
    num_groups = 2L,
    num_subj_lst = c(4L, 5L),
    num_cond = 2L,
    method = 1L,
    num_boot = ncol(bootsamp),
    observed_u = observed$u,
    observed_v = observed$v,
    observed_s = observed$s,
    bootsamp = bootsamp,
    clim = 95,
    parallel_config = list(backend = "sequential", workers = 1L),
    progress = FALSE
  )

  expect_equal(actual$compare_u, expected$compare_u, tolerance = 1e-12)
  expect_equal(actual$u_se, expected$u_se, tolerance = 1e-12)
  expect_equal(actual$distrib, expected$distrib, tolerance = 1e-12)
  expect_equal(actual$ulusc, expected$ulusc, tolerance = 1e-12)
  expect_equal(actual$llusc, expected$llusc, tolerance = 1e-12)
  expect_equal(actual$bootsamp, expected$bootsamp, tolerance = 0)
})

test_that("balanced non-rotated task bootstrap reduced fast path matches full path exactly", {
  set.seed(531)
  dat1 <- matrix(rnorm(8 * 7), nrow = 8, ncol = 7)
  dat2 <- matrix(rnorm(10 * 7), nrow = 10, ncol = 7)
  stacked <- rbind(dat1, dat2)
  design <- matrix(
    c(
      1, 0,
      0, 1,
      1, 0,
      0, 1
    ),
    nrow = 4,
    byrow = TRUE
  )
  bootsamp <- pls_boot_order(
    num_subj_lst = c(4L, 5L),
    num_cond = 2L,
    num_boot = 4L,
    incl_seq = FALSE,
    boot_type = "strat"
  )

  observed <- pls_analysis(
    datamat_lst = list(dat1, dat2),
    num_subj_lst = c(4L, 5L),
    num_cond = 2L,
    method = 2L,
    stacked_designdata = design,
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  local_options(plsrri.fast_paths = FALSE)
  expected <- pls_bootstrap_test(
    stacked_datamat = stacked,
    stacked_designdata = normalize_rows(design, margin = 2L),
    num_groups = 2L,
    num_subj_lst = c(4L, 5L),
    num_cond = 2L,
    method = 2L,
    num_boot = ncol(bootsamp),
    observed_u = observed$u,
    observed_v = observed$v,
    observed_s = observed$s,
    bootsamp = bootsamp,
    clim = 95,
    parallel_config = list(backend = "sequential", workers = 1L),
    progress = FALSE
  )

  local_options(plsrri.fast_paths = TRUE)
  actual <- pls_bootstrap_test(
    stacked_datamat = stacked,
    stacked_designdata = normalize_rows(design, margin = 2L),
    num_groups = 2L,
    num_subj_lst = c(4L, 5L),
    num_cond = 2L,
    method = 2L,
    num_boot = ncol(bootsamp),
    observed_u = observed$u,
    observed_v = observed$v,
    observed_s = observed$s,
    bootsamp = bootsamp,
    clim = 95,
    parallel_config = list(backend = "sequential", workers = 1L),
    progress = FALSE
  )

  expect_equal(actual$compare_u, expected$compare_u, tolerance = 1e-12)
  expect_equal(actual$u_se, expected$u_se, tolerance = 1e-12)
  expect_equal(actual$distrib, expected$distrib, tolerance = 1e-12)
  expect_equal(actual$ulusc, expected$ulusc, tolerance = 1e-12)
  expect_equal(actual$llusc, expected$llusc, tolerance = 1e-12)
  expect_equal(actual$bootsamp, expected$bootsamp, tolerance = 0)
})
