local_options <- function(...) {
  old <- options(...)
  withr::defer(options(old), testthat::teardown_env())
  invisible(old)
}

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

test_that("balanced task bootstrap fast path matches slow path exactly", {
  set.seed(321)
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

  local_options(plsrri.fast_paths = FALSE)
  slow <- pls_bootstrap_test(
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
    progress = FALSE
  )

  local_options(plsrri.fast_paths = TRUE)
  fast <- pls_bootstrap_test(
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
    progress = FALSE
  )

  expect_equal(fast$compare_u, slow$compare_u, tolerance = 1e-10)
  expect_equal(fast$u_se, slow$u_se, tolerance = 1e-10)
  expect_equal(fast$bootsamp, slow$bootsamp, tolerance = 0)
  expect_equal(fast$usc2, slow$usc2, tolerance = 1e-10)
  expect_equal(fast$orig_usc, slow$orig_usc, tolerance = 1e-10)
  expect_equal(fast$distrib, slow$distrib, tolerance = 1e-10)
  expect_equal(fast$ulusc, slow$ulusc, tolerance = 1e-10)
  expect_equal(fast$llusc, slow$llusc, tolerance = 1e-10)
})
