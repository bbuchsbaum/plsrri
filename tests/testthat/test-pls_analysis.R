# Tests for core PLS analysis functions

test_that("pls_analysis runs with basic inputs", {
  set.seed(42)

  # Create test data: 2 groups, 3 conditions, 10 subjects each, 50 features
  n_subj <- 10
  n_cond <- 3
  n_features <- 50

  datamat1 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)
  datamat2 <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)

  result <- pls_analysis(
    datamat_lst = list(datamat1, datamat2),
    num_subj_lst = c(n_subj, n_subj),
    num_cond = n_cond,
    method = 1,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_s3_class(result, "pls_task")
  expect_equal(result$method, 1L)
  expect_equal(length(result$s), n_cond * 2)  # num_groups * num_cond
  expect_equal(nrow(result$u), n_features)
  expect_equal(ncol(result$u), n_cond * 2)
})

test_that("pls_analysis singular values are positive and sorted", {
  set.seed(123)

  datamat1 <- matrix(rnorm(30 * 100), 30, 100)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    progress = FALSE
  )

  expect_true(all(result$s >= 0))
  expect_equal(result$s, sort(result$s, decreasing = TRUE))
})

test_that("pls_analysis with permutation test", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_perm = 10,  # Small for speed
    progress = FALSE
  )

  expect_s3_class(result$perm_result, "pls_perm_result")
  expect_equal(result$perm_result$num_perm, 10)
  expect_true(all(result$perm_result$sprob >= 0))
  expect_true(all(result$perm_result$sprob <= 1))
})

test_that("pls_analysis with bootstrap test", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_boot = 10,  # Small for speed
    progress = FALSE
  )

  expect_s3_class(result$boot_result, "pls_boot_result")
  expect_equal(nrow(result$boot_result$compare_u), 50)
  expect_true(all(is.finite(result$boot_result$compare_u)))
})

test_that("pls_analysis uses caller-supplied resampling orders", {
  set.seed(42)

  datamat <- matrix(rnorm(12 * 10), nrow = 12, ncol = 10)
  behav <- matrix(rnorm(12 * 2), nrow = 12, ncol = 2)

  Tpermsamp <- cbind(
    1:12,
    c(2:12, 1),
    c(12, 1:11)
  )
  Bpermsamp <- cbind(
    c(12:1),
    c(6:12, 1:5),
    c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)
  )
  bootsamp <- cbind(
    1:12,
    c(3:12, 1:2),
    c(5:12, 1:4)
  )
  bootsamp_4beh <- cbind(
    c(2:12, 1),
    c(4:12, 1:3),
    c(6:12, 1:5)
  )

  result <- pls_analysis(
    datamat_lst = list(datamat),
    num_subj_lst = 6L,
    num_cond = 2L,
    method = 4L,
    stacked_behavdata = behav,
    num_perm = 3L,
    num_boot = 3L,
    bscan = c(1L, 2L),
    Tpermsamp = Tpermsamp,
    Bpermsamp = Bpermsamp,
    bootsamp = bootsamp,
    bootsamp_4beh = bootsamp_4beh,
    progress = FALSE
  )

  expect_equal(result$perm_result$Tpermsamp, Tpermsamp)
  expect_equal(result$perm_result$Bpermsamp, Bpermsamp)
  expect_equal(result$boot_result$bootsamp, bootsamp)
  expect_equal(result$boot_result$bootsamp_4beh, bootsamp_4beh)
})

test_that("pls_analysis method 3 (behavior) requires behavdata", {
  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  expect_error(
    pls_analysis(
      datamat_lst = list(datamat1),
      num_subj_lst = 10,
      num_cond = 3,
      method = 3,
      progress = FALSE
    ),
    "stacked_behavdata is required"
  )
})

test_that("pls_analysis method 3 (behavior) works with behavdata", {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)
  behavdata <- matrix(rnorm(30 * 2), 30, 2)  # 2 behavior measures

  result <- pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 3,
    stacked_behavdata = behavdata,
    progress = FALSE
  )

  expect_s3_class(result, "pls_behavior")
  expect_equal(result$method, 3L)
  expect_true(!is.null(result$lvcorrs))
  expect_true(!is.null(result$vsc))
  expect_equal(nrow(result$vsc), nrow(behavdata))
  expect_equal(ncol(result$vsc), length(result$s))
})
