# Tests for builder_run.R: seed_pls, behav_pls, .validate_spec error paths

# --- quick_pls ---

test_that("quick_pls returns pls_result with expected LV count", {
  set.seed(42)
  data1 <- matrix(rnorm(12 * 10), 12, 10)  # 4 subj x 3 cond

  result <- quick_pls(
    list(data1),
    num_subj_lst = 4,
    num_cond     = 3,
    nperm        = 5,
    nboot        = 5,
    progress     = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(n_lv(result), 3L)
  expect_equal(n_features(result), 10L)
})

test_that("quick_pls with two groups returns stacked result", {
  set.seed(42)
  data1 <- matrix(rnorm(12 * 10), 12, 10)
  data2 <- matrix(rnorm(9  * 10),  9, 10)

  result <- quick_pls(
    list(data1, data2),
    num_subj_lst = c(4, 3),
    num_cond     = 3,
    nperm        = 5,
    nboot        = 5,
    progress     = FALSE
  )

  expect_s3_class(result, "pls_result")
})

# --- seed_pls ---

test_that("seed_pls runs with synthetic seed data", {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 2L
  n_vox   <- 15L
  n_seeds <- 2L

  datamat   <- matrix(rnorm(n_subj * n_cond * n_vox),   nrow = n_subj * n_cond, ncol = n_vox)
  seed_data <- matrix(rnorm(n_subj * n_cond * n_seeds), nrow = n_subj * n_cond, ncol = n_seeds)

  result <- seed_pls(
    datamat_lst  = list(datamat),
    seed_data    = seed_data,
    num_subj_lst = n_subj,
    num_cond     = n_cond,
    nperm        = 5,
    nboot        = 5,
    progress     = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(n_features(result), n_vox)
})

test_that("seed_pls errors when seed_data row count mismatches", {
  set.seed(42)
  datamat   <- matrix(rnorm(60), 12, 5)
  seed_data <- matrix(rnorm(10),  5, 2)  # wrong number of rows

  expect_error(
    seed_pls(
      datamat_lst  = list(datamat),
      seed_data    = seed_data,
      num_subj_lst = 6,
      num_cond     = 2,
      nperm        = 0,
      nboot        = 0,
      progress     = FALSE
    ),
    "rows"
  )
})

# --- behav_pls ---

test_that("behav_pls runs with synthetic behavior data", {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 2L
  n_vox   <- 15L
  n_behav <- 3L

  datamat    <- matrix(rnorm(n_subj * n_cond * n_vox),   nrow = n_subj * n_cond, ncol = n_vox)
  behav_data <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)

  result <- behav_pls(
    datamat_lst  = list(datamat),
    behav_data   = behav_data,
    num_subj_lst = n_subj,
    num_cond     = n_cond,
    nperm        = 5,
    nboot        = 5,
    progress     = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_equal(n_features(result), n_vox)
})

test_that("behav_pls errors when behav_data row count mismatches", {
  set.seed(42)
  datamat    <- matrix(rnorm(60), 12, 5)
  behav_data <- matrix(rnorm(10),  5, 2)  # wrong number of rows

  expect_error(
    behav_pls(
      datamat_lst  = list(datamat),
      behav_data   = behav_data,
      num_subj_lst = 6,
      num_cond     = 2,
      nperm        = 0,
      nboot        = 0,
      progress     = FALSE
    ),
    "rows"
  )
})

test_that("behav_pls with covariance cormode runs without error", {
  set.seed(42)
  n_subj  <- 6L
  n_cond  <- 2L
  n_vox   <- 10L
  n_behav <- 2L

  datamat    <- matrix(rnorm(n_subj * n_cond * n_vox),   nrow = n_subj * n_cond, ncol = n_vox)
  behav_data <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)

  result <- behav_pls(
    datamat_lst  = list(datamat),
    behav_data   = behav_data,
    num_subj_lst = n_subj,
    num_cond     = n_cond,
    cormode      = 2L,
    nperm        = 0,
    nboot        = 0,
    progress     = FALSE
  )

  expect_s3_class(result, "pls_result")
})

# --- .validate_spec error paths ---

test_that(".validate_spec errors when no data matrices", {
  spec <- pls_spec()
  spec$num_cond <- 3L

  expect_error(
    plsrri:::.validate_spec(spec),
    "No data matrices"
  )
})

test_that(".validate_spec errors when num_cond not set", {
  set.seed(42)
  spec <- pls_spec()
  spec$datamat_lst  <- list(matrix(rnorm(30), 6, 5))
  spec$num_subj_lst <- 3L

  expect_error(
    plsrri:::.validate_spec(spec),
    "Number of conditions not set"
  )
})

test_that(".validate_spec errors when behavior method lacks behavdata", {
  set.seed(42)
  spec <- pls_spec() |>
    add_subjects(list(matrix(rnorm(60), 12, 5)), groups = 6) |>
    add_conditions(2) |>
    configure(method = "behavior")

  expect_error(
    plsrri:::.validate_spec(spec),
    "behavior data"
  )
})

test_that(".validate_spec errors when bootstrap requested with too few subjects", {
  set.seed(42)
  spec <- pls_spec() |>
    add_subjects(list(matrix(rnorm(20), 4, 5)), groups = 2) |>
    add_conditions(2) |>
    configure(method = "task", nboot = 10)

  expect_error(
    plsrri:::.validate_spec(spec),
    "at least 3 subjects"
  )
})

test_that(".validate_spec passes for a valid task spec", {
  set.seed(42)
  spec <- pls_spec() |>
    add_subjects(list(matrix(rnorm(60), 12, 5)), groups = 6) |>
    add_conditions(2) |>
    configure(method = "task", nperm = 0, nboot = 0)

  expect_true(plsrri:::.validate_spec(spec))
})
