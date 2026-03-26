# Tests for builder_conditions.R untested paths

# ---- add_conditions() with data.frame (events path) ----

test_that("add_conditions with trial_type data frame sets conditions from unique values", {
  spec <- pls_spec()
  events <- data.frame(
    trial_type = c("A", "B", "A", "C", "B"),
    onset      = c(0, 2, 4, 6, 8),
    duration   = rep(1, 5),
    stringsAsFactors = FALSE
  )
  spec2 <- add_conditions(spec, events)

  expect_equal(spec2$num_cond, 3L)
  expect_equal(sort(spec2$conditions), c("A", "B", "C"))
  expect_equal(spec2$events, events)
})

test_that("add_conditions with 'condition' column in data frame works", {
  spec <- pls_spec()
  events <- data.frame(
    condition = c("rest", "task", "rest"),
    onset     = c(0, 10, 20),
    duration  = rep(5, 3),
    stringsAsFactors = FALSE
  )
  spec2 <- add_conditions(spec, events)

  expect_equal(spec2$num_cond, 2L)
  expect_equal(sort(spec2$conditions), c("rest", "task"))
})

test_that("add_conditions errors when data frame has no trial_type or condition column", {
  spec <- pls_spec()
  bad_events <- data.frame(onset = c(0, 1), duration = c(1, 1))
  expect_error(add_conditions(spec, bad_events), "trial_type.*condition")
})

# ---- .add_conditions_events() directly ----

test_that(".add_conditions_events stores events on spec", {
  spec <- pls_spec()
  events <- data.frame(
    trial_type = c("X", "Y"),
    onset      = c(0, 5),
    stringsAsFactors = FALSE
  )
  spec2 <- plsrri:::.add_conditions_events(spec, events)

  expect_equal(spec2$num_cond, 2L)
  expect_true(!is.null(spec2$events))
})

# ---- .validate_dimensions() ----

test_that(".validate_dimensions is silent when num_cond is NULL", {
  spec <- pls_spec()
  spec$num_cond <- NULL
  expect_silent(plsrri:::.validate_dimensions(spec))
})

test_that(".validate_dimensions errors on row count mismatch (integer num_subj_lst)", {
  set.seed(42)
  # 13 rows, num_subj=5, num_cond=3: 13 is not divisible by 3 -> hard error
  spec <- pls_spec()
  spec$datamat_lst <- list(matrix(rnorm(13 * 10), 13, 10))
  spec$num_subj_lst <- 5L
  spec$num_cond <- 3L
  expect_error(
    plsrri:::.validate_dimensions(spec),
    "data matrix has"
  )
})

test_that(".validate_dimensions is silent when dimensions are consistent", {
  set.seed(42)
  spec <- pls_spec()
  spec$datamat_lst <- list(matrix(rnorm(15 * 10), 15, 10))
  spec$num_subj_lst <- 5L   # 5 * 3 = 15 rows
  spec$num_cond <- 3L
  expect_silent(plsrri:::.validate_dimensions(spec))
})

test_that(".validate_dimensions messages when rows divisible by conditions (auto-correct)", {
  set.seed(42)
  # 12 rows, num_subj=5 (wrong), but 12 %% 3 == 0 so auto-correct fires a cli message
  spec <- pls_spec()
  spec$datamat_lst <- list(matrix(rnorm(12 * 10), 12, 10))
  spec$num_subj_lst <- 5L
  spec$num_cond <- 3L
  expect_message(
    plsrri:::.validate_dimensions(spec),
    "adjusting num_subj"
  )
})

test_that(".validate_dimensions errors when rows not divisible by conditions", {
  set.seed(42)
  # 13 rows, 3 conditions: 13 %% 3 != 0
  spec <- pls_spec()
  spec$datamat_lst <- list(matrix(rnorm(13 * 10), 13, 10))
  spec$num_subj_lst <- 5L
  spec$num_cond <- 3L
  expect_error(
    plsrri:::.validate_dimensions(spec),
    "data matrix has"
  )
})
