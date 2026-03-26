# Tests for R/utils.R utility functions

# --- stack_datamats ---

test_that("stack_datamats stacks matrices by rows", {
  set.seed(42)
  m1 <- matrix(1:6, nrow = 2)
  m2 <- matrix(7:12, nrow = 2)
  result <- plsrri:::stack_datamats(list(m1, m2))
  expect_equal(nrow(result), 4L)
  expect_equal(ncol(result), 3L)
  expect_equal(result[1:2, ], m1)
  expect_equal(result[3:4, ], m2)
})

test_that("stack_datamats with single matrix returns that matrix", {
  set.seed(42)
  m <- matrix(rnorm(20), nrow = 4)
  result <- plsrri:::stack_datamats(list(m))
  expect_equal(result, m)
})

test_that("stack_datamats preserves column count", {
  set.seed(42)
  mats <- lapply(1:3, function(i) matrix(rnorm(50), nrow = 5))
  result <- plsrri:::stack_datamats(mats)
  expect_equal(nrow(result), 15L)
  expect_equal(ncol(result), 10L)
})

# --- check_datamat ---

test_that("check_datamat returns TRUE invisibly for valid matrix", {
  m <- matrix(1:9, nrow = 3)
  result <- plsrri:::check_datamat(m)
  expect_true(result)
})

test_that("check_datamat errors on non-matrix input", {
  expect_error(plsrri:::check_datamat(data.frame(a = 1:3)), "must be a matrix")
  expect_error(plsrri:::check_datamat(1:10), "must be a matrix")
  expect_error(plsrri:::check_datamat(list(1, 2)), "must be a matrix")
})

test_that("check_datamat uses custom name in error message", {
  expect_error(plsrri:::check_datamat(1:5, name = "mydata"), "mydata must be a matrix")
})

test_that("check_datamat warns on non-finite values", {
  m <- matrix(c(1, 2, NA, 4), nrow = 2)
  expect_warning(plsrri:::check_datamat(m), "non-finite values")
})

test_that("check_datamat warns with Inf values", {
  m <- matrix(c(1, Inf, 3, -Inf), nrow = 2)
  expect_warning(plsrri:::check_datamat(m), "non-finite values")
})

# --- safe_div ---

test_that("safe_div performs normal division", {
  expect_equal(plsrri:::safe_div(10, 2), 5)
  expect_equal(plsrri:::safe_div(c(4, 6), c(2, 3)), c(2, 2))
})

test_that("safe_div returns 0 for 0/0", {
  expect_equal(plsrri:::safe_div(0, 0), 0)
})

test_that("safe_div returns 0 for non-finite results", {
  # 1/0 is Inf, which gets replaced with 0
  expect_equal(plsrri:::safe_div(1, 0), 0)
  expect_equal(plsrri:::safe_div(-1, 0), 0)
})

test_that("safe_div handles vector inputs with mixed zero denominators", {
  result <- plsrri:::safe_div(c(4, 0, 6), c(2, 0, 3))
  expect_equal(result, c(2, 0, 2))
})

# --- variance_explained ---

test_that("variance_explained returns percentages summing to 100", {
  s <- c(3, 2, 1)
  ve <- variance_explained(s)
  expect_equal(sum(ve), 100, tolerance = 1e-10)
  expect_true(all(ve > 0))
})

test_that("variance_explained with cumulative = TRUE returns cumulative sum", {
  s <- c(3, 2, 1)
  ve_cum <- variance_explained(s, cumulative = TRUE)
  ve_raw <- variance_explained(s)
  expect_equal(ve_cum, cumsum(ve_raw))
  expect_equal(ve_cum[length(ve_cum)], 100, tolerance = 1e-10)
})

test_that("variance_explained with single value returns 100", {
  expect_equal(variance_explained(5), 100)
})

test_that("variance_explained proportional to squared singular values", {
  s <- c(4, 3, 2, 1)
  ve <- variance_explained(s)
  expected <- (s^2) / sum(s^2) * 100
  expect_equal(ve, expected)
})

# --- make_row_indices ---

test_that("make_row_indices returns all indices when group and condition are NULL", {
  # 2 groups of 5 subjects, 3 conditions -> 30 rows total
  idx <- plsrri:::make_row_indices(c(5L, 5L), num_cond = 3)
  expect_equal(idx, 1:30)
})

test_that("make_row_indices selects correct rows for a single group", {
  # 2 groups of 5 subjects, 3 conditions
  # Group 1: rows 1-15, Group 2: rows 16-30
  idx <- plsrri:::make_row_indices(c(5L, 5L), num_cond = 3, group = 1)
  expect_equal(idx, 1:15)

  idx2 <- plsrri:::make_row_indices(c(5L, 5L), num_cond = 3, group = 2)
  expect_equal(idx2, 16:30)
})

test_that("make_row_indices selects correct rows for a single condition", {
  # 1 group of 4 subjects, 3 conditions: rows 1-4, 5-8, 9-12
  idx <- plsrri:::make_row_indices(c(4L), num_cond = 3, condition = 2)
  expect_equal(idx, 5:8)
})

test_that("make_row_indices selects group and condition jointly", {
  # 2 groups of 3 subjects, 2 conditions
  # Group1: rows 1-3 (cond1), 4-6 (cond2); Group2: rows 7-9 (cond1), 10-12 (cond2)
  idx <- plsrri:::make_row_indices(c(3L, 3L), num_cond = 2, group = 2, condition = 1)
  expect_equal(idx, 7:9)
})

test_that("make_row_indices handles ssb list input with NULL group/condition", {
  # ssb design: group 1 has 3 subjects per condition, group 2 has 4 per condition, 2 conditions
  ns_lst <- list(c(3L, 3L), c(4L, 4L))
  idx <- plsrri:::make_row_indices(ns_lst, num_cond = 2)
  expect_equal(idx, 1:14)
})

test_that("make_row_indices handles ssb list input selecting a condition", {
  ns_lst <- list(c(3L, 4L))  # 1 group, 2 conditions, unequal subjects
  idx <- plsrri:::make_row_indices(ns_lst, num_cond = 2, condition = 1)
  expect_equal(idx, 1:3)
  idx2 <- plsrri:::make_row_indices(ns_lst, num_cond = 2, condition = 2)
  expect_equal(idx2, 4:7)
})

# --- format_pvalue ---

test_that("format_pvalue formats small p-values with < notation", {
  result <- plsrri:::format_pvalue(0.00001, digits = 4)
  expect_match(result, "^<")
})

test_that("format_pvalue formats regular p-values as decimals", {
  result <- plsrri:::format_pvalue(0.05, digits = 4)
  expect_equal(result, "0.0500")
})

test_that("format_pvalue handles p = 1", {
  result <- plsrri:::format_pvalue(1, digits = 4)
  expect_equal(result, "1.0000")
})

test_that("format_pvalue respects digits argument", {
  result <- plsrri:::format_pvalue(0.123456, digits = 2)
  expect_equal(result, "0.12")
})

test_that("format_pvalue handles vector input", {
  ps <- c(0.5, 0.00001, 0.03)
  results <- plsrri:::format_pvalue(ps, digits = 4)
  expect_length(results, 3L)
  expect_match(results[2], "^<")
})

# --- require_package ---

test_that("require_package returns TRUE invisibly for available package", {
  result <- plsrri:::require_package("stats")
  expect_true(result)
})

test_that("require_package errors for unavailable package", {
  expect_error(
    plsrri:::require_package("this_package_does_not_exist_xyz"),
    "required"
  )
})

test_that("require_package includes purpose in error message", {
  expect_error(
    plsrri:::require_package("this_package_does_not_exist_xyz", purpose = "testing things"),
    "testing things"
  )
})

test_that("require_package includes install suggestion in error", {
  expect_error(
    plsrri:::require_package("no_such_pkg_abc"),
    "install.packages"
  )
})
