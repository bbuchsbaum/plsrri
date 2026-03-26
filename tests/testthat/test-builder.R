# Tests for builder API

test_that("pls_spec creates valid object", {
  spec <- pls_spec()

  expect_s3_class(spec, "pls_spec")
  expect_null(spec$mask)
  expect_equal(length(spec$datamat_lst), 0)
  expect_equal(spec$method, 1L)
})

test_that("add_subjects with matrix list works", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  data2 <- matrix(rnorm(24 * 50), 24, 50)

  spec <- pls_spec() |>
    add_subjects(list(data1, data2), groups = c(10, 8))

  expect_equal(length(spec$datamat_lst), 2)
  expect_equal(spec$num_subj_lst, c(10, 8))
})

test_that("add_conditions works", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3, labels = c("A", "B", "C"))

  expect_equal(spec$num_cond, 3)
  expect_equal(spec$conditions, c("A", "B", "C"))
})

test_that("add_behavior works", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  behav <- matrix(rnorm(30 * 2), 30, 2)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    add_behavior(behav)

  expect_equal(dim(spec$stacked_behavdata), c(30, 2))
})

test_that("configure sets parameters correctly", {
  spec <- pls_spec() |>
    configure(method = "behavior", nperm = 500, nboot = 200, cormode = "covariance")

  expect_equal(spec$method, 3L)
  expect_equal(spec$num_perm, 500L)
  expect_equal(spec$num_boot, 200L)
  expect_equal(spec$cormode, 2L)
})

test_that("configure resolves method names", {
  expect_equal(pls_spec() |> configure(method = "task") |> (\(x) x$method)(), 1L)
  expect_equal(pls_spec() |> configure(method = "behavior") |> (\(x) x$method)(), 3L)
  expect_equal(pls_spec() |> configure(method = "multiblock") |> (\(x) x$method)(), 4L)
  expect_equal(pls_spec() |> configure(method = 2) |> (\(x) x$method)(), 2L)
})

test_that("run validates spec", {
  spec <- pls_spec()

  expect_error(run(spec), "No data matrices")
})

test_that("full builder pipeline works", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    configure(method = "task", nperm = 5) |>
    run(progress = FALSE)

  expect_s3_class(result, "pls_result")
  expect_equal(result$method, 1L)
  expect_s3_class(result$perm_result, "pls_perm_result")
})

test_that("quick_pls convenience function works", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  result <- quick_pls(
    datamat_lst = list(data1),
    num_subj_lst = 10,
    num_cond = 3,
    nperm = 5,
    nboot = 5,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
})

# --- add_behavior() additional tests ---

test_that("add_behavior stores stacked_behavdata with correct dimensions", {
  set.seed(42)
  data1 <- matrix(rnorm(60 * 50), 60, 50)  # 20 subj x 3 cond
  behav <- matrix(rnorm(60 * 3), 60, 3)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 20) |>
    add_conditions(3) |>
    add_behavior(behav)

  expect_equal(dim(spec$stacked_behavdata), c(60, 3))
})

test_that("add_behavior assigns default measure names when none given", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  behav <- matrix(rnorm(30 * 2), 30, 2)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    add_behavior(behav)

  expect_equal(colnames(spec$stacked_behavdata), c("measure_1", "measure_2"))
})

test_that("add_behavior preserves column names from matrix", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  behav <- matrix(rnorm(30 * 2), 30, 2)
  colnames(behav) <- c("accuracy", "rt")

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    add_behavior(behav)

  expect_equal(colnames(spec$stacked_behavdata), c("accuracy", "rt"))
})

test_that("add_behavior accepts data frame and converts to matrix", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  behav_df <- data.frame(accuracy = rnorm(30), rt = rnorm(30))

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    add_behavior(behav_df)

  expect_true(is.matrix(spec$stacked_behavdata))
  expect_equal(dim(spec$stacked_behavdata), c(30, 2))
  expect_equal(colnames(spec$stacked_behavdata), c("accuracy", "rt"))
})

test_that("add_behavior accepts explicit measures argument", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  behav <- matrix(rnorm(30 * 2), 30, 2)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    add_behavior(behav, measures = c("score_A", "score_B"))

  expect_equal(colnames(spec$stacked_behavdata), c("score_A", "score_B"))
})

test_that("add_behavior errors when row count mismatches subjects x conditions", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)  # 10 subj x 3 cond = 30 rows
  behav_wrong <- matrix(rnorm(20 * 2), 20, 2)  # wrong: 20 rows

  spec_partial <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3)

  expect_error(add_behavior(spec_partial, behav_wrong), regexp = "rows")
})

test_that("add_behavior supports ssb num_subj_lst row counts", {
  set.seed(42)
  data1 <- matrix(rnorm(9 * 20), 9, 20)
  data2 <- matrix(rnorm(5 * 20), 5, 20)
  behav <- matrix(rnorm(14 * 2), 14, 2)

  spec <- pls_spec() |>
    add_subjects(
      list(data1, data2),
      groups = list(c(2, 3, 4), c(1, 1, 3))
    ) |>
    add_conditions(3) |>
    add_behavior(behav)

  expect_equal(dim(spec$stacked_behavdata), c(14, 2))
})

# --- add_design() tests ---

test_that("add_design stores stacked_designdata", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  # 1 group x 3 conditions = 3 rows, 2 contrasts
  contrasts <- matrix(c(1, 0, -1, 0, 1, -1), ncol = 2)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    add_design(contrasts)

  expect_true(is.matrix(spec$stacked_designdata))
  expect_equal(dim(spec$stacked_designdata), c(3, 2))
})

test_that("add_design assigns default contrast labels", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  contrasts <- matrix(c(1, 0, -1, 0, 1, -1), ncol = 2)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    add_design(contrasts)

  expect_equal(colnames(spec$stacked_designdata), c("contrast_1", "contrast_2"))
})

test_that("add_design stores provided labels", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  contrasts <- matrix(c(1, 0, -1, 0, 1, -1), ncol = 2)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    add_design(contrasts, labels = c("task_vs_rest", "cond1_vs_cond2"))

  expect_equal(colnames(spec$stacked_designdata), c("task_vs_rest", "cond1_vs_cond2"))
})

test_that("add_design accepts data frame contrasts", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  contrasts_df <- data.frame(c1 = c(1, 0, -1), c2 = c(0, 1, -1))

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3) |>
    add_design(contrasts_df)

  expect_true(is.matrix(spec$stacked_designdata))
  expect_equal(dim(spec$stacked_designdata), c(3, 2))
})

# --- generate_contrasts() tests ---

test_that("generate_contrasts helmert has correct dimensions", {
  result <- generate_contrasts(num_groups = 1, num_cond = 3, type = "helmert")

  # 1 group x 3 conditions = 3 rows; helmert for 3 conditions = 2 columns
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("generate_contrasts replicates pattern across groups", {
  result <- generate_contrasts(num_groups = 2, num_cond = 3, type = "helmert")

  expect_equal(nrow(result), 6)
  # Both groups should have the same contrast pattern
  expect_equal(result[1:3, ], result[4:6, ])
})

test_that("generate_contrasts treatment type works", {
  result <- generate_contrasts(num_groups = 1, num_cond = 4, type = "treatment")

  # treatment for 4 conditions: 4 rows, 3 columns
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
})

test_that("generate_contrasts polynomial type works", {
  result <- generate_contrasts(num_groups = 1, num_cond = 4, type = "polynomial")

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
})

test_that("generate_contrasts deviation type works", {
  result <- generate_contrasts(num_groups = 1, num_cond = 3, type = "deviation")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("generate_contrasts errors on unknown type", {
  expect_error(generate_contrasts(1, 3, type = "unknown"))
})

test_that("generate_contrasts multi-group dimensions scale correctly", {
  result <- generate_contrasts(num_groups = 3, num_cond = 4, type = "helmert")

  expect_equal(nrow(result), 12)  # 3 * 4
  expect_equal(ncol(result), 3)   # helmert for 4 conditions = 3 columns
})

# --- add_conditions() additional tests ---

test_that("add_conditions with integer sets num_cond", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3)

  expect_equal(spec$num_cond, 3)
  expect_equal(spec$conditions, c("condition_1", "condition_2", "condition_3"))
})

test_that("add_conditions with character vector sets labels", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(c("rest", "task_A", "task_B"))

  expect_equal(spec$num_cond, 3)
  expect_equal(spec$conditions, c("rest", "task_A", "task_B"))
})

test_that("add_conditions with integer and labels uses provided labels", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3, labels = c("baseline", "task1", "task2"))

  expect_equal(spec$conditions, c("baseline", "task1", "task2"))
})

test_that("add_conditions with event_table data frame works", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  events <- data.frame(
    trial_type = c("A", "B", "A", "C"),
    onset = c(0, 10, 20, 30),
    duration = rep(5, 4)
  )

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(events)

  expect_equal(spec$num_cond, 3)
  expect_true(!is.null(spec$events))
})

test_that("add_conditions errors with mismatched labels length", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  spec_partial <- pls_spec() |>
    add_subjects(list(data1), groups = 10)

  expect_error(add_conditions(spec_partial, 3, labels = c("A", "B")))
})

test_that("add_conditions errors with invalid type", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  spec_partial <- pls_spec() |>
    add_subjects(list(data1), groups = 10)

  expect_error(add_conditions(spec_partial, list(1, 2, 3)))
})

# --- get_condition_rows() tests ---

test_that("get_condition_rows returns correct indices for balanced design", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)  # 10 subjects x 3 conditions

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3)

  # Condition 1: rows 1-10
  rows_c1 <- get_condition_rows(spec, group = 1, condition = 1)
  expect_equal(rows_c1, 1:10)

  # Condition 2: rows 11-20
  rows_c2 <- get_condition_rows(spec, group = 1, condition = 2)
  expect_equal(rows_c2, 11:20)

  # Condition 3: rows 21-30
  rows_c3 <- get_condition_rows(spec, group = 1, condition = 3)
  expect_equal(rows_c3, 21:30)
})

test_that("get_condition_rows returns correct indices for two-group design", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)  # group1: 10 subj x 3 cond
  data2 <- matrix(rnorm(24 * 50), 24, 50)  # group2: 8 subj x 3 cond

  spec <- pls_spec() |>
    add_subjects(list(data1, data2), groups = c(10, 8)) |>
    add_conditions(3)

  # Group 1, condition 1: rows 1-10
  rows_g1c1 <- get_condition_rows(spec, group = 1, condition = 1)
  expect_equal(rows_g1c1, 1:10)

  # Group 2, condition 1: rows 31-38 (group1 has 30 rows total)
  rows_g2c1 <- get_condition_rows(spec, group = 2, condition = 1)
  expect_equal(rows_g2c1, 31:38)
})

test_that("get_condition_rows handles condition by label", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  spec <- pls_spec() |>
    add_subjects(list(data1), groups = 10) |>
    add_conditions(3, labels = c("rest", "task_A", "task_B"))

  rows_by_label <- get_condition_rows(spec, group = 1, condition = "task_A")
  rows_by_index <- get_condition_rows(spec, group = 1, condition = 2)
  expect_equal(rows_by_label, rows_by_index)
})

# --- add_subjects() additional tests ---

test_that("add_subjects with single matrix (not list) works", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  spec <- pls_spec() |>
    add_subjects(data1, groups = 10)

  expect_equal(length(spec$datamat_lst), 1)
  expect_equal(spec$num_subj_lst, 10)
})

test_that("add_subjects stores datamat_lst correctly", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  data2 <- matrix(rnorm(24 * 50), 24, 50)

  spec <- pls_spec() |>
    add_subjects(list(data1, data2), groups = c(10, 8))

  expect_equal(length(spec$datamat_lst), 2)
  expect_equal(nrow(spec$datamat_lst[[1]]), 30)
  expect_equal(nrow(spec$datamat_lst[[2]]), 24)
})

test_that("add_subjects errors when matrices have different column counts", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  data2 <- matrix(rnorm(24 * 40), 24, 40)  # different ncol

  expect_error(
    pls_spec() |> add_subjects(list(data1, data2), groups = c(10, 8)),
    regexp = "same number of columns"
  )
})

test_that("add_subjects errors when groups length mismatches data list", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  data2 <- matrix(rnorm(24 * 50), 24, 50)

  expect_error(
    pls_spec() |> add_subjects(list(data1, data2), groups = c(10, 8, 6)),
    regexp = "same length"
  )
})

test_that("add_subjects infers num_subj_lst from nrow when groups is NULL", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  data2 <- matrix(rnorm(24 * 50), 24, 50)

  spec <- pls_spec() |>
    add_subjects(list(data1, data2))

  expect_equal(spec$num_subj_lst, c(30, 24))
})

# --- add_group_labels() tests ---

test_that("add_group_labels stores labels in spec", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  data2 <- matrix(rnorm(24 * 50), 24, 50)

  spec <- pls_spec() |>
    add_subjects(list(data1, data2), groups = c(10, 8)) |>
    add_group_labels(c("control", "patient"))

  expect_equal(spec$groups, c("control", "patient"))
})

test_that("add_group_labels errors when label count mismatches group count", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)
  data2 <- matrix(rnorm(24 * 50), 24, 50)

  spec_partial <- pls_spec() |>
    add_subjects(list(data1, data2), groups = c(10, 8))

  expect_error(
    add_group_labels(spec_partial, c("control", "patient", "extra")),
    regexp = "Number of labels"
  )
})

test_that("add_group_labels errors when labels is not character", {
  set.seed(42)
  data1 <- matrix(rnorm(30 * 50), 30, 50)

  spec_partial <- pls_spec() |>
    add_subjects(list(data1), groups = 10)

  expect_error(add_group_labels(spec_partial, 1))
})
