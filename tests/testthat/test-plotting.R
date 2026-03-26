# Smoke tests for plotting functions
#
# These tests verify that plotting functions run without error.
# They don't check visual correctness, just that plots are produced.

# Helper to create a test result object
make_test_result <- function(with_perm = FALSE, with_boot = FALSE) {
  set.seed(42)

  datamat1 <- matrix(rnorm(30 * 50), 30, 50)

  pls_analysis(
    datamat_lst = list(datamat1),
    num_subj_lst = 10,
    num_cond = 3,
    method = 1,
    num_perm = if (with_perm) 20 else 0,
    num_boot = if (with_boot) 20 else 0,
    progress = FALSE
  )
}

# -----------------------------------------------------------------------------
# plot_singular_values Tests
# -----------------------------------------------------------------------------

test_that("plot_singular_values produces ggplot", {
  result <- make_test_result(with_perm = TRUE)

  p <- plot_singular_values(result)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plot_singular_values works without permutation", {
  result <- make_test_result(with_perm = FALSE)

  p <- plot_singular_values(result)

  expect_s3_class(p, "ggplot")
})

test_that("plot_singular_values respects alpha parameter", {
  result <- make_test_result(with_perm = TRUE)

  p1 <- plot_singular_values(result, alpha = 0.05)
  p2 <- plot_singular_values(result, alpha = 0.01)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

# -----------------------------------------------------------------------------
# plot_scores Tests
# -----------------------------------------------------------------------------

test_that("plot_scores produces ggplot for design scores", {
  result <- make_test_result()

  p <- plot_scores(result, lv = 1, type = "design")

  expect_s3_class(p, "ggplot")
})

test_that("plot_scores produces ggplot for brain scores", {
  result <- make_test_result()

  p <- plot_scores(result, lv = 1, type = "brain")

  expect_s3_class(p, "ggplot")
})

test_that("plot_scores handles different plot_types", {
  result <- make_test_result()

  p_bar <- plot_scores(result, lv = 1, type = "design", plot_type = "bar")
  p_violin <- plot_scores(result, lv = 1, type = "brain", plot_type = "violin")

  expect_s3_class(p_bar, "ggplot")
  expect_s3_class(p_violin, "ggplot")
})

test_that("plot_scores works for multiple LVs", {
  result <- make_test_result()

  # Should work for LV 1, 2, 3
  for (lv in 1:min(3, n_lv(result))) {
    p <- plot_scores(result, lv = lv)
    expect_s3_class(p, "ggplot")
  }
})

# -----------------------------------------------------------------------------
# plot_loadings Tests
# -----------------------------------------------------------------------------

test_that("plot_loadings produces ggplot", {
  result <- make_test_result()

  p <- plot_loadings(result, lv = 1)

  expect_s3_class(p, "ggplot")
})

test_that("plot_loadings works for design loadings", {
  result <- make_test_result()

  p <- plot_loadings(result, lv = 1, type = "design")

  expect_s3_class(p, "ggplot")
})

test_that("plot_loadings with bootstrap shows error bars", {
  result <- make_test_result(with_boot = TRUE)

  p <- plot_loadings(result, lv = 1)

  expect_s3_class(p, "ggplot")
})

test_that("plot_loadings works for multiblock behavior loadings with subset bscan", {
  set.seed(321)

  n_subj <- 12L
  n_cond <- 3L
  n_features <- 80L
  total_rows <- n_subj * n_cond

  datamat <- matrix(rnorm(total_rows * n_features), nrow = total_rows, ncol = n_features)
  behav <- cbind(
    rt = rnorm(total_rows),
    acc = rnorm(total_rows)
  )

  result <- pls_spec() |>
    add_subjects(list(datamat), groups = n_subj) |>
    add_conditions(n_cond, labels = c("encoding", "retrieval", "rest")) |>
    add_behavior(behav, block_conditions = c("encoding", "retrieval")) |>
    configure(method = "multiblock") |>
    run(progress = FALSE)

  p <- plot_loadings(result, lv = 1, type = "behavior", plot_type = "dot")

  expect_s3_class(p, "ggplot")
})

test_that("plot_scores works for multiblock stacked scores with subset bscan", {
  set.seed(321)

  n_subj <- 12L
  n_cond <- 3L
  n_features <- 80L
  total_rows <- n_subj * n_cond

  datamat <- matrix(rnorm(total_rows * n_features), nrow = total_rows, ncol = n_features)
  behav <- cbind(
    rt = rnorm(total_rows),
    acc = rnorm(total_rows)
  )

  result <- pls_spec() |>
    add_subjects(list(datamat), groups = n_subj) |>
    add_conditions(n_cond, labels = c("encoding", "retrieval", "rest")) |>
    add_behavior(behav, block_conditions = c("encoding", "retrieval")) |>
    configure(method = "multiblock") |>
    run(progress = FALSE)

  p <- plot_scores(
    result,
    lv = 1,
    type = "brain",
    plot_type = "violin",
    facet_by = "block"
  )

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

# -----------------------------------------------------------------------------
# Generic plot method Tests
# -----------------------------------------------------------------------------

test_that("plot.pls_result works with default arguments", {
  result <- make_test_result()

  # Should produce some output without error
  expect_silent(p <- plot(result))
  expect_s3_class(p, "ggplot")
})

test_that("plot.pls_result works with what parameter", {
  result <- make_test_result(with_perm = TRUE, with_boot = TRUE)

  p_sv <- plot(result, what = "singular_values")
  p_scores <- plot(result, what = "scores", lv = 1)

  expect_s3_class(p_sv, "ggplot")
  expect_s3_class(p_scores, "ggplot")
})

# -----------------------------------------------------------------------------
# Theme Tests
# -----------------------------------------------------------------------------

test_that("theme_pls returns valid ggplot theme", {
  thm <- theme_pls()

  expect_s3_class(thm, "theme")
})

test_that("theme_pls can be applied to plots", {
  result <- make_test_result()

  p <- plot_singular_values(result) + theme_pls()

  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------
# Color Scale Tests
# -----------------------------------------------------------------------------

test_that("scale_color_pls works", {
  result <- make_test_result()

  p <- plot_scores(result, lv = 1, type = "design") +
    scale_color_pls()

  expect_s3_class(p, "ggplot")
})

test_that("scale_fill_pls works", {
  result <- make_test_result()

  p <- plot_scores(result, lv = 1, type = "design", plot_type = "bar") +
    scale_fill_pls()

  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------
# Error Handling Tests
# -----------------------------------------------------------------------------

test_that("plot functions handle invalid LV gracefully", {
  result <- make_test_result()

  # LV 0 or negative should error
  expect_error(plot_scores(result, lv = 0))
  expect_error(plot_scores(result, lv = -1))

  # LV beyond range should error
  expect_error(plot_scores(result, lv = 100))
})

# -----------------------------------------------------------------------------
# Summary Plot Tests
# -----------------------------------------------------------------------------

test_that("plot_pls_summary produces combined plot", {
  result <- make_test_result(with_perm = TRUE, with_boot = TRUE)

  # This might use patchwork or similar to combine plots
  # If it exists, test it
  if (exists("plot_pls_summary", mode = "function")) {
    p <- plot_pls_summary(result)
    # Should produce some kind of plot object
    expect_true(!is.null(p))
  }
})
