# Tests for R/plot_design.R

make_factorial_design_result <- function() {
  result <- make_mock_pls_result(n_voxels = 20, n_lv = 2, n_obs = 16)
  result$num_subj_lst <- c(2L, 2L)
  result$num_cond <- 4L
  result$groups <- c("control", "sdam")
  result$conditions <- c("recog_low_mid", "recog_high_sem", "nback_low_mid", "nback_high_sem")

  cell_means <- c(0.6, 0.0, -0.4, -0.2, -0.1, -0.5, 0.2, 0.4)
  result$vsc[, 1L] <- rep(cell_means, each = 2L)
  result$vsc[, 2L] <- rep(rev(cell_means), each = 2L)
  result
}

factorial_condition_key <- function() {
  data.frame(
    condition = c("recog_low_mid", "recog_high_sem", "nback_low_mid", "nback_high_sem"),
    task = c("recog", "recog", "nback", "nback"),
    level = c("low_mid", "high_sem", "low_mid", "high_sem"),
    stringsAsFactors = FALSE
  )
}

test_that("as_design_scores rehydrates condition factors and summarizes cells", {
  result <- make_factorial_design_result()
  scores <- as_design_scores(result, lv = 1, condition_key = factorial_condition_key())

  expect_equal(nrow(scores), 8L)
  expect_named(scores, c("group", "condition", "task", "level", "mean", "sd", "n", "se"))
  expect_equal(scores$n, rep(2L, 8L))
  expect_equal(as.character(scores$task[match("nback_high_sem", scores$condition)]), "nback")
  expect_equal(as.character(scores$level[match("recog_low_mid", scores$condition)]), "low_mid")
})

test_that("as_design_contrasts decomposes a full two-by-two-by-two design", {
  result <- make_factorial_design_result()
  contrasts <- as_design_contrasts(result, lv = 1, condition_key = factorial_condition_key())

  expect_equal(nrow(contrasts), 7L)
  expect_setequal(
    contrasts$effect,
    c("group", "task", "level", "group:task", "group:level", "task:level", "group:task:level")
  )
  expect_true(all(is.finite(contrasts$magnitude)))
  expect_true(all(contrasts$proportion >= 0 & contrasts$proportion <= 1))
})

test_that("as_design_score_space summarizes cells across two latent variables", {
  result <- make_factorial_design_result()
  space <- as_design_score_space(result, lv = c(1, 2), condition_key = factorial_condition_key())

  expect_equal(nrow(space), 8L)
  expect_true(all(c("x", "y", "x_se", "y_se", "lv_x", "lv_y") %in% names(space)))
  expect_equal(unique(space$lv_x), 1L)
  expect_equal(unique(space$lv_y), 2L)
  expect_true(all(is.finite(space$x)))
  expect_true(all(is.finite(space$y)))
})

test_that("design interpretation plots return ggplot objects", {
  skip_if_not_installed("ggplot2")

  result <- make_factorial_design_result()
  key <- factorial_condition_key()

  expect_s3_class(plot_design_heatmap(result, lv = 1, condition_key = key), "gg")
  expect_s3_class(plot_design_interaction(result, lv = 1, condition_key = key), "gg")
  expect_s3_class(plot_design_contrasts(result, lv = 1, condition_key = key), "gg")
  expect_s3_class(plot_design_score_space(result, lv = c(1, 2), condition_key = key), "gg")
})

test_that("condition keys must cover all PLS condition labels", {
  result <- make_factorial_design_result()
  bad_key <- factorial_condition_key()[-1L, ]

  expect_error(
    as_design_scores(result, lv = 1, condition_key = bad_key),
    "missing condition labels"
  )
})
