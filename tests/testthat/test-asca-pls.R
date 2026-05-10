test_that("asca_pls returns term results with estimand, test, display, and bridge objects", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.05)

  fit <- asca_pls(
    fx$spec,
    fit = fx$fit,
    decompose = fx$formula,
    condition_key = fx$condition_key,
    id = "subject",
    within = c("task", "level"),
    between = "group",
    test = partial_test(method = "none", statistic = "trace"),
    selection = hierarchical_selection(method = "none")
  )

  expect_s3_class(fit, "asca_pls_result")
  expect_equal(names(fit$terms), attr(stats::terms(fx$formula), "term.labels"))
  expect_true(is.data.frame(asca_term_table(fit)))
  term <- fit$terms$`group:task:level`
  expect_s3_class(term, "asca_term_result")
  expect_named(term, c("name", "estimand", "test", "display", "bridge"))
  expect_named(term$display, c("effect", "partial", "tested_effect"))
  expect_true(is.data.frame(term$estimand$cell_grid))
  expect_true(is.list(term$bridge))
  expect_equal(term$test$space, "partial")
})

test_that("diagnose_asca_formula reports effect and partial ranks for synthetic factorial data", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.05)

  diag <- diagnose_asca_formula(
    fx$fit,
    decompose = fx$formula,
    condition_key = fx$condition_key,
    id = "subject",
    within = c("task", "level"),
    between = "group"
  )

  expect_equal(diag$term, attr(stats::terms(fx$formula), "term.labels"))
  expect_true(all(diag$rank_effect >= 0L))
  expect_true(all(diag$rank_partial >= 0L))
  expect_true(diag$estimable[match("group:task:level", diag$term)])
  expect_false(diag$aliased[match("group:task:level", diag$term)])
})

test_that("synthetic ASCA-PLS fixture plants expected term structure", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.02)

  signal_score <- function(features, contrast) {
    y <- rowMeans(fx$datamat[, features, drop = FALSE])
    abs(stats::cor(y, contrast))
  }

  expect_gt(signal_score(fx$supports$`group:task`, fx$metadata$group_code * fx$metadata$task_code), 0.95)
  expect_gt(signal_score(fx$supports$level, fx$metadata$level_code), 0.95)
  expect_gt(
    signal_score(
      fx$supports$`group:task:level`,
      fx$metadata$group_code * fx$metadata$task_code * fx$metadata$level_code
    ),
    0.95
  )

  term_fit <- design_subspace_svd(fx$fit, design = fx$design, statistic = "trace")
  expect_true(all(is.finite(term_fit$statistics$trace)))
  expect_true(all(term_fit$statistics$trace >= 0))
})

test_that("Freedman-Lane partial tests return bounded reproducible p-values", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.10)
  set.seed(77)
  first <- asca_pls(
    fx$spec,
    fit = fx$fit,
    decompose = fx$formula,
    condition_key = fx$condition_key,
    test = partial_test(method = "freedman_lane", statistic = "trace", nperm = 9, correction = "maxT"),
    selection = hierarchical_selection(method = "backward", alpha = 0.10)
  )
  set.seed(77)
  second <- asca_pls(
    fx$spec,
    fit = fx$fit,
    decompose = fx$formula,
    condition_key = fx$condition_key,
    test = partial_test(method = "freedman_lane", statistic = "trace", nperm = 9, correction = "maxT"),
    selection = hierarchical_selection(method = "backward", alpha = 0.10)
  )

  tab <- anova(first)
  expect_true(all(is.finite(tab$p_value)))
  expect_true(all(tab$p_value >= 0 & tab$p_value <= 1))
  expect_true(all(tab$p_adjusted >= tab$p_value - 1e-12))
  expect_equal(tab$p_value, anova(second)$p_value)
  expect_equal(tab$p_adjusted, anova(second)$p_adjusted)
  expect_true(all(tab$status %in% c("kept", "dropped", "protected", "not_estimable")))
})

test_that("tested-effect bridge lifts partial components into factorial cell coordinates", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.05)
  fit <- asca_pls(
    fx$fit,
    decompose = fx$formula,
    condition_key = fx$condition_key,
    test = partial_test(method = "none", statistic = "largest_root"),
    selection = hierarchical_selection(method = "none")
  )

  comp <- asca_components(fit, term = "group:task:level", view = "tested_effect", component = 1L)
  brain <- brain_saliences(fit, term = "group:task:level", view = "tested_effect", component = 1L)
  design <- design_saliences(fit, term = "group:task:level", view = "tested_effect", component = 1L)
  expect_equal(nrow(comp$design_profile), nrow(design_cell_table(fx$fit, design = fx$design)))
  expect_equal(ncol(comp$brain_salience), 1L)
  expect_equal(brain, comp$brain_salience)
  expect_equal(design, comp$design_profile)
  expect_true(all(is.finite(comp$design_profile[, 1L])))
  expect_true(all(comp$bridge$alignment <= 1 + 1e-8, na.rm = TRUE))
})

test_that("ASCA-PLS plotting helpers return ggplot objects", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.05)
  fit <- asca_pls(
    fx$fit,
    decompose = fx$formula,
    condition_key = fx$condition_key,
    test = partial_test(method = "none", statistic = "largest_root"),
    selection = hierarchical_selection(method = "none")
  )

  expect_s3_class(
    plot_asca_effect_profile(fit, term = "group:task:level", x_axis = "level", line = "task", facet = "group"),
    "ggplot"
  )
  expect_s3_class(plot_asca_alignment(fit), "ggplot")
  expect_s3_class(plot_asca_selection(fit), "ggplot")
  expect_s3_class(plot(fit, term = "group:task:level", x_axis = "level"), "ggplot")
})

test_that("ASCA-PLS diagnostics flag non-estimable synthetic terms", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.05)
  collapsed_key <- fx$condition_key
  collapsed_key$level <- collapsed_key$task
  design <- pls_design(~ group * task * level, condition_key = collapsed_key)

  diag <- diagnose_asca_formula(
    fx$fit,
    decompose = design$formula,
    condition_key = collapsed_key
  )

  expect_true(any(diag$aliased))
  expect_true(any(!diag$estimable))
})
