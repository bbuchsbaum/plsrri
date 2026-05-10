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

test_that("synthetic ASCA-PLS fixture and raw metadata use Task PLS row order", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.02)
  expected_condition <- unlist(lapply(names(fx$n_per_group), function(group) {
    rep(fx$condition_key$condition, each = fx$n_per_group[[group]])
  }), use.names = FALSE)

  expect_equal(fx$metadata$condition, expected_condition)

  ctx <- plsrri:::.design_subspace_context(
    result = fx$fit,
    design = fx$design,
    formula = fx$formula,
    condition_key = fx$condition_key,
    weights = "cell_equal",
    require_crossblock = TRUE
  )
  obs <- plsrri:::.asca_observation_table(fx$spec, ctx)

  expect_equal(as.character(obs$group), fx$metadata$group)
  expect_equal(as.character(obs$subject), fx$metadata$subject)
  expect_equal(as.character(obs$condition), fx$metadata$condition)
  expect_equal(as.character(obs$task), fx$metadata$task)
  expect_equal(as.character(obs$level), fx$metadata$level)
})

test_that("ASCA-PLS raw permutation orders respect within and between exchangeability", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.02)
  ctx <- plsrri:::.design_subspace_context(
    result = fx$fit,
    design = fx$design,
    formula = fx$formula,
    condition_key = fx$condition_key,
    weights = "cell_equal",
    require_crossblock = TRUE
  )
  obs <- plsrri:::.asca_observation_table(fx$spec, ctx)
  exch <- exchangeability(unit = "subject", within = c("task", "level"), between = "group")

  set.seed(101)
  between_perm <- plsrri:::.asca_raw_permutation_orders(obs, term = "group", nperm = 8L, exchangeability = exch)
  for (p in seq_len(ncol(between_perm))) {
    for (subject in unique(as.character(obs$subject))) {
      target <- which(as.character(obs$subject) == subject)
      source_subject <- unique(as.character(obs$subject[between_perm[target, p]]))
      expect_length(source_subject, 1L)
      expect_equal(as.character(obs$condition[between_perm[target, p]]), as.character(obs$condition[target]))
    }
  }

  set.seed(102)
  within_perm <- plsrri:::.asca_raw_permutation_orders(obs, term = "task", nperm = 8L, exchangeability = exch)
  for (p in seq_len(ncol(within_perm))) {
    for (subject in unique(as.character(obs$subject))) {
      target <- which(as.character(obs$subject) == subject)
      expect_equal(unique(as.character(obs$subject[within_perm[target, p]])), subject)
      expect_setequal(as.character(obs$condition[within_perm[target, p]]), as.character(obs$condition[target]))
    }
  }
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

test_that("ASCA-PLS reduced-model permutations require raw spec for calibrated inference", {
  fx <- make_asca_pls_synthetic_fixture(noise_sd = 0.10)

  expect_error(
    asca_pls(
      fx$fit,
      decompose = fx$formula,
      condition_key = fx$condition_key,
      test = partial_test(method = "freedman_lane", statistic = "trace", nperm = 9),
      selection = hierarchical_selection(method = "none")
    ),
    "requires the original pls_spec"
  )
})

test_that("ASCA-PLS interaction tests are not anti-conservative under main-effects-only synthetic data", {
  interaction_terms <- c("group:task", "group:level", "task:level", "group:task:level")
  main_only <- c(
    group = 1.4,
    task = 1.2,
    level = 1.1,
    `group:task` = 0,
    `group:level` = 0,
    `task:level` = 0,
    `group:task:level` = 0
  )
  alpha <- 0.10
  n_reps <- 12L
  nperm <- 99L
  p_values <- vector("list", n_reps)

  for (i in seq_len(n_reps)) {
    fx <- make_asca_pls_synthetic_fixture(
      seed = 9000L + i,
      n_per_group = c(control = 8L, sdam = 8L),
      effects = main_only,
      noise_sd = 0.8
    )
    set.seed(5000L + i)
    fit <- asca_pls(
      fx$spec,
      fit = fx$fit,
      decompose = fx$formula,
      condition_key = fx$condition_key,
      id = "subject",
      within = c("task", "level"),
      between = "group",
      test = partial_test(method = "freedman_lane", statistic = "trace", nperm = nperm, correction = "none"),
      selection = hierarchical_selection(method = "none")
    )
    tab <- anova(fit)
    p_values[[i]] <- tab$p_value[match(interaction_terms, tab$term)]
  }

  p_values <- unlist(p_values, use.names = FALSE)
  false_positives <- sum(p_values <= alpha)
  total_tests <- length(p_values)
  upper_99 <- stats::qbinom(0.99, size = total_tests, prob = alpha)

  expect_true(all(is.finite(p_values)))
  expect_true(all(p_values >= 0 & p_values <= 1))
  expect_true(
    false_positives <= upper_99,
    label = sprintf(
      "Observed %d/%d interaction false positives at alpha %.2f; 99%% binomial upper bound is %d.",
      false_positives, total_tests, alpha, upper_99
    )
  )
})

test_that("ASCA-PLS between-subject tests are not anti-conservative under group-null synthetic data", {
  group_null <- c(
    group = 0,
    task = 1.2,
    level = 1.1,
    `group:task` = 0,
    `group:level` = 0,
    `task:level` = 0,
    `group:task:level` = 0
  )
  alpha <- 0.10
  n_reps <- 12L
  nperm <- 99L
  p_values <- numeric(n_reps)

  for (i in seq_len(n_reps)) {
    fx <- make_asca_pls_synthetic_fixture(
      seed = 12000L + i,
      n_per_group = c(control = 8L, sdam = 8L),
      effects = group_null,
      noise_sd = 0.8
    )
    set.seed(13000L + i)
    fit <- asca_pls(
      fx$spec,
      fit = fx$fit,
      decompose = fx$formula,
      condition_key = fx$condition_key,
      id = "subject",
      within = c("task", "level"),
      between = "group",
      test = partial_test(method = "freedman_lane", statistic = "trace", nperm = nperm, correction = "none"),
      selection = hierarchical_selection(method = "none")
    )
    tab <- anova(fit)
    p_values[[i]] <- tab$p_value[match("group", tab$term)]
  }

  false_positives <- sum(p_values <= alpha)
  upper_99 <- stats::qbinom(0.99, size = length(p_values), prob = alpha)

  expect_true(all(is.finite(p_values)))
  expect_true(all(p_values >= 0 & p_values <= 1))
  expect_true(
    false_positives <= upper_99,
    label = sprintf(
      "Observed %d/%d group false positives at alpha %.2f; 99%% binomial upper bound is %d.",
      false_positives, length(p_values), alpha, upper_99
    )
  )
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
