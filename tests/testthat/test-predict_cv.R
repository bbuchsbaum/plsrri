# Tests for nested predictive cross-validation

.make_prediction_fixture <- function() {
  n_subj <- 20L
  n_cond <- 2L
  n_features <- 10L

  z <- as.numeric(scale(seq(-1, 1, length.out = n_subj) + rnorm(n_subj, sd = 0.15)))
  w1 <- c(rep(1, 4), rep(0, n_features - 4))
  w2 <- c(rep(0, 2), rep(1, 4), rep(0, n_features - 6))

  datamat <- rbind(
    outer(z, w1) + matrix(rnorm(n_subj * n_features, sd = 0.25), n_subj, n_features),
    outer(z, w2) + matrix(rnorm(n_subj * n_features, sd = 0.25), n_subj, n_features)
  )

  behav <- cbind(
    target = rep(z, times = n_cond) + rnorm(n_subj * n_cond, sd = 0.05),
    nuisance = rnorm(n_subj * n_cond, sd = 0.5)
  )

  spec <- pls_spec()
  spec$datamat_lst <- list(datamat)
  spec$num_subj_lst <- n_subj
  spec$num_cond <- n_cond
  spec$method <- 3L
  spec$stacked_behavdata <- behav

  list(
    spec = spec,
    outcome_reg = z + rnorm(n_subj, sd = 0.05),
    outcome_cls = factor(ifelse(z > stats::median(z), "case", "control"),
                         levels = c("control", "case"))
  )
}

.make_multigroup_prediction_fixture <- function() {
  n1 <- 8L
  n2 <- 6L
  n_cond <- 2L
  n_features <- 8L

  z1 <- as.numeric(scale(seq(-1, 1, length.out = n1) + rnorm(n1, sd = 0.1)))
  z2 <- as.numeric(scale(seq(1, -1, length.out = n2) + rnorm(n2, sd = 0.1)))
  z <- c(z1, z2)
  w1 <- c(rep(1, 3), rep(0, n_features - 3))
  w2 <- c(rep(0, 2), rep(1, 3), rep(0, n_features - 5))

  datamat1 <- rbind(
    outer(z1, w1) + matrix(rnorm(n1 * n_features, sd = 0.25), n1, n_features),
    outer(z1, w2) + matrix(rnorm(n1 * n_features, sd = 0.25), n1, n_features)
  )
  datamat2 <- rbind(
    outer(z2, w1) + matrix(rnorm(n2 * n_features, sd = 0.25), n2, n_features),
    outer(z2, w2) + matrix(rnorm(n2 * n_features, sd = 0.25), n2, n_features)
  )

  behav <- cbind(
    target = rep(z, times = n_cond) + rnorm((n1 + n2) * n_cond, sd = 0.05),
    nuisance = rnorm((n1 + n2) * n_cond, sd = 0.5)
  )

  spec <- pls_spec()
  spec$datamat_lst <- list(datamat1, datamat2)
  spec$num_subj_lst <- c(n1, n2)
  spec$num_cond <- n_cond
  spec$method <- 3L
  spec$groups <- c("g1", "g2")
  spec$stacked_behavdata <- behav

  list(
    spec = spec,
    outcome_cls = factor(ifelse(z > stats::median(z), "case", "control"),
                         levels = c("control", "case"))
  )
}

test_that("subject-level split generation is leakage-safe for outer and inner CV", {
  set.seed(201)
  fixture <- .make_prediction_fixture()
  index <- .prediction_subject_index(fixture$spec)

  outer_splits <- .prediction_make_splits(
    index = index,
    subject_ids = index$subjects$subject_id,
    outcome = fixture$outcome_cls,
    n_folds = 5L,
    n_repeats = 2L,
    stratify = TRUE,
    seed = 77L
  )

  expect_equal(nrow(outer_splits), 10L)
  for (i in seq_len(nrow(outer_splits))) {
    expect_length(
      intersect(outer_splits$train_subject_ids[[i]], outer_splits$test_subject_ids[[i]]),
      0L
    )
  }

  inner_splits <- .prediction_make_splits(
    index = index,
    subject_ids = outer_splits$train_subject_ids[[1]],
    outcome = fixture$outcome_cls,
    n_folds = 4L,
    n_repeats = 1L,
    stratify = TRUE,
    seed = 78L
  )

  expect_true(nrow(inner_splits) >= 2L)
  for (i in seq_len(nrow(inner_splits))) {
    expect_length(
      intersect(inner_splits$train_subject_ids[[i]], inner_splits$test_subject_ids[[i]]),
      0L
    )
  }
})

test_that("subject-level split generation preserves group structure across folds", {
  set.seed(206)
  fixture <- .make_multigroup_prediction_fixture()
  index <- .prediction_subject_index(fixture$spec)

  splits <- .prediction_make_splits(
    index = index,
    subject_ids = index$subjects$subject_id,
    outcome = fixture$outcome_cls,
    n_folds = 3L,
    n_repeats = 1L,
    stratify = TRUE,
    seed = 88L
  )

  expect_equal(nrow(splits), 3L)
  for (i in seq_len(nrow(splits))) {
    train_groups <- unique(index$subjects$group[index$subjects$subject_id %in% splits$train_subject_ids[[i]]])
    test_groups <- unique(index$subjects$group[index$subjects$subject_id %in% splits$test_subject_ids[[i]]])

    expect_setequal(train_groups, c("g1", "g2"))
    expect_setequal(test_groups, c("g1", "g2"))
    expect_length(
      intersect(splits$train_subject_ids[[i]], splits$test_subject_ids[[i]]),
      0L
    )
  }
})

test_that("evaluate_prediction supports classification with nested tuning and fixed-seed reproducibility", {
  set.seed(202)
  fixture <- .make_prediction_fixture()

  res1 <- evaluate_prediction(
    spec = fixture$spec,
    outcome = fixture$outcome_cls,
    task = "classification",
    components = 1:2,
    resampling = list(
      outer_folds = 5L,
      outer_repeats = 2L,
      inner_folds = 4L,
      inner_repeats = 1L,
      stratify = TRUE
    ),
    seed = 99L,
    progress = FALSE
  )

  res2 <- evaluate_prediction(
    spec = fixture$spec,
    outcome = fixture$outcome_cls,
    task = "classification",
    components = 1:2,
    resampling = list(
      outer_folds = 5L,
      outer_repeats = 2L,
      inner_folds = 4L,
      inner_repeats = 1L,
      stratify = TRUE
    ),
    seed = 99L,
    progress = FALSE
  )

  expect_s3_class(res1, "predict_cv_result")
  expect_equal(nrow(res1$oof_predictions), 40L)
  expect_true(all(c("accuracy", "auc") %in% names(res1$metrics_by_fold)))
  expect_true(all(res1$metrics_by_fold$selected_components %in% c(1L, 2L)))
  expect_true(all(res1$tuning$summary$n_components %in% c(1L, 2L)))

  expect_equal(res1$splits$train_subject_ids, res2$splits$train_subject_ids)
  expect_equal(res1$splits$test_subject_ids, res2$splits$test_subject_ids)
  expect_equal(res1$oof_predictions, res2$oof_predictions)
  expect_equal(res1$metrics_by_fold, res2$metrics_by_fold)
  expect_equal(res1$tuning$summary, res2$tuning$summary)
})

test_that("evaluate_prediction is invariant to candidate component ordering", {
  set.seed(207)
  fixture <- .make_prediction_fixture()

  res_sorted <- evaluate_prediction(
    spec = fixture$spec,
    outcome = fixture$outcome_cls,
    task = "classification",
    components = 1:2,
    resampling = list(
      outer_folds = 4L,
      outer_repeats = 1L,
      inner_folds = 3L,
      inner_repeats = 1L,
      stratify = TRUE
    ),
    seed = 131L,
    progress = FALSE
  )

  res_unsorted <- evaluate_prediction(
    spec = fixture$spec,
    outcome = fixture$outcome_cls,
    task = "classification",
    components = c(2L, 1L, 2L),
    resampling = list(
      outer_folds = 4L,
      outer_repeats = 1L,
      inner_folds = 3L,
      inner_repeats = 1L,
      stratify = TRUE
    ),
    seed = 131L,
    progress = FALSE
  )

  expect_equal(res_sorted$oof_predictions, res_unsorted$oof_predictions)
  expect_equal(res_sorted$metrics_by_fold, res_unsorted$metrics_by_fold)
  expect_equal(res_sorted$tuning$summary, res_unsorted$tuning$summary)
})

test_that("evaluate_prediction supports regression metrics and returns subject-level predictions", {
  set.seed(203)
  fixture <- .make_prediction_fixture()

  res <- evaluate_prediction(
    spec = fixture$spec,
    outcome = fixture$outcome_reg,
    task = "regression",
    components = 1:2,
    resampling = list(
      outer_folds = 4L,
      outer_repeats = 1L,
      inner_folds = 3L,
      inner_repeats = 1L,
      stratify = FALSE
    ),
    seed = 101L,
    progress = FALSE
  )

  expect_s3_class(res, "predict_cv_result")
  expect_equal(nrow(res$oof_predictions), 20L)
  expect_true(all(c("rmse", "mae", "rsq") %in% names(res$metrics_by_fold)))
  expect_true(is.numeric(res$oof_predictions$estimate))
  expect_true(all(res$metrics_by_fold$selected_components %in% c(1L, 2L)))
  expect_equal(res$primary_metric, "rmse")
})

test_that("evaluate_prediction adds predictive permutation tests and bootstrap intervals for classification", {
  set.seed(204)
  fixture <- .make_prediction_fixture()

  res <- evaluate_prediction(
    spec = fixture$spec,
    outcome = fixture$outcome_cls,
    task = "classification",
    components = 1:2,
    resampling = list(
      outer_folds = 4L,
      outer_repeats = 1L,
      inner_folds = 3L,
      inner_repeats = 1L,
      stratify = TRUE
    ),
    seed = 111L,
    progress = FALSE,
    num_perm = 3L,
    num_boot = 12L
  )

  expect_true(is.list(res$perm_test))
  expect_equal(res$perm_test$metric, res$primary_metric)
  expect_equal(length(res$perm_test$null_distribution), 3L)
  expect_true(res$perm_test$p_value >= 0 && res$perm_test$p_value <= 1)

  expect_true(is.data.frame(res$boot_ci))
  expect_true(all(c("accuracy", "auc") %in% res$boot_ci$metric))
  expect_true(all(res$boot_ci$lower <= res$boot_ci$upper))
})

test_that("predictive summaries, permutation outputs, and bootstrap estimates are internally consistent", {
  set.seed(208)
  fixture <- .make_prediction_fixture()

  res <- evaluate_prediction(
    spec = fixture$spec,
    outcome = fixture$outcome_cls,
    task = "classification",
    components = 1:2,
    resampling = list(
      outer_folds = 4L,
      outer_repeats = 2L,
      inner_folds = 3L,
      inner_repeats = 1L,
      stratify = TRUE
    ),
    seed = 141L,
    progress = FALSE,
    num_perm = 3L,
    num_boot = 12L
  )

  summary_mean <- setNames(res$summary$mean, res$summary$metric)
  fold_mean <- vapply(res$metrics, function(metric) {
    mean(res$metrics_by_fold[[metric]], na.rm = TRUE)
  }, numeric(1))
  expect_equal(summary_mean[res$metrics], fold_mean, tolerance = 1e-12)

  expect_equal(
    res$perm_test$observed,
    summary_mean[[res$primary_metric]],
    tolerance = 1e-12
  )

  subj_oof <- .prediction_subject_level_oof(res)
  subj_metrics <- .prediction_compute_metrics(
    task = res$task,
    truth = subj_oof$truth,
    estimate = subj_oof$estimate,
    probability = subj_oof$probability,
    metrics = res$metrics
  )
  boot_est <- setNames(res$boot_ci$estimate, res$boot_ci$metric)
  expect_equal(boot_est[res$metrics], subj_metrics[res$metrics], tolerance = 1e-12)
})

test_that("evaluate_prediction adds predictive inference outputs for regression", {
  set.seed(205)
  fixture <- .make_prediction_fixture()

  res <- evaluate_prediction(
    spec = fixture$spec,
    outcome = fixture$outcome_reg,
    task = "regression",
    components = 1:2,
    resampling = list(
      outer_folds = 4L,
      outer_repeats = 1L,
      inner_folds = 3L,
      inner_repeats = 1L,
      stratify = FALSE
    ),
    seed = 121L,
    progress = FALSE,
    num_perm = 3L,
    num_boot = 12L
  )

  expect_true(is.list(res$perm_test))
  expect_equal(res$perm_test$metric, "rmse")
  expect_equal(length(res$perm_test$null_distribution), 3L)

  expect_true(is.data.frame(res$boot_ci))
  expect_true(all(c("rmse", "mae", "rsq") %in% res$boot_ci$metric))
  expect_true(all(res$boot_ci$lower <= res$boot_ci$upper | is.na(res$boot_ci$lower) | is.na(res$boot_ci$upper)))
})

test_that("evaluate_prediction validates malformed inputs and unsupported prediction modes", {
  set.seed(209)
  fixture <- .make_prediction_fixture()

  expect_error(
    evaluate_prediction(
      spec = fixture$spec,
      outcome = fixture$outcome_cls[-1],
      task = "classification",
      progress = FALSE
    ),
    "Outcome length must equal"
  )

  multiblock_spec <- fixture$spec
  multiblock_spec$method <- 4L
  expect_error(
    evaluate_prediction(
      spec = multiblock_spec,
      outcome = fixture$outcome_cls,
      task = "classification",
      progress = FALSE
    ),
    "not yet implemented for multiblock"
  )

  ssb_spec <- fixture$spec
  ssb_spec$num_subj_lst <- list(c(10L, 10L))
  expect_error(
    evaluate_prediction(
      spec = ssb_spec,
      outcome = fixture$outcome_cls,
      task = "classification",
      progress = FALSE
    ),
    "requires balanced subject counts"
  )
})

test_that("degenerate learner fallbacks produce finite predictions", {
  x_train <- data.frame(a = c(-1, 0, 1), b = c(1, 1, 1))
  x_test <- data.frame(a = c(-0.5, 0.5), b = c(1, 1))

  cls_model <- .prediction_fit_model(
    task = "classification",
    x = x_train,
    y = factor(rep("control", 3), levels = c("control", "case"))
  )
  cls_pred <- .prediction_predict_model(cls_model, x_test, task = "classification")
  expect_identical(cls_model$type, "constant_class")
  expect_true(all(is.finite(cls_pred$probability)))
  expect_true(all(as.character(cls_pred$estimate) == "control"))

  reg_model <- .prediction_fit_model(
    task = "regression",
    x = x_train,
    y = c(2, 2, 2)
  )
  reg_pred <- .prediction_predict_model(reg_model, x_test, task = "regression")
  expect_identical(reg_model$type, "constant_regression")
  expect_true(all(is.finite(reg_pred$estimate)))
  expect_equal(unique(reg_pred$estimate), 2)
})
