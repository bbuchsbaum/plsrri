#' Predictive Cross-Validation for PLS
#'
#' @description
#' Leakage-safe predictive evaluation for fitted PLS pipelines. The
#' decomposition is refit inside every training fold and held-out subjects are
#' projected with training-only artifacts via [project_scores()].
#'
#' @name predict-cv
NULL

.prediction_resampling_defaults <- function(task) {
  list(
    outer_folds = 5L,
    outer_repeats = 1L,
    inner_folds = 5L,
    inner_repeats = 1L,
    stratify = identical(task, "classification")
  )
}

.prediction_resampling_config <- function(task, resampling = NULL) {
  cfg <- utils::modifyList(.prediction_resampling_defaults(task), resampling %||% list())

  if (!is.null(cfg$folds)) {
    cfg$outer_folds <- cfg$outer_folds %||% cfg$folds
    cfg$inner_folds <- cfg$inner_folds %||% cfg$folds
  }
  if (!is.null(cfg$repeats)) {
    cfg$outer_repeats <- cfg$outer_repeats %||% cfg$repeats
    cfg$inner_repeats <- cfg$inner_repeats %||% cfg$repeats
  }

  cfg$outer_folds <- as.integer(cfg$outer_folds)
  cfg$outer_repeats <- as.integer(cfg$outer_repeats)
  cfg$inner_folds <- as.integer(cfg$inner_folds)
  cfg$inner_repeats <- as.integer(cfg$inner_repeats)
  cfg$stratify <- isTRUE(cfg$stratify)

  if (cfg$outer_folds < 2L || cfg$inner_folds < 2L) {
    stop("Prediction resampling requires at least 2 folds for both outer and inner CV.", call. = FALSE)
  }
  if (cfg$outer_repeats < 1L || cfg$inner_repeats < 1L) {
    stop("Prediction resampling repeats must be positive integers.", call. = FALSE)
  }

  cfg
}

.prediction_validate_outcome <- function(outcome, task, n_subjects) {
  if (length(outcome) != n_subjects) {
    stop(
      sprintf(
        "Outcome length must equal the number of subjects (%d).",
        n_subjects
      ),
      call. = FALSE
    )
  }

  if (task == "classification") {
    if (is.logical(outcome)) {
      outcome <- factor(ifelse(outcome, "TRUE", "FALSE"))
    } else if (is.factor(outcome)) {
      outcome <- droplevels(outcome)
    } else if (is.character(outcome)) {
      outcome <- factor(outcome)
    } else if (is.numeric(outcome)) {
      if (!all(outcome %in% c(0, 1))) {
        stop("Numeric classification outcomes must be coded as 0/1.", call. = FALSE)
      }
      outcome <- factor(outcome, levels = c(0, 1))
    } else {
      stop("Unsupported classification outcome type.", call. = FALSE)
    }

    if (nlevels(outcome) != 2L) {
      stop("Classification currently supports binary outcomes only.", call. = FALSE)
    }
    return(outcome)
  }

  if (!is.numeric(outcome)) {
    stop("Regression outcomes must be numeric.", call. = FALSE)
  }

  as.numeric(outcome)
}

.prediction_subject_index <- function(spec) {
  assert_that(inherits(spec, "pls_spec"))

  if (is.list(spec$num_subj_lst)) {
    stop(
      "Predictive cross-validation currently requires balanced subject counts per group.",
      call. = FALSE
    )
  }

  n_by_group <- as.integer(spec$num_subj_lst)
  k <- as.integer(spec$num_cond)
  groups <- spec$groups %||% paste0("group_", seq_along(n_by_group))

  subjects <- vector("list", sum(n_by_group))
  row_index <- vector("list", sum(n_by_group))

  subject_id <- 1L
  row_offset <- 0L

  for (g in seq_along(n_by_group)) {
    n <- n_by_group[g]
    for (s in seq_len(n)) {
      rows <- row_offset + s + n * (seq_len(k) - 1L)
      subjects[[subject_id]] <- data.frame(
        subject_id = subject_id,
        group_index = g,
        group = as.character(groups[g]),
        subject_in_group = s,
        stringsAsFactors = FALSE
      )
      row_index[[subject_id]] <- as.integer(rows)
      subject_id <- subject_id + 1L
    }
    row_offset <- row_offset + n * k
  }

  list(
    subjects = do.call(rbind, subjects),
    row_index = row_index,
    n_subjects = sum(n_by_group),
    num_cond = k
  )
}

.prediction_order_subjects <- function(index, subject_ids) {
  subject_ids <- unique(as.integer(subject_ids))
  tab <- index$subjects
  ord <- tab$subject_id[tab$subject_id %in% subject_ids]
  ord[order(match(ord, tab$subject_id))]
}

.prediction_subset_spec <- function(spec, subject_ids, index) {
  subject_ids <- .prediction_order_subjects(index, subject_ids)
  tab <- index$subjects[index$subjects$subject_id %in% subject_ids, , drop = FALSE]
  tab <- tab[order(tab$group_index, tab$subject_in_group), , drop = FALSE]

  out <- spec
  out$num_perm <- 0L
  out$num_boot <- 0L
  out$num_split <- 0L

  out$datamat_lst <- vector("list", length(spec$datamat_lst))
  out$num_subj_lst <- integer(length(spec$datamat_lst))

  for (g in seq_along(spec$datamat_lst)) {
    gtab <- tab[tab$group_index == g, , drop = FALSE]
    n_train <- nrow(gtab)
    if (n_train < 1L) {
      stop("Each predictive split must retain at least one subject per group.", call. = FALSE)
    }

    n_full <- as.integer(spec$num_subj_lst[g])
    rows <- as.integer(unlist(lapply(gtab$subject_in_group, function(s) {
      s + n_full * (seq_len(spec$num_cond) - 1L)
    }), use.names = FALSE))

    out$datamat_lst[[g]] <- spec$datamat_lst[[g]][rows, , drop = FALSE]
    out$num_subj_lst[g] <- n_train
  }

  global_rows <- sort(as.integer(unlist(index$row_index[subject_ids], use.names = FALSE)))
  if (!is.null(spec$stacked_behavdata)) {
    out$stacked_behavdata <- spec$stacked_behavdata[global_rows, , drop = FALSE]
  }

  out
}

.prediction_assign_fold_labels <- function(ids, n_folds) {
  ids <- sample(as.integer(ids), length(ids), replace = FALSE)
  fold_ids <- cut(seq_along(ids), breaks = n_folds, labels = FALSE)
  out <- vector("list", n_folds)
  for (f in seq_len(n_folds)) {
    out[[f]] <- ids[fold_ids == f]
  }
  out
}

.prediction_make_splits <- function(index,
                                    subject_ids,
                                    outcome,
                                    n_folds,
                                    n_repeats,
                                    stratify = FALSE,
                                    seed = NULL) {
  subject_ids <- .prediction_order_subjects(index, subject_ids)
  tab <- index$subjects[index$subjects$subject_id %in% subject_ids, , drop = FALSE]
  min_group_n <- min(table(tab$group_index))
  actual_folds <- min(as.integer(n_folds), as.integer(min_group_n))

  if (actual_folds < 2L) {
    stop("Predictive cross-validation requires at least 2 subjects per group in every training split.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(as.integer(seed)[1])
  }

  splits <- vector("list", actual_folds * as.integer(n_repeats))
  idx <- 1L

  for (rep_idx in seq_len(as.integer(n_repeats))) {
    fold_members <- vector("list", actual_folds)
    for (f in seq_len(actual_folds)) {
      fold_members[[f]] <- integer(0)
    }

    for (g in unique(tab$group_index)) {
      group_ids <- tab$subject_id[tab$group_index == g]

      if (isTRUE(stratify)) {
        class_levels <- split(group_ids, outcome[group_ids])
        for (class_ids in class_levels) {
          assigned <- .prediction_assign_fold_labels(class_ids, actual_folds)
          for (f in seq_len(actual_folds)) {
            fold_members[[f]] <- c(fold_members[[f]], assigned[[f]] %||% integer(0))
          }
        }
      } else {
        assigned <- .prediction_assign_fold_labels(group_ids, actual_folds)
        for (f in seq_len(actual_folds)) {
          fold_members[[f]] <- c(fold_members[[f]], assigned[[f]] %||% integer(0))
        }
      }
    }

    all_ids <- .prediction_order_subjects(index, subject_ids)

    for (f in seq_len(actual_folds)) {
      test_ids <- sort(unique(as.integer(fold_members[[f]])))
      train_ids <- setdiff(all_ids, test_ids)

      splits[[idx]] <- data.frame(
        `repeat` = rep_idx,
        fold = f,
        train_subject_ids = I(list(train_ids)),
        test_subject_ids = I(list(test_ids)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  do.call(rbind, splits)
}

.prediction_subject_matrix <- function(score_mat, num_subj_lst, num_cond, n_components) {
  stopifnot(is.matrix(score_mat))
  n_components <- as.integer(n_components)
  k <- as.integer(num_cond)
  n_by_group <- as.integer(num_subj_lst)

  pieces <- vector("list", length(n_by_group))
  row_offset <- 0L

  for (g in seq_along(n_by_group)) {
    n <- n_by_group[g]
    g_rows <- row_offset + seq_len(n * k)
    g_scores <- score_mat[g_rows, seq_len(n_components), drop = FALSE]

    cols <- lapply(seq_len(n_components), function(comp) {
      matrix(g_scores[, comp], nrow = n, ncol = k)
    })
    pieces[[g]] <- do.call(cbind, cols)
    row_offset <- row_offset + n * k
  }

  out <- do.call(rbind, pieces)
  colnames(out) <- as.vector(vapply(seq_len(n_components), function(comp) {
    paste0("LV", comp, "_C", seq_len(k))
  }, character(k)))
  out
}

.prediction_primary_metric_default <- function(task) {
  if (task == "classification") "accuracy" else "rmse"
}

.prediction_metrics_default <- function(task) {
  if (task == "classification") c("accuracy", "auc") else c("rmse", "mae", "rsq")
}

.prediction_metric_direction <- function(metric) {
  if (metric %in% c("accuracy", "auc", "rsq")) "maximize" else "minimize"
}

.prediction_binary_auc <- function(truth, probability) {
  truth <- droplevels(as.factor(truth))
  if (nlevels(truth) != 2L || length(unique(truth)) < 2L) {
    return(NA_real_)
  }

  pos <- truth == levels(truth)[2L]
  n_pos <- sum(pos)
  n_neg <- sum(!pos)
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[pos]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

.prediction_compute_metrics <- function(task,
                                        truth,
                                        estimate,
                                        probability = NULL,
                                        metrics = NULL) {
  metrics <- metrics %||% .prediction_metrics_default(task)

  out <- setNames(numeric(length(metrics)), metrics)
  for (metric in metrics) {
    out[[metric]] <- switch(
      metric,
      accuracy = mean(estimate == truth),
      auc = .prediction_binary_auc(truth, probability),
      rmse = sqrt(mean((estimate - truth)^2)),
      mae = mean(abs(estimate - truth)),
      rsq = {
        denom <- sum((truth - mean(truth))^2)
        if (denom <= 0) NA_real_ else 1 - sum((truth - estimate)^2) / denom
      },
      stop("Unsupported predictive metric: ", metric, call. = FALSE)
    )
  }

  out
}

.prediction_fit_model <- function(task, x, y) {
  x <- as.data.frame(x)
  colnames(x) <- paste0("x", seq_len(ncol(x)))

  if (task == "classification") {
    y <- as.factor(y)
    positive <- levels(y)[2L]
    baseline_prob <- mean(y == positive)

    if (length(unique(y)) < 2L) {
      return(list(
        type = "constant_class",
        value = levels(y)[1L],
        baseline_prob = baseline_prob,
        levels = levels(y)
      ))
    }

    fit <- suppressWarnings(
      stats::glm(
        y ~ .,
        data = data.frame(y = y, x, check.names = FALSE),
        family = stats::binomial()
      )
    )

    return(list(
      type = "glm",
      fit = fit,
      baseline_prob = baseline_prob,
      levels = levels(y)
    ))
  }

  baseline <- mean(y)
  if (stats::sd(y) == 0) {
    return(list(type = "constant_regression", value = baseline))
  }

  fit <- stats::lm(y ~ ., data = data.frame(y = y, x, check.names = FALSE))
  list(type = "lm", fit = fit, value = baseline)
}

.prediction_predict_model <- function(model, newx, task) {
  newx <- as.data.frame(newx)
  colnames(newx) <- paste0("x", seq_len(ncol(newx)))

  if (task == "classification") {
    if (identical(model$type, "constant_class")) {
      prob <- rep(model$baseline_prob, nrow(newx))
      cls <- factor(
        rep(model$value, nrow(newx)),
        levels = model$levels
      )
      return(list(estimate = cls, probability = prob))
    }

    prob <- suppressWarnings(stats::predict(model$fit, newdata = newx, type = "response"))
    prob[!is.finite(prob)] <- model$baseline_prob
    prob <- pmin(pmax(prob, 0), 1)

    cls <- ifelse(prob >= 0.5, model$levels[2L], model$levels[1L])
    return(list(
      estimate = factor(cls, levels = model$levels),
      probability = prob
    ))
  }

  if (identical(model$type, "constant_regression")) {
    pred <- rep(model$value, nrow(newx))
  } else {
    pred <- suppressWarnings(stats::predict(model$fit, newdata = newx))
    pred[!is.finite(pred)] <- model$value
  }

  list(estimate = as.numeric(pred), probability = NULL)
}

.prediction_select_components <- function(summary_df, primary_metric) {
  direction <- .prediction_metric_direction(primary_metric)
  stats <- summary_df$primary_metric_mean

  if (direction == "maximize") {
    best <- which.max(stats)
  } else {
    best <- which.min(stats)
  }

  as.integer(summary_df$n_components[best][1])
}

.prediction_metric_summary <- function(metrics_by_fold, metrics) {
  do.call(rbind, lapply(metrics, function(metric) {
    vals <- metrics_by_fold[[metric]]
    data.frame(
      metric = metric,
      mean = mean(vals, na.rm = TRUE),
      sd = stats::sd(vals, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

.prediction_subject_level_oof <- function(result) {
  df <- result$oof_predictions
  split_rows <- split(seq_len(nrow(df)), df$subject_id)

  if (result$task == "classification") {
    levels_truth <- levels(df$truth)
    rows <- lapply(split_rows, function(idx) {
      sub <- df[idx, , drop = FALSE]
      prob <- mean(sub$probability, na.rm = TRUE)
      est <- factor(
        ifelse(prob >= 0.5, levels_truth[2L], levels_truth[1L]),
        levels = levels_truth
      )
      data.frame(
        subject_id = sub$subject_id[1],
        group = sub$group[1],
        truth = factor(sub$truth[1], levels = levels_truth),
        estimate = est,
        probability = prob,
        stringsAsFactors = FALSE
      )
    })
  } else {
    rows <- lapply(split_rows, function(idx) {
      sub <- df[idx, , drop = FALSE]
      data.frame(
        subject_id = sub$subject_id[1],
        group = sub$group[1],
        truth = as.numeric(sub$truth[1]),
        estimate = mean(sub$estimate, na.rm = TRUE),
        probability = NA_real_,
        stringsAsFactors = FALSE
      )
    })
  }

  out <- do.call(rbind, rows)
  out[order(out$subject_id), , drop = FALSE]
}

.prediction_permute_outcome <- function(outcome, index) {
  permuted <- outcome
  tab <- index$subjects
  for (g in unique(tab$group_index)) {
    ids <- tab$subject_id[tab$group_index == g]
    permuted[ids] <- sample(outcome[ids], length(ids), replace = FALSE)
  }
  permuted
}

.prediction_permutation_test <- function(result,
                                         spec,
                                         outcome,
                                         index,
                                         task,
                                         components,
                                         resampling,
                                         metrics,
                                         primary_metric,
                                         num_perm,
                                         seed = NULL) {
  observed <- result$summary$mean[result$summary$metric == primary_metric][1]
  null_values <- numeric(num_perm)
  direction <- .prediction_metric_direction(primary_metric)

  if (!is.null(seed)) {
    set.seed(as.integer(seed)[1] + 10000L)
  }

  for (p in seq_len(num_perm)) {
    perm_outcome <- .prediction_permute_outcome(outcome, index)
    perm_seed <- if (is.null(seed)) NULL else as.integer(seed)[1] + 10000L + p
    perm_res <- evaluate_prediction(
      spec = spec,
      outcome = perm_outcome,
      task = task,
      components = components,
      resampling = resampling,
      metrics = metrics,
      primary_metric = primary_metric,
      seed = perm_seed,
      progress = FALSE,
      num_perm = 0L,
      num_boot = 0L
    )
    null_values[p] <- perm_res$summary$mean[perm_res$summary$metric == primary_metric][1]
  }

  p_value <- if (direction == "maximize") {
    (sum(null_values >= observed) + 1) / (num_perm + 1)
  } else {
    (sum(null_values <= observed) + 1) / (num_perm + 1)
  }

  list(
    metric = primary_metric,
    observed = observed,
    null_distribution = null_values,
    p_value = p_value,
    num_perm = as.integer(num_perm)
  )
}

.prediction_bootstrap_ci <- function(result,
                                     metrics,
                                     num_boot,
                                     clim = 95,
                                     seed = NULL) {
  subj <- .prediction_subject_level_oof(result)
  observed <- .prediction_compute_metrics(
    task = result$task,
    truth = subj$truth,
    estimate = subj$estimate,
    probability = subj$probability,
    metrics = metrics
  )
  metric_mat <- matrix(NA_real_, nrow = num_boot, ncol = length(metrics))

  if (!is.null(seed)) {
    set.seed(as.integer(seed)[1] + 20000L)
  }

  for (b in seq_len(num_boot)) {
    rows <- sample(seq_len(nrow(subj)), nrow(subj), replace = TRUE)
    samp <- subj[rows, , drop = FALSE]
    vals <- .prediction_compute_metrics(
      task = result$task,
      truth = samp$truth,
      estimate = samp$estimate,
      probability = samp$probability,
      metrics = metrics
    )
    metric_mat[b, ] <- vals[metrics]
  }

  alpha <- (100 - clim) / 2
  ci <- t(vapply(seq_along(metrics), function(j) {
    pls_percentile(metric_mat[, j], c(alpha, 100 - alpha))
  }, numeric(2)))

  data.frame(
    metric = metrics,
    estimate = observed[metrics],
    lower = ci[, 1],
    upper = ci[, 2],
    clim = clim,
    num_boot = as.integer(num_boot),
    stringsAsFactors = FALSE
  )
}

.new_predict_cv_result <- function(task,
                                   method,
                                   primary_metric,
                                   metrics,
                                   resampling,
                                   splits,
                                   oof_predictions,
                                   metrics_by_fold,
                                   tuning,
                                   summary = NULL,
                                   perm_test = NULL,
                                   boot_ci = NULL) {
  structure(
    list(
      task = task,
      method = method,
      primary_metric = primary_metric,
      metrics = metrics,
      resampling = resampling,
      splits = splits,
      oof_predictions = oof_predictions,
      metrics_by_fold = metrics_by_fold,
      tuning = tuning,
      summary = summary,
      perm_test = perm_test,
      boot_ci = boot_ci
    ),
    class = "predict_cv_result"
  )
}

#' @export
print.predict_cv_result <- function(x, ...) {
  cat("<predict_cv_result>\n")
  cat("  task:", x$task, "\n")
  cat("  method:", x$method, "\n")
  cat("  outer splits:", nrow(x$splits), "\n")
  cat("  primary metric:", x$primary_metric, "\n")
  if (!is.null(x$perm_test)) {
    cat("  permutation test:", x$perm_test$num_perm, "permutations\n")
  }
  if (!is.null(x$boot_ci)) {
    cat("  bootstrap CI:", unique(x$boot_ci$num_boot), "resamples\n")
  }
  invisible(x)
}

#' Evaluate Predictive Performance with Nested Cross-Validation
#'
#' @param spec A `pls_spec` object.
#' @param outcome Subject-level outcome vector ordered by group, then subject.
#' @param task `"classification"` or `"regression"`.
#' @param components Optional integer vector of candidate LV counts. When NULL,
#'   each outer split tunes over `1:min(5, available_components)`.
#' @param resampling Optional list controlling nested CV. Supported fields are
#'   `outer_folds`, `outer_repeats`, `inner_folds`, `inner_repeats`,
#'   `stratify`, plus shorthand `folds`/`repeats`.
#' @param metrics Optional character vector of metric names.
#' @param primary_metric Optional metric used for inner-loop selection.
#' @param seed Optional integer seed for reproducible split generation.
#' @param progress Logical; show a progress bar.
#' @param num_perm Optional number of subject-level label permutations for
#'   predictive inference.
#' @param num_boot Optional number of subject-level bootstrap resamples for
#'   predictive confidence intervals.
#' @param clim Confidence level for bootstrap intervals.
#'
#' @return A `predict_cv_result` list with out-of-fold predictions, per-fold
#'   metrics, split membership, and tuning traces.
#' @export
evaluate_prediction <- function(spec,
                                outcome,
                                task = c("classification", "regression"),
                                components = NULL,
                                resampling = NULL,
                                metrics = NULL,
                                primary_metric = NULL,
                                seed = NULL,
                                progress = TRUE,
                                num_perm = 0L,
                                num_boot = 0L,
                                clim = 95) {
  task <- match.arg(task)
  spec <- .materialize_pls_spec(spec, derive_seed_behavior = TRUE)
  .validate_spec(spec)

  method_int <- as.integer(spec$method)[1]
  if (method_int %in% c(4L, 6L)) {
    stop("Predictive evaluation is not yet implemented for multiblock PLS methods.", call. = FALSE)
  }

  index <- .prediction_subject_index(spec)
  outcome <- .prediction_validate_outcome(outcome, task = task, n_subjects = index$n_subjects)
  cfg <- .prediction_resampling_config(task = task, resampling = resampling)

  metrics <- metrics %||% .prediction_metrics_default(task)
  primary_metric <- primary_metric %||% .prediction_primary_metric_default(task)

  if (!primary_metric %in% metrics) {
    metrics <- unique(c(primary_metric, metrics))
  }

  method <- get_method(pls_method_int_to_name(method_int))
  outer_splits <- .prediction_make_splits(
    index = index,
    subject_ids = index$subjects$subject_id,
    outcome = outcome,
    n_folds = cfg$outer_folds,
    n_repeats = cfg$outer_repeats,
    stratify = cfg$stratify,
    seed = seed
  )

  n_outer <- nrow(outer_splits)
  pb <- NULL
  if (isTRUE(progress)) {
    pb <- cli::cli_progress_bar(
      "Nested predictive CV",
      total = n_outer,
      clear = TRUE
    )
    on.exit(cli::cli_progress_done(id = pb), add = TRUE)
  }

  oof_rows <- vector("list", n_outer)
  fold_rows <- vector("list", n_outer)
  tuning_summary_rows <- vector("list", n_outer)
  inner_metric_rows <- vector("list", n_outer)

  for (i in seq_len(n_outer)) {
    split <- outer_splits[i, , drop = FALSE]
    train_ids <- split$train_subject_ids[[1]]
    test_ids <- split$test_subject_ids[[1]]

    outer_train_spec <- .prediction_subset_spec(spec, train_ids, index)
    outer_test_spec <- .prediction_subset_spec(spec, test_ids, index)

    outer_fit <- method$fit(outer_train_spec, progress = FALSE)
    available_outer <- ncol(feature_weights(outer_fit))
    candidate_components <- if (is.null(components)) {
      seq_len(min(5L, available_outer))
    } else {
      sort(unique(as.integer(components)))
    }
    candidate_components <- candidate_components[candidate_components >= 1L & candidate_components <= available_outer]
    if (!length(candidate_components)) {
      stop("No valid component counts remain after filtering against the fitted outer model.", call. = FALSE)
    }

    inner_seed <- if (is.null(seed)) NULL else as.integer(seed) + i
    inner_splits <- .prediction_make_splits(
      index = index,
      subject_ids = train_ids,
      outcome = outcome,
      n_folds = cfg$inner_folds,
      n_repeats = cfg$inner_repeats,
      stratify = cfg$stratify,
      seed = inner_seed
    )

    inner_rows <- vector("list", nrow(inner_splits))
    for (j in seq_len(nrow(inner_splits))) {
      inner_split <- inner_splits[j, , drop = FALSE]
      inner_train_ids <- inner_split$train_subject_ids[[1]]
      inner_test_ids <- inner_split$test_subject_ids[[1]]

      inner_train_spec <- .prediction_subset_spec(spec, inner_train_ids, index)
      inner_test_spec <- .prediction_subset_spec(spec, inner_test_ids, index)

      inner_fit <- method$fit(inner_train_spec, progress = FALSE)
      inner_available <- ncol(feature_weights(inner_fit))
      inner_candidates <- candidate_components[candidate_components <= inner_available]
      train_scores <- scores(inner_fit, type = "feature")
      test_scores <- project_scores(inner_fit, inner_test_spec, type = "feature")

      metrics_for_split <- vector("list", length(inner_candidates))
      for (k_idx in seq_along(inner_candidates)) {
        k_comp <- inner_candidates[k_idx]
        x_train <- .prediction_subject_matrix(
          train_scores,
          num_subj_lst = inner_train_spec$num_subj_lst,
          num_cond = inner_train_spec$num_cond,
          n_components = k_comp
        )
        x_test <- .prediction_subject_matrix(
          test_scores,
          num_subj_lst = inner_test_spec$num_subj_lst,
          num_cond = inner_test_spec$num_cond,
          n_components = k_comp
        )

        ord_train <- .prediction_order_subjects(index, inner_train_ids)
        ord_test <- .prediction_order_subjects(index, inner_test_ids)
        model <- .prediction_fit_model(task, x_train, outcome[ord_train])
        pred <- .prediction_predict_model(model, x_test, task)
        vals <- .prediction_compute_metrics(
          task = task,
          truth = outcome[ord_test],
          estimate = pred$estimate,
          probability = pred$probability,
          metrics = metrics
        )

        metrics_for_split[[k_idx]] <- data.frame(
          outer_repeat = split[["repeat"]],
          outer_fold = split$fold,
          inner_repeat = inner_split[["repeat"]],
          inner_fold = inner_split$fold,
          n_components = k_comp,
          metric = names(vals),
          value = as.numeric(vals),
          stringsAsFactors = FALSE
        )
      }

      inner_rows[[j]] <- do.call(rbind, metrics_for_split)
    }

    inner_metrics <- do.call(rbind, inner_rows)
    primary_rows <- inner_metrics[inner_metrics$metric == primary_metric, , drop = FALSE]
    tuning_summary <- stats::aggregate(
      value ~ n_components,
      data = primary_rows,
      FUN = mean
    )
    names(tuning_summary)[names(tuning_summary) == "value"] <- "primary_metric_mean"
    tuning_summary[["repeat"]] <- split[["repeat"]]
    tuning_summary$fold <- split$fold
    tuning_summary$selected <- FALSE

    best_k <- .prediction_select_components(tuning_summary, primary_metric = primary_metric)
    tuning_summary$selected[tuning_summary$n_components == best_k] <- TRUE

    outer_train_scores <- scores(outer_fit, type = "feature")
    outer_test_scores <- project_scores(outer_fit, outer_test_spec, type = "feature")
    x_outer_train <- .prediction_subject_matrix(
      outer_train_scores,
      num_subj_lst = outer_train_spec$num_subj_lst,
      num_cond = outer_train_spec$num_cond,
      n_components = best_k
    )
    x_outer_test <- .prediction_subject_matrix(
      outer_test_scores,
      num_subj_lst = outer_test_spec$num_subj_lst,
      num_cond = outer_test_spec$num_cond,
      n_components = best_k
    )

    ord_outer_train <- .prediction_order_subjects(index, train_ids)
    ord_outer_test <- .prediction_order_subjects(index, test_ids)
    outer_model <- .prediction_fit_model(task, x_outer_train, outcome[ord_outer_train])
    outer_pred <- .prediction_predict_model(outer_model, x_outer_test, task)
    outer_metrics <- .prediction_compute_metrics(
      task = task,
      truth = outcome[ord_outer_test],
      estimate = outer_pred$estimate,
      probability = outer_pred$probability,
      metrics = metrics
    )

    test_tab <- index$subjects[index$subjects$subject_id %in% ord_outer_test, , drop = FALSE]
    test_tab <- test_tab[order(test_tab$group_index, test_tab$subject_in_group), , drop = FALSE]

    oof_rows[[i]] <- data.frame(
      `repeat` = split[["repeat"]],
      fold = split$fold,
      subject_id = ord_outer_test,
      group = test_tab$group,
      truth = outcome[ord_outer_test],
      estimate = outer_pred$estimate,
      probability = if (task == "classification") outer_pred$probability else NA_real_,
      selected_components = best_k,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    fold_rows[[i]] <- data.frame(
      `repeat` = split[["repeat"]],
      fold = split$fold,
      n_train = length(ord_outer_train),
      n_test = length(ord_outer_test),
      selected_components = best_k,
      as.list(outer_metrics),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    tuning_summary_rows[[i]] <- tuning_summary
    inner_metric_rows[[i]] <- inner_metrics

    if (!is.null(pb)) {
      cli::cli_progress_update(id = pb, set = i)
    }
  }

  result <- .new_predict_cv_result(
    task = task,
    method = method$name,
    primary_metric = primary_metric,
    metrics = metrics,
    resampling = cfg,
    splits = outer_splits,
    oof_predictions = do.call(rbind, oof_rows),
    metrics_by_fold = do.call(rbind, fold_rows),
    tuning = list(
      summary = do.call(rbind, tuning_summary_rows),
      inner_metrics = do.call(rbind, inner_metric_rows)
    ),
    summary = .prediction_metric_summary(do.call(rbind, fold_rows), metrics)
  )

  num_perm <- as.integer(num_perm)[1]
  num_boot <- as.integer(num_boot)[1]

  if (num_perm > 0L) {
    result$perm_test <- .prediction_permutation_test(
      result = result,
      spec = spec,
      outcome = outcome,
      index = index,
      task = task,
      components = components,
      resampling = resampling,
      metrics = metrics,
      primary_metric = primary_metric,
      num_perm = num_perm,
      seed = seed
    )
  }

  if (num_boot > 0L) {
    result$boot_ci <- .prediction_bootstrap_ci(
      result = result,
      metrics = metrics,
      num_boot = num_boot,
      clim = clim,
      seed = seed
    )
  }

  result
}
