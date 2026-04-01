#' Predictive Cross-Validation for PLS
#'
#' @description
#' Leakage-safe predictive evaluation for fitted PLS pipelines. The
#' decomposition is refit inside every training fold and held-out subjects are
#' projected with training-only artifacts via [project_scores()].
#'
#' @name predict-cv
NULL

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
