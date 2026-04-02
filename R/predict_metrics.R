#' Predictive CV Metrics and Inference Helpers
#'
#' Internal helpers for predictive metrics, subject-level out-of-fold summaries,
#' and permutation/bootstrap summaries.
#'
#' @keywords internal
NULL

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

  out <- stats::setNames(numeric(length(metrics)), metrics)
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
