#' Predictive CV Model Helpers
#'
#' Internal helpers for component selection and downstream supervised models
#' fitted on projected PLS scores.
#'
#' @keywords internal
NULL

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
