#' Tidy Design Scores
#'
#' @description
#' Extract design scores from a PLS result and optionally summarize them by
#' group and condition. A `condition_key` can add design factors, such as task
#' or difficulty level, back onto the flattened PLS condition labels.
#'
#' @param x A `pls_result` object.
#' @param lv Latent variable to extract.
#' @param condition_key Optional data frame with one row per condition and a
#'   `condition` column. Additional columns are joined onto the returned table.
#' @param summarize Logical; when `TRUE`, return group-condition means and
#'   standard errors. When `FALSE`, return observation-level scores.
#' @param block Optional score block to keep for multiblock results. If omitted
#'   and a task block is present, the task block is used.
#'
#' @return A data frame containing score values and design metadata.
#' @export
as_design_scores <- function(x,
                             lv = 1L,
                             condition_key = NULL,
                             summarize = TRUE,
                             block = NULL) {
  UseMethod("as_design_scores")
}

#' @export
as_design_scores.pls_result <- function(x,
                                        lv = 1L,
                                        condition_key = NULL,
                                        summarize = TRUE,
                                        block = NULL) {
  sc <- scores(x, type = "design", lv = lv)
  df <- data.frame(
    score = as.numeric(sc[, 1L]),
    .build_score_metadata(x, "design"),
    stringsAsFactors = FALSE
  )

  df <- .filter_score_block(df, block)
  df <- .join_condition_key(df, condition_key)

  if (!isTRUE(summarize)) {
    return(df)
  }

  .summarize_design_scores(df)
}

#' Factorial Design Contrasts for a Latent Variable
#'
#' @description
#' Decompose the summarized LV design scores into an effect-coded factorial
#' model. The returned magnitudes show how much of the flattened design-score
#' pattern lies in each main-effect or interaction subspace.
#'
#' @param x A `pls_result` object.
#' @param lv Latent variable to summarize.
#' @param condition_key Optional data frame with one row per condition and a
#'   `condition` column.
#' @param factors Character vector of factor columns to include. Defaults to
#'   `group` plus every column supplied by `condition_key` except `condition`.
#' @param block Optional score block to keep for multiblock results.
#'
#' @return A data frame with one row per factorial term.
#' @export
as_design_contrasts <- function(x,
                                lv = 1L,
                                condition_key = NULL,
                                factors = NULL,
                                block = NULL) {
  UseMethod("as_design_contrasts")
}

#' @export
as_design_contrasts.pls_result <- function(x,
                                           lv = 1L,
                                           condition_key = NULL,
                                           factors = NULL,
                                           block = NULL) {
  df <- as_design_scores(
    x,
    lv = lv,
    condition_key = condition_key,
    summarize = TRUE,
    block = block
  )

  if (is.null(factors)) {
    key_factors <- if (is.null(condition_key)) character(0) else {
      setdiff(names(condition_key), "condition")
    }
    factors <- c("group", key_factors)
  }

  factors <- intersect(factors, names(df))
  factors <- factors[vapply(df[factors], function(z) length(unique(z)) > 1L, logical(1))]
  if (!length(factors)) {
    stop("No varying design factors are available for contrast decomposition.", call. = FALSE)
  }

  contrast_data <- df[, c("mean", factors), drop = FALSE]
  for (nm in factors) {
    contrast_data[[nm]] <- factor(contrast_data[[nm]], levels = unique(contrast_data[[nm]]))
  }

  formula <- stats::as.formula(paste("~", paste(factors, collapse = " * ")))
  contrast_args <- stats::setNames(
    rep(list(stats::contr.sum), length(factors)),
    factors
  )
  model <- stats::model.matrix(formula, contrast_data, contrasts.arg = contrast_args)
  assign <- attr(model, "assign")
  terms <- attr(stats::terms(formula), "term.labels")
  scores_centered <- contrast_data$mean - mean(contrast_data$mean)
  total_ss <- sum(scores_centered^2)

  rows <- vector("list", length(terms))
  for (term_id in seq_along(terms)) {
    columns <- which(assign == term_id)
    term_model <- model[, columns, drop = FALSE]
    qr_model <- qr(term_model)
    rank <- qr_model$rank
    q <- qr.Q(qr_model)[, seq_len(rank), drop = FALSE]
    coefficients <- as.numeric(crossprod(q, scores_centered))
    magnitude <- sqrt(sum(coefficients^2))

    rows[[term_id]] <- data.frame(
      effect = terms[[term_id]],
      df = rank,
      signed_projection = if (rank == 1L) coefficients[[1L]] else NA_real_,
      magnitude = magnitude,
      proportion = if (total_ss > 0) magnitude^2 / total_ss else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

#' Plot Design Scores as a Heatmap
#'
#' @description
#' Plot summarized LV design scores after rehydrating flattened condition labels
#' with a condition key.
#'
#' @param x A `pls_result` object.
#' @param lv Latent variable to plot.
#' @param condition_key Optional condition metadata.
#' @param row,column,facet Column names used for the heatmap layout.
#' @param show_values Logical; print rounded scores inside tiles.
#' @param limits Optional fill limits. Defaults to symmetric limits around zero.
#' @param title Optional plot title.
#' @param block Optional score block to keep for multiblock results.
#'
#' @return A ggplot object.
#' @export
plot_design_heatmap <- function(x,
                                lv = 1L,
                                condition_key = NULL,
                                row = NULL,
                                column = NULL,
                                facet = "group",
                                show_values = TRUE,
                                limits = NULL,
                                title = NULL,
                                block = NULL) {
  df <- as_design_scores(
    x,
    lv = lv,
    condition_key = condition_key,
    summarize = TRUE,
    block = block
  )
  layout <- .design_layout_columns(df, condition_key, row, column, facet)
  if (is.null(limits)) {
    limits <- .symmetric_limits(df$mean)
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[layout$column]], y = .data[[layout$row]], fill = mean)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::scale_fill_gradient2(
      low = pls_colors(1, "diverging"),
      mid = "#F7F7F7",
      high = pls_colors(1, "diverging", reverse = TRUE),
      midpoint = 0,
      limits = limits,
      name = "Score"
    ) +
    ggplot2::labs(
      title = title %||% sprintf("Design Score Heatmap (LV%d)", lv),
      x = tools::toTitleCase(layout$column),
      y = tools::toTitleCase(layout$row)
    ) +
    theme_pls()

  row_count <- length(unique(df[[layout$row]]))
  if (row_count > 1L) {
    p <- p + ggplot2::geom_hline(
      yintercept = seq(1.5, row_count - 0.5, by = 1),
      color = "white",
      linewidth = 0.4
    )
  }

  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", mean)),
      size = 3.4,
      color = "#222222"
    )
  }

  if (!is.null(layout$facet)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", layout$facet)))
  }

  p
}

#' Plot Design Score Interactions
#'
#' @description
#' Plot LV design-score means as an interaction plot. This is useful when a
#' latent variable combines group, task, or condition effects.
#'
#' @param x A `pls_result` object.
#' @param lv Latent variable to plot.
#' @param condition_key Optional condition metadata.
#' @param x_axis,trace,facet Column names used for the interaction layout.
#' @param show_se Logical; include standard-error bars.
#' @param title Optional plot title.
#' @param block Optional score block to keep for multiblock results.
#'
#' @return A ggplot object.
#' @export
plot_design_interaction <- function(x,
                                    lv = 1L,
                                    condition_key = NULL,
                                    x_axis = NULL,
                                    trace = NULL,
                                    facet = "group",
                                    show_se = TRUE,
                                    title = NULL,
                                    block = NULL) {
  df <- as_design_scores(
    x,
    lv = lv,
    condition_key = condition_key,
    summarize = TRUE,
    block = block
  )
  layout <- .design_layout_columns(df, condition_key, row = trace, column = x_axis, facet = facet)
  trace_col <- layout$row
  x_col <- layout$column
  dodge <- ggplot2::position_dodge(width = 0.18)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data[[x_col]],
      y = mean,
      color = .data[[trace_col]],
      group = .data[[trace_col]]
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, color = "#555555", linewidth = 0.35) +
    ggplot2::geom_line(position = dodge, linewidth = 0.9) +
    ggplot2::geom_point(position = dodge, size = 2.4) +
    ggplot2::labs(
      title = title %||% sprintf("Design Score Interaction (LV%d)", lv),
      x = tools::toTitleCase(x_col),
      y = "Mean design score",
      color = tools::toTitleCase(trace_col)
    ) +
    theme_pls() +
    scale_color_pls_discrete()

  if (isTRUE(show_se)) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - se, ymax = mean + se),
      width = 0.08,
      position = dodge,
      linewidth = 0.35
    )
  }

  if (!is.null(layout$facet)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", layout$facet)))
  }

  p
}

#' Plot Factorial Design Contrast Magnitudes
#'
#' @description
#' Plot the effect-coded contrast decomposition returned by
#' `as_design_contrasts()`. Single-degree-of-freedom effects are signed; larger
#' multi-degree-of-freedom effects are shown as positive magnitudes.
#'
#' @param x A `pls_result` object.
#' @param lv Latent variable to plot.
#' @param condition_key Optional condition metadata.
#' @param factors Character vector of factor columns to include.
#' @param sort Logical; order effects by absolute contribution.
#' @param title Optional plot title.
#' @param block Optional score block to keep for multiblock results.
#'
#' @return A ggplot object.
#' @export
plot_design_contrasts <- function(x,
                                  lv = 1L,
                                  condition_key = NULL,
                                  factors = NULL,
                                  sort = TRUE,
                                  title = NULL,
                                  block = NULL) {
  df <- as_design_contrasts(
    x,
    lv = lv,
    condition_key = condition_key,
    factors = factors,
    block = block
  )

  df$plot_value <- ifelse(df$df == 1L, df$signed_projection, df$magnitude)
  df$direction <- ifelse(df$plot_value < 0, "negative", "positive")
  if (isTRUE(sort)) {
    df$effect <- factor(df$effect, levels = df$effect[order(df$magnitude)])
  }

  ggplot2::ggplot(df, ggplot2::aes(x = effect, y = plot_value, fill = direction)) +
    ggplot2::geom_hline(yintercept = 0, color = "#555555", linewidth = 0.35) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title %||% sprintf("Design Contrast Decomposition (LV%d)", lv),
      x = NULL,
      y = "Projection magnitude",
      fill = "Direction"
    ) +
    theme_pls() +
    ggplot2::scale_fill_manual(
      values = c(negative = pls_colors(1, "diverging"), positive = pls_colors(1, "diverging", reverse = TRUE))
    )
}

.filter_score_block <- function(df, block = NULL) {
  if (!"block" %in% names(df)) {
    return(df)
  }

  df$block <- as.character(df$block)
  if (is.null(block) && "task" %in% df$block) {
    block <- "task"
  }
  if (is.null(block)) {
    return(df)
  }

  keep <- df$block == block
  if (!any(keep)) {
    stop("No design-score rows found for block '", block, "'.", call. = FALSE)
  }
  df[keep, , drop = FALSE]
}

.join_condition_key <- function(df, condition_key = NULL) {
  group_levels <- unique(as.character(df$group))
  condition_levels <- unique(as.character(df$condition))
  df$group <- factor(as.character(df$group), levels = group_levels)
  df$condition <- factor(as.character(df$condition), levels = condition_levels)

  if (is.null(condition_key)) {
    return(df)
  }
  if (!is.data.frame(condition_key) || !"condition" %in% names(condition_key)) {
    stop("condition_key must be a data frame with a 'condition' column.", call. = FALSE)
  }

  key <- condition_key
  key$condition <- as.character(key$condition)
  if (anyDuplicated(key$condition)) {
    stop("condition_key$condition must contain unique condition labels.", call. = FALSE)
  }

  key_index <- match(as.character(df$condition), key$condition)
  if (anyNA(key_index)) {
    missing <- unique(df$condition[is.na(key_index)])
    stop(
      "condition_key is missing condition labels: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  for (nm in setdiff(names(key), "condition")) {
    values <- key[[nm]][key_index]
    if (is.factor(key[[nm]])) {
      df[[nm]] <- factor(values, levels = levels(key[[nm]]))
    } else {
      df[[nm]] <- factor(values, levels = unique(key[[nm]]))
    }
  }

  df
}

.summarize_design_scores <- function(df) {
  grouping <- setdiff(names(df), c("score", "subject"))
  formula <- stats::as.formula(paste("score ~", paste(grouping, collapse = " + ")))
  out <- stats::aggregate(
    formula,
    data = df,
    FUN = function(z) c(mean = mean(z), sd = stats::sd(z), n = length(z))
  )
  out$mean <- out$score[, "mean"]
  out$sd <- out$score[, "sd"]
  out$n <- as.integer(out$score[, "n"])
  out$se <- out$sd / sqrt(out$n)
  out$se[!is.finite(out$se)] <- 0
  out$score <- NULL
  out
}

.design_layout_columns <- function(df,
                                   condition_key = NULL,
                                   row = NULL,
                                   column = NULL,
                                   facet = "group") {
  key_columns <- if (is.null(condition_key)) character(0) else {
    setdiff(names(condition_key), "condition")
  }

  if (is.null(column)) {
    column <- if (length(key_columns) >= 1L) key_columns[[1L]] else "condition"
  }
  if (is.null(row)) {
    row <- if (length(key_columns) >= 2L) key_columns[[2L]] else "group"
  }
  if (!is.null(facet) && !facet %in% names(df)) {
    facet <- NULL
  }

  needed <- c(row, column, facet)
  missing <- setdiff(needed[!is.na(needed)], names(df))
  if (length(missing)) {
    stop("Unknown design-score column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  list(row = row, column = column, facet = facet)
}

.symmetric_limits <- function(x) {
  limit <- max(abs(x), na.rm = TRUE)
  if (!is.finite(limit) || limit <= 0) {
    limit <- 1
  }
  c(-limit, limit)
}
