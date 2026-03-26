#' Loading Plots for PLS
#'
#' @description
#' Functions for visualizing PLS loadings (design loadings, behavior loadings).
#'
#' @name plot-loadings
NULL

#' Plot PLS Loadings
#'
#' @description
#' Creates bar plots or dot plots of PLS loadings.
#'
#' @param x A `pls_result` object
#' @param lv Latent variable to plot (default 1)
#' @param type Loading type: "design" or "behavior"
#' @param plot_type Plot style: "bar" or "dot"
#' @param show_ci Show confidence intervals if available
#' @param threshold Minimum |loading| to show (for decluttering)
#' @param sort Sort loadings by value
#' @param title Plot title
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- quick_pls(list(d1, d2), c(20, 18), 3)
#' plot_loadings(result, lv = 1, type = "design")
#' }
plot_loadings <- function(x,
                           lv = 1,
                           type = "design",
                           plot_type = "bar",
                           show_ci = TRUE,
                           threshold = NULL,
                           sort = TRUE,
                           title = NULL) {

  UseMethod("plot_loadings")
}

#' @export
plot_loadings.pls_result <- function(x,
                                      lv = 1,
                                      type = "design",
                                      plot_type = "bar",
                                      show_ci = TRUE,
                                      threshold = NULL,
                                      sort = TRUE,
                                      title = NULL) {

  type <- match.arg(type, c("design", "behavior"))
  plot_type <- match.arg(plot_type, c("bar", "dot"))

  # Get loadings
  v <- loadings(x, type = type, lv = lv)

  # Build labels
  labels <- .build_loading_labels(x, type)

  df <- data.frame(
    label = labels,
    loading = v,
    stringsAsFactors = FALSE
  )

  # Apply threshold
  if (!is.null(threshold)) {
    df <- df[abs(df$loading) >= threshold, ]
  }

  # Sort
  if (sort) {
    df <- df[order(df$loading, decreasing = TRUE), ]
    df$label <- factor(df$label, levels = df$label)
  }

  # Color by sign
  df$sign <- ifelse(df$loading >= 0, "positive", "negative")

  # Create plot
  if (plot_type == "bar") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = label, y = loading, fill = sign)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, color = "gray30") +
      ggplot2::coord_flip()
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = loading, y = label, color = sign)) +
      ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, color = "gray30") +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = loading,
                                          y = label, yend = label),
                            linewidth = 0.8)
  }

  # Colors
  p <- p + ggplot2::scale_fill_manual(
    values = c(positive = "#2166AC", negative = "#D6604D"),
    guide = "none"
  ) +
  ggplot2::scale_color_manual(
    values = c(positive = "#2166AC", negative = "#D6604D"),
    guide = "none"
  )

  # Title
  if (is.null(title)) {
    title <- sprintf("%s Loadings (LV%d)", tools::toTitleCase(type), lv)
  }

  p + ggplot2::labs(title = title, x = "", y = "Loading") +
    theme_pls()
}

#' Build Loading Labels
#'
#' @keywords internal
.build_loading_labels <- function(x, type) {
  num_groups <- length(x$num_subj_lst)
  num_cond <- x$num_cond

  groups <- x$groups %||% paste0("G", seq_len(num_groups))
  conditions <- x$conditions %||% paste0("C", seq_len(num_cond))

  if (type == "design") {
    # Design loadings: group x condition
    labels <- character(num_groups * num_cond)
    idx <- 1
    for (g in seq_len(num_groups)) {
      for (c in seq_len(num_cond)) {
        labels[idx] <- paste(groups[g], conditions[c], sep = ":")
        idx <- idx + 1
      }
    }
    return(labels)
  }

  if (type == "behavior" && !is.null(x$stacked_behavdata)) {
    # Behavior loadings: group x condition x measure
    measures <- colnames(x$stacked_behavdata)
    if (is.null(measures)) {
      measures <- paste0("M", seq_len(ncol(x$stacked_behavdata)))
    }

    # Multiblock stores behavior loadings only for the selected behavior-block
    # conditions. Regular behavior/seed PLS uses all conditions.
    if (!is.null(x$bscan)) {
      cond_ids <- as.integer(x$bscan)
      conditions <- conditions[cond_ids]
    }

    n_measures <- length(measures)
    labels <- character(num_groups * length(conditions) * n_measures)

    idx <- 1
    for (g in seq_len(num_groups)) {
      for (c in seq_along(conditions)) {
        for (m in seq_len(n_measures)) {
          labels[idx] <- paste(groups[g], conditions[c], measures[m], sep = ":")
          idx <- idx + 1
        }
      }
    }
    return(labels)
  }

  # Fallback
  paste0("V", seq_len(nrow(loadings(x, type = type))))
}

#' Plot Singular Values
#'
#' @description
#' Creates a bar plot of singular values (variance explained).
#'
#' @param x A `pls_result` object
#' @param show_pvalue Show p-values from permutation test
#' @param alpha Significance threshold for coloring
#' @param cumulative Show cumulative variance explained
#' @param title Plot title
#'
#' @return A ggplot object
#' @export
plot_singular_values <- function(x,
                                  show_pvalue = TRUE,
                                  alpha = 0.05,
                                  cumulative = FALSE,
                                  title = NULL) {

  UseMethod("plot_singular_values")
}

#' @export
plot_singular_values.pls_result <- function(x,
                                             show_pvalue = TRUE,
                                             alpha = 0.05,
                                             cumulative = FALSE,
                                             title = NULL) {

  s <- x$s
  var_exp <- (s^2) / sum(s^2) * 100
  n_lv <- length(s)

  df <- data.frame(
    lv = factor(seq_len(n_lv)),
    singular_value = s,
    var_explained = var_exp,
    cum_var = cumsum(var_exp)
  )

  # Add significance
  if (!is.null(x$perm_result)) {
    df$pvalue <- x$perm_result$sprob
    df$significant <- df$pvalue < alpha
  } else {
    df$significant <- TRUE
  }

  # Choose y variable
  y_var <- if (cumulative) "cum_var" else "var_explained"
  y_label <- if (cumulative) "Cumulative Variance (%)" else "Variance Explained (%)"

  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = lv, y = .data[[y_var]], fill = significant)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = "#4E79A7", `FALSE` = "#CCCCCC"),
      labels = c(`TRUE` = paste("p <", alpha), `FALSE` = paste("p >=", alpha)),
      name = "Significance"
    )

  # Add p-value labels
  if (show_pvalue && !is.null(x$perm_result)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("p=%.3f", pvalue)),
      vjust = -0.5, size = 3
    )
  }

  # Title
  if (is.null(title)) {
    title <- "Singular Values"
  }

  p + ggplot2::labs(title = title, x = "Latent Variable", y = y_label) +
    theme_pls()
}

#' Plot S3 Method for pls_result
#'
#' @description
#' Generic plot method for PLS results.
#'
#' @param x A `pls_result` object
#' @param what What to plot: "singular_values", "scores", "loadings", "brain"
#' @param ... Additional arguments passed to specific plot functions
#'
#' @return A ggplot object
#' @export
plot.pls_result <- function(x, what = "singular_values", ...) {
  what <- match.arg(what, c("singular_values", "scores", "loadings", "brain", "summary"))

  switch(what,
    singular_values = plot_singular_values(x, ...),
    scores = plot_scores(x, ...),
    loadings = plot_loadings(x, ...),
    brain = plot_brain(x, ...),
    summary = plot_pls_summary(x, ...)
  )
}

#' Plot PLS Summary
#'
#' @description
#' Creates a multi-panel summary plot of PLS results.
#'
#' @param x A `pls_result` object
#' @param lv Latent variable for detailed plots
#'
#' @return A patchwork object
#' @export
plot_pls_summary <- function(x, lv = 1) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for summary plots")
  }

  # Create component plots
  p1 <- plot_singular_values(x, title = "A. Singular Values")
  p2 <- plot_scores(x, lv = lv, type = "design", plot_type = "bar",
                    title = sprintf("B. Design Scores (LV%d)", lv))
  p3 <- plot_loadings(x, lv = lv, title = sprintf("C. Loadings (LV%d)", lv))

  # Combine
  (p1 | p2) / p3 +
    patchwork::plot_annotation(
      title = sprintf("PLS Analysis Summary - %s", PLS_METHOD_NAMES[x$method]),
      theme = theme_pls()
    )
}
