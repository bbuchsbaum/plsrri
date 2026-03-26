#' Score Plots for PLS
#'
#' @description
#' Functions for visualizing PLS scores (brain scores, design scores).
#'
#' @name plot-scores
NULL

#' Plot PLS Scores
#'
#' @description
#' Creates scatter plots, bar plots, or trajectory plots of PLS scores.
#'
#' @param x A `pls_result` object
#' @param lv Latent variable to plot (default 1)
#' @param type Score type: "brain" or "design"
#' @param plot_type Plot style: "scatter", "bar", "violin", "box"
#' @param group_by Grouping variable for colors/facets
#' @param color_by Variable for color mapping
#' @param facet_by Variable for faceting
#' @param show_ci Show confidence intervals if available
#' @param title Plot title
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- quick_pls(list(d1, d2), c(20, 18), 3)
#' plot_scores(result, lv = 1, type = "design", plot_type = "bar")
#' }
plot_scores <- function(x,
                         lv = 1,
                         type = "design",
                         plot_type = "bar",
                         group_by = NULL,
                         color_by = NULL,
                         facet_by = NULL,
                         show_ci = TRUE,
                         title = NULL) {

  UseMethod("plot_scores")
}

#' @export
plot_scores.pls_result <- function(x,
                                    lv = 1,
                                    type = "design",
                                    plot_type = "bar",
                                    group_by = NULL,
                                    color_by = NULL,
                                    facet_by = NULL,
                                    show_ci = TRUE,
                                    title = NULL) {

  type <- match.arg(type, c("brain", "design", "behavior"))
  plot_type <- match.arg(plot_type, c("scatter", "bar", "violin", "box", "line"))

  # Get scores and construct data frame
  sc <- scores(x, type = type, lv = lv)

  # Build metadata
  metadata <- .build_score_metadata(x, type)
  df <- data.frame(score = sc, metadata)

  # Default groupings
  if (is.null(group_by) && "condition" %in% names(df)) {
    group_by <- "condition"
  }
  if (is.null(color_by) && "group" %in% names(df)) {
    color_by <- "group"
  }

  # Create appropriate plot
  p <- switch(plot_type,
    bar = .plot_scores_bar(df, group_by, color_by, facet_by),
    scatter = .plot_scores_scatter(df, group_by, color_by, facet_by),
    violin = .plot_scores_violin(df, group_by, color_by, facet_by),
    box = .plot_scores_box(df, group_by, color_by, facet_by),
    line = .plot_scores_line(df, group_by, color_by, facet_by)
  )

  # Add title
  if (is.null(title)) {
    title <- sprintf("%s Scores (LV%d)", tools::toTitleCase(type), lv)
  }

  p + ggplot2::labs(title = title, y = "Score") +
    theme_pls() +
    scale_fill_pls_discrete() +
    scale_color_pls_discrete()
}

#' Build Score Metadata
#'
#' @keywords internal
.build_score_metadata <- function(x, type) {
  num_subj_lst <- x$num_subj_lst
  num_cond <- x$num_cond
  num_groups <- length(num_subj_lst)
  total_rows <- sum(num_subj_lst) * num_cond

  # Build group and condition labels
  groups <- x$groups %||% paste0("Group", seq_len(num_groups))
  conditions <- x$conditions %||% paste0("Cond", seq_len(num_cond))

  build_block_metadata <- function(cond_ids, block_label) {
    block_conditions <- conditions[cond_ids]
    block_rows <- sum(num_subj_lst) * length(cond_ids)

    group_vec <- character(block_rows)
    cond_vec <- character(block_rows)
    subj_vec <- integer(block_rows)
    block_vec <- character(block_rows)

    idx <- 1L
    for (g in seq_len(num_groups)) {
      n_subj <- num_subj_lst[g]
      subj_offset <- sum(num_subj_lst[seq_len(g - 1L)])
      for (cond_id in cond_ids) {
        for (s in seq_len(n_subj)) {
          group_vec[idx] <- groups[g]
          cond_vec[idx] <- conditions[cond_id]
          subj_vec[idx] <- subj_offset + s
          block_vec[idx] <- block_label
          idx <- idx + 1L
        }
      }
    }

    data.frame(
      group = factor(group_vec, levels = groups),
      condition = factor(cond_vec, levels = conditions),
      subject = subj_vec,
      block = factor(block_vec, levels = c("task", "behavior"))
    )
  }

  # Multiblock scores stack task rows followed by behavior-block rows.
  if (!is.null(x$bscan) && !is.null(x$TBusc) && !is.null(x$TBvsc)) {
    task_meta <- build_block_metadata(seq_len(num_cond), "task")
    behavior_meta <- build_block_metadata(as.integer(x$bscan), "behavior")
    return(rbind(task_meta, behavior_meta))
  }

  build_block_metadata(seq_len(num_cond), "task")[, c("group", "condition", "subject")]
}

#' Score Bar Plot
#'
#' @keywords internal
.plot_scores_bar <- function(df, group_by, color_by, facet_by) {

  # Compute means and SEs
  if (!is.null(group_by)) {
    if (!is.null(color_by) && color_by != group_by) {
      agg <- aggregate(score ~ get(group_by) + get(color_by), df, function(x) {
        c(mean = mean(x), se = sd(x)/sqrt(length(x)))
      })
      names(agg)[1:2] <- c(group_by, color_by)
    } else {
      agg <- aggregate(score ~ get(group_by), df, function(x) {
        c(mean = mean(x), se = sd(x)/sqrt(length(x)))
      })
      names(agg)[1] <- group_by
    }
    agg$mean <- agg$score[, "mean"]
    agg$se <- agg$score[, "se"]
    agg$score <- NULL
  } else {
    agg <- data.frame(
      mean = mean(df$score),
      se = sd(df$score)/sqrt(nrow(df))
    )
  }

  # Create plot
  if (!is.null(group_by)) {
    if (!is.null(color_by) && color_by != group_by) {
      p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data[[group_by]],
                                              y = mean,
                                              fill = .data[[color_by]])) +
        ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.7) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                               position = ggplot2::position_dodge(0.8),
                               width = 0.2)
    } else {
      p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data[[group_by]], y = mean)) +
        ggplot2::geom_col(width = 0.7, fill = pls_colors(1, "qualitative")) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                               width = 0.2)
    }
    p <- p + ggplot2::labs(x = tools::toTitleCase(group_by))
  } else {
    p <- ggplot2::ggplot(agg, ggplot2::aes(x = 1, y = mean)) +
      ggplot2::geom_col(width = 0.5) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                             width = 0.1)
  }

  # Add faceting
  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }

  p
}

#' Score Scatter Plot
#'
#' @keywords internal
.plot_scores_scatter <- function(df, group_by, color_by, facet_by) {

  p <- ggplot2::ggplot(df, ggplot2::aes(x = seq_len(nrow(df)), y = score))

  if (!is.null(color_by)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[color_by]]), alpha = 0.7)
  } else {
    p <- p + ggplot2::geom_point(alpha = 0.7, color = pls_colors(1, "qualitative"))
  }

  p <- p + ggplot2::labs(x = "Observation")

  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }

  p
}

#' Score Violin Plot
#'
#' @keywords internal
.plot_scores_violin <- function(df, group_by, color_by, facet_by) {

  if (is.null(group_by)) {
    group_by <- "condition"
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[group_by]], y = score))

  if (!is.null(color_by)) {
    p <- p + ggplot2::geom_violin(ggplot2::aes(fill = .data[[color_by]]), alpha = 0.7)
  } else {
    p <- p + ggplot2::geom_violin(fill = pls_colors(1, "qualitative"), alpha = 0.7)
  }

  p <- p + ggplot2::labs(x = tools::toTitleCase(group_by))

  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }

  p
}

#' Score Box Plot
#'
#' @keywords internal
.plot_scores_box <- function(df, group_by, color_by, facet_by) {

  if (is.null(group_by)) {
    group_by <- "condition"
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[group_by]], y = score))

  if (!is.null(color_by)) {
    p <- p + ggplot2::geom_boxplot(ggplot2::aes(fill = .data[[color_by]]), alpha = 0.7)
  } else {
    p <- p + ggplot2::geom_boxplot(fill = pls_colors(1, "qualitative"), alpha = 0.7)
  }

  p <- p + ggplot2::labs(x = tools::toTitleCase(group_by))

  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }

  p
}

#' Score Line Plot
#'
#' @keywords internal
.plot_scores_line <- function(df, group_by, color_by, facet_by) {

  if (is.null(group_by)) {
    group_by <- "condition"
  }

  # Aggregate by group and condition
  if (!is.null(color_by)) {
    agg <- aggregate(score ~ get(group_by) + get(color_by), df, mean)
    names(agg)[1:2] <- c(group_by, color_by)
    p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data[[group_by]], y = score,
                                            color = .data[[color_by]],
                                            group = .data[[color_by]])) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3)
  } else {
    agg <- aggregate(score ~ get(group_by), df, mean)
    names(agg)[1] <- group_by
    p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data[[group_by]], y = score, group = 1)) +
      ggplot2::geom_line(linewidth = 1, color = pls_colors(1, "qualitative")) +
      ggplot2::geom_point(size = 3, color = pls_colors(1, "qualitative"))
  }

  p <- p + ggplot2::labs(x = tools::toTitleCase(group_by))

  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }

  p
}
