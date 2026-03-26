#' Builder Helpers: Observation Ordering
#'
#' @description
#' Shared helpers for ordering manifest-derived observations in a deterministic
#' condition-major, subject-minor layout while preserving SSB support.
#'
#' @name builder-observation-order
NULL

.order_manifest_observations <- function(obs_df, groups, conditions) {
  assert_that(is.data.frame(obs_df))
  groups <- as.character(groups)
  conditions <- as.character(conditions)

  obs_df$group <- factor(obs_df$group, levels = groups)
  obs_df$condition <- factor(obs_df$condition, levels = conditions)

  group_obs <- lapply(groups, function(g) {
    obs_df[obs_df$group == g, , drop = FALSE]
  })
  names(group_obs) <- groups

  ordered_group_obs <- list()
  num_subj_lst <- vector("list", length(groups))
  names(num_subj_lst) <- groups

  for (g in groups) {
    gd <- group_obs[[g]]
    subj_sets <- lapply(conditions, function(cn) unique(gd$subject[gd$condition == cn]))
    counts <- vapply(subj_sets, length, integer(1))
    balanced <- length(unique(counts)) == 1L &&
      length(subj_sets) > 0 &&
      all(vapply(subj_sets, function(s) setequal(s, subj_sets[[1]]), logical(1)))

    if (balanced) {
      subj_order <- sort(subj_sets[[1]])
      num_subj_lst[[g]] <- as.integer(length(subj_order))
      idx_all <- integer(0)
      for (cn in conditions) {
        rows <- gd[gd$condition == cn, , drop = FALSE]
        ord <- match(subj_order, rows$subject)
        if (anyNA(ord)) stop("Missing subjects within condition; cannot form balanced design", call. = FALSE)
        idx_all <- c(idx_all, which(gd$condition == cn)[ord])
      }
      ordered_group_obs[[g]] <- gd[idx_all, , drop = FALSE]
    } else {
      num_subj_lst[[g]] <- as.integer(counts)
      idx_all <- integer(0)
      for (cn in conditions) {
        rows <- which(gd$condition == cn)
        ord <- order(gd$subject[rows])
        idx_all <- c(idx_all, rows[ord])
      }
      ordered_group_obs[[g]] <- gd[idx_all, , drop = FALSE]
    }
  }

  is_ssb <- any(vapply(num_subj_lst, function(x) length(x) != 1L, logical(1)))
  num_subj_out <- if (!is_ssb) {
    as.integer(vapply(num_subj_lst, `[[`, integer(1), 1L))
  } else {
    lapply(num_subj_lst, as.integer)
  }

  list(
    obs_by_group = ordered_group_obs,
    num_subj_lst = num_subj_out
  )
}
