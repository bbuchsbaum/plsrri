#' Site-Pooling Diagnostics for Multisite PLS
#'
#' @description
#' Optional post-fit diagnostics for pooled multisite analyses. When
#' observation- or subject-level site labels are available, these summaries
#' quantify how stable a pooled solution is across sites.
#'
#' @param x A fitted \code{pls_result}.
#' @param site Optional site labels. May have length equal to the number of
#'   observations or the number of subjects. If omitted, \code{x$site} is used.
#' @param spec Optional \code{pls_spec} used to fit \code{x}. This is required
#'   for site-specific and leave-one-site-out reruns when \code{x} does not
#'   already carry enough input context.
#' @param progress Logical; emit informational messages during reruns.
#'
#' @return A named list containing pooled observation metadata, sitewise score
#'   summaries, sitewise score correlations, site-specific salience similarity,
#'   and leave-one-site-out rerun summaries.
#' @export
site_pooling_diagnostics <- function(x, site = NULL, spec = NULL, progress = FALSE) {
  if (inherits(x, "mva_result")) {
    x <- mva_result_to_pls_result(x)
  }
  if (!inherits(x, "pls_result")) {
    stop("x must be a pls_result or mva_result", call. = FALSE)
  }

  if (is.null(site)) {
    site <- x$site %||% spec$site %||% NULL
  }
  if (is.null(site)) {
    stop("site labels are required", call. = FALSE)
  }

  obs_meta <- .plsrri_observation_metadata(
    num_subj_lst = x$num_subj_lst,
    num_cond = x$num_cond,
    groups = x$groups,
    conditions = x$conditions
  )
  site_obs <- .plsrri_expand_site_labels(
    site = site,
    obs_meta = obs_meta
  )

  brain_scores <- scores(x, type = "brain")
  design_scores <- tryCatch(scores(x, type = "design"), error = function(e) NULL)
  lvs <- seq_len(ncol(brain_scores))

  score_rows <- lapply(lvs, function(lv) {
    data.frame(
      row = obs_meta$row,
      group = obs_meta$group,
      condition = obs_meta$condition,
      subject = obs_meta$subject,
      site = site_obs,
      lv = lv,
      brain_score = brain_scores[, lv],
      design_score = if (is.null(design_scores)) NA_real_ else design_scores[, lv],
      stringsAsFactors = FALSE
    )
  })
  score_df <- do.call(rbind, score_rows)

  site_score_summary <- do.call(rbind, lapply(split(score_df, list(score_df$site, score_df$lv), drop = TRUE), function(df) {
    data.frame(
      site = df$site[[1]],
      lv = df$lv[[1]],
      n_obs = nrow(df),
      brain_mean = mean(df$brain_score, na.rm = TRUE),
      brain_sd = stats::sd(df$brain_score, na.rm = TRUE),
      design_mean = mean(df$design_score, na.rm = TRUE),
      design_sd = stats::sd(df$design_score, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  rownames(site_score_summary) <- NULL

  site_score_correlations <- do.call(rbind, lapply(split(score_df, list(score_df$site, score_df$lv), drop = TRUE), function(df) {
    corr <- if (all(is.na(df$design_score)) ||
                length(unique(df$brain_score[is.finite(df$brain_score)])) < 2L ||
                length(unique(df$design_score[is.finite(df$design_score)])) < 2L) {
      NA_real_
    } else {
      stats::cor(df$brain_score, df$design_score, use = "pairwise.complete.obs")
    }
    data.frame(
      site = df$site[[1]],
      lv = df$lv[[1]],
      n_obs = nrow(df),
      correlation = corr,
      stringsAsFactors = FALSE
    )
  }))
  rownames(site_score_correlations) <- NULL

  site_fit_similarity <- NULL
  leave_one_site_out <- NULL
  if (!is.null(spec) && inherits(spec, "pls_spec") && !is.list(spec$num_subj_lst) &&
      identical(x$method %in% c(3L, 5L), TRUE)) {
    site_fit_similarity <- .plsrri_site_specific_similarity(
      result = x,
      spec = spec,
      site_obs = site_obs,
      progress = progress
    )
    leave_one_site_out <- .plsrri_leave_one_site_out(
      result = x,
      spec = spec,
      site_obs = site_obs,
      progress = progress
    )
  }

  structure(
    list(
      sites = sort(unique(site_obs)),
      observation_scores = score_df,
      site_score_summary = site_score_summary,
      site_score_correlations = site_score_correlations,
      site_fit_similarity = site_fit_similarity,
      leave_one_site_out = leave_one_site_out
    ),
    class = "plsrri_site_diagnostics"
  )
}

.plsrri_observation_metadata <- function(num_subj_lst, num_cond, groups = NULL, conditions = NULL) {
  if (is.list(num_subj_lst)) {
    stop("Site diagnostics reruns currently require balanced designs", call. = FALSE)
  }

  num_groups <- length(num_subj_lst)
  groups <- as.character(groups %||% paste0("Group", seq_len(num_groups)))
  conditions <- as.character(conditions %||% paste0("Cond", seq_len(num_cond)))

  out <- vector("list", length = sum(as.integer(num_subj_lst)) * as.integer(num_cond))
  idx <- 1L
  row_idx <- 1L
  subject_offset <- 0L

  for (g in seq_len(num_groups)) {
    n_subj <- as.integer(num_subj_lst[g])
    for (cond in seq_len(as.integer(num_cond))) {
      for (s in seq_len(n_subj)) {
        out[[idx]] <- data.frame(
          row = row_idx,
          group = groups[[g]],
          group_index = g,
          condition = conditions[[cond]],
          condition_index = cond,
          subject = subject_offset + s,
          subject_in_group = s,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
        row_idx <- row_idx + 1L
      }
    }
    subject_offset <- subject_offset + n_subj
  }

  do.call(rbind, out)
}

.plsrri_expand_site_labels <- function(site, obs_meta) {
  site <- as.character(site)
  n_obs <- nrow(obs_meta)
  n_subjects <- length(unique(obs_meta$subject))

  if (length(site) == n_obs) {
    site_by_subject <- tapply(site, obs_meta$subject, function(x) unique(x))
    if (any(lengths(site_by_subject) != 1L)) {
      stop("Observation-level site labels must be constant within subject", call. = FALSE)
    }
    return(site)
  }

  if (length(site) == n_subjects) {
    return(site[obs_meta$subject])
  }

  stop(sprintf(
    "site must have length %d (observations) or %d (subjects)",
    n_obs, n_subjects
  ), call. = FALSE)
}

.plsrri_cosine_cols <- function(x, y) {
  stopifnot(is.matrix(x), is.matrix(y), ncol(x) == ncol(y))
  out <- numeric(ncol(x))
  for (j in seq_len(ncol(x))) {
    xj <- x[, j]
    yj <- y[, j]
    denom <- sqrt(sum(xj^2)) * sqrt(sum(yj^2))
    out[[j]] <- if (denom <= 0) NA_real_ else sum(xj * yj) / denom
  }
  out
}

.plsrri_align_to_reference <- function(x, ref) {
  stopifnot(is.matrix(x), is.matrix(ref), ncol(x) == ncol(ref))
  out <- x
  for (j in seq_len(ncol(x))) {
    if (sum(out[, j] * ref[, j]) < 0) {
      out[, j] <- -out[, j]
    }
  }
  out
}

.plsrri_subset_spec_by_site <- function(spec, site_obs, keep_sites) {
  obs_meta <- .plsrri_observation_metadata(
    num_subj_lst = spec$num_subj_lst,
    num_cond = spec$num_cond,
    groups = spec$groups,
    conditions = spec$conditions
  )

  keep_subject <- tapply(site_obs %in% keep_sites, obs_meta$subject, all)
  subject_keep_ids <- as.integer(names(keep_subject)[keep_subject])
  keep_rows <- obs_meta$subject %in% subject_keep_ids

  group_levels <- unique(obs_meta$group)
  datamat_stacked <- stack_datamats(spec$datamat_lst)
  datamat_lst <- vector("list", 0L)
  num_subj_new <- integer(0L)
  groups_new <- character(0L)

  for (g in group_levels) {
    rows_g <- obs_meta$group == g & keep_rows
    subj_g <- unique(obs_meta$subject_in_group[obs_meta$group == g & keep_rows])
    if (!length(subj_g)) next
    datamat_lst[[length(datamat_lst) + 1L]] <- datamat_stacked[which(rows_g), , drop = FALSE]
    num_subj_new <- c(num_subj_new, length(subj_g))
    groups_new <- c(groups_new, g)
  }

  out <- spec
  out$datamat_lst <- datamat_lst
  out$num_subj_lst <- num_subj_new
  out$groups <- groups_new
  if (!is.null(spec$stacked_behavdata)) {
    out$stacked_behavdata <- spec$stacked_behavdata[keep_rows, , drop = FALSE]
  }
  if (!is.null(spec$stacked_designdata)) {
    out$stacked_designdata <- spec$stacked_designdata[keep_rows, , drop = FALSE]
  }
  out$site <- site_obs[keep_rows]
  out$num_perm <- 0L
  out$num_boot <- 0L
  out$num_split <- 0L
  out$.skip_site_diagnostics <- TRUE
  out
}

.plsrri_site_specific_similarity <- function(result, spec, site_obs, progress = FALSE) {
  sites <- sort(unique(site_obs))
  rows <- lapply(sites, function(site_name) {
    site_spec <- .plsrri_subset_spec_by_site(spec, site_obs, keep_sites = site_name)
    fit <- run(site_spec, progress = progress)
    fit_u <- .plsrri_align_to_reference(fit$u, result$u)
    fit_v <- .plsrri_align_to_reference(fit$v, result$v)

    data.frame(
      site = site_name,
      lv = seq_len(ncol(result$u)),
      feature_cosine = .plsrri_cosine_cols(fit_u, result$u),
      design_cosine = .plsrri_cosine_cols(fit_v, result$v),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.plsrri_leave_one_site_out <- function(result, spec, site_obs, progress = FALSE) {
  sites <- sort(unique(site_obs))
  rows <- lapply(sites, function(site_name) {
    train_spec <- .plsrri_subset_spec_by_site(spec, site_obs, keep_sites = setdiff(sites, site_name))
    test_spec <- .plsrri_subset_spec_by_site(spec, site_obs, keep_sites = site_name)
    fit <- run(train_spec, progress = progress)
    fit_u <- .plsrri_align_to_reference(fit$u, result$u)
    fit_v <- .plsrri_align_to_reference(fit$v, result$v)

    heldout_corr <- rep(NA_real_, ncol(result$u))
    proj <- tryCatch(project_scores(fit, test_spec, type = "both"), error = function(e) NULL)
    if (is.list(proj) && !is.null(proj$feature) && !is.null(proj$design)) {
      heldout_corr <- vapply(seq_len(ncol(proj$feature)), function(j) {
        xj <- proj$feature[, j]
        yj <- proj$design[, j]
        if (length(unique(xj[is.finite(xj)])) < 2L || length(unique(yj[is.finite(yj)])) < 2L) {
          return(NA_real_)
        }
        stats::cor(xj, yj, use = "pairwise.complete.obs")
      }, numeric(1))
    }

    data.frame(
      held_out_site = site_name,
      lv = seq_len(ncol(result$u)),
      feature_cosine = .plsrri_cosine_cols(fit_u, result$u),
      design_cosine = .plsrri_cosine_cols(fit_v, result$v),
      heldout_score_correlation = heldout_corr,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
