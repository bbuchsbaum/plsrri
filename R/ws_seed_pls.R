#' Within-Subject Seed PLS (ws-fcMRI)
#'
#' @description
#' Functions for within-subject seed-based PLS analysis, implementing the
#' ws-seed PLS method described in Roberts et al. (2016). Unlike standard
#' seed PLS (as-fcMRI), which correlates seed and voxel activity *across*
#' subjects, ws-seed PLS correlates seed and voxel trial-level beta estimates
#' *within* each subject, then submits the resulting connectivity maps to
#' task PLS.
#'
#' @details
#' The approach requires trial-level activation estimates (e.g., from
#' \code{fmrilss::lss()}) rather than condition means. For each subject and
#' condition, Pearson correlations are computed between the seed region's
#' trial-by-trial betas and every voxel's trial-by-trial betas. The resulting
#' within-subject correlation maps (optionally Fisher-z transformed) are
#' stacked into a data matrix and analyzed with task PLS.
#'
#' @references
#' Roberts, R. P., Hach, S., Tippett, L. J., & Addis, D. R. (2016).
#' The Simpson's paradox and fMRI: Similarities and differences between
#' functional connectivity measures derived from within-subject and
#' across-subject correlations. *NeuroImage*.
#' \doi{10.1016/j.neuroimage.2016.04.028}
#'
#' @name ws-seed-pls
NULL


#' Compute Within-Subject Seed-Voxel Correlation Maps
#'
#' @description
#' For each subject and condition, computes Pearson correlations between a
#' seed region's trial-level betas and every voxel's trial-level betas.
#' Returns a stacked matrix suitable for task PLS.
#'
#' @param beta_lst A list of matrices (one per subject). Each matrix has
#'   rows = trials, columns = voxels. All matrices must have the same number
#'   of columns (voxels). Trial counts may vary across subjects.
#' @param seed_lst A list of numeric vectors or single-column matrices (one
#'   per subject). Each element contains the seed region's trial-level betas,
#'   with length equal to the number of trials (rows) for that subject. If a
#'   matrix with multiple columns is provided, each column is treated as a
#'   separate seed and correlations are computed independently for each.
#' @param condition_lst A list of factors or integer vectors (one per subject).
#'   Each element maps trials to conditions (same length as rows in the
#'   corresponding beta matrix). Condition labels must be consistent across
#'   subjects.
#' @param fisher_z Logical; if \code{TRUE} (default), apply Fisher's r-to-z
#'   transform to the within-subject correlations before stacking.
#' @param min_trials Integer; minimum number of trials per condition required
#'   to compute a valid correlation (default 3). Conditions with fewer trials
#'   produce \code{NA} values.
#'
#' @return A list with components:
#'   \describe{
#'     \item{datamat}{A matrix with rows = subjects x conditions (stacked in
#'       condition-within-subject order, i.e., all conditions for subject 1,
#'       then all conditions for subject 2, etc.) and columns = voxels
#'       (x n_seeds if multiple seeds). For multiple seeds, columns are
#'       ordered as all voxels for seed 1, then all voxels for seed 2, etc.}
#'     \item{num_cond}{Integer; number of conditions.}
#'     \item{cond_labels}{Character vector of condition labels.}
#'     \item{n_subjects}{Integer; number of subjects.}
#'     \item{n_seeds}{Integer; number of seed regions.}
#'     \item{n_voxels}{Integer; number of voxels per seed.}
#'   }
#'
#' @details
#' The stacking order places conditions within subjects (matching the
#' convention used by \code{pls_analysis()}):
#' \preformatted{
#'   row 1:  subject 1, condition 1
#'   row 2:  subject 1, condition 2
#'   ...
#'   row k:  subject 1, condition k
#'   row k+1: subject 2, condition 1
#'   ...
#' }
#'
#' @export
#'
#' @examples
#' # Simulate trial-level data for 10 subjects, 2 conditions, 50 voxels
#' set.seed(42)
#' n_subj <- 10; n_trials <- 24; n_vox <- 50; n_cond <- 2
#' beta_lst <- lapply(seq_len(n_subj), function(i) matrix(rnorm(n_trials * n_vox), n_trials, n_vox))
#' seed_lst <- lapply(seq_len(n_subj), function(i) rnorm(n_trials))
#' cond_lst <- lapply(seq_len(n_subj), function(i) rep(1:n_cond, each = n_trials / n_cond))
#'
#' ws <- ws_seed_correlation(beta_lst, seed_lst, cond_lst)
#' dim(ws$datamat)  # 20 x 50
ws_seed_correlation <- function(beta_lst,
                                seed_lst,
                                condition_lst,
                                fisher_z = TRUE,
                                min_trials = 3L) {
  prep <- .prepare_ws_seed_inputs(
    beta_lst = beta_lst,
    seed_lst = seed_lst,
    condition_lst = condition_lst,
    min_trials = min_trials,
    allow_missing_conditions = FALSE
  )
  corr_rows <- .compute_ws_seed_subject_rows(prep, fisher_z = fisher_z)
  out_mat <- matrix(NA_real_, nrow = prep$n_subjects * prep$n_cond, ncol = prep$n_voxels * prep$n_seeds)
  for (i in seq_len(prep$n_subjects)) {
    for (ci in seq_len(prep$n_cond)) {
      row_out <- (i - 1L) * prep$n_cond + ci
      out_mat[row_out, ] <- corr_rows[[i]][[prep$cond_labels[ci]]]
    }
  }

  list(
    datamat = out_mat,
    num_cond = prep$n_cond,
    cond_labels = prep$cond_labels,
    n_subjects = prep$n_subjects,
    n_seeds = prep$n_seeds,
    n_voxels = prep$n_voxels
  )
}


#' Within-Subject Seed PLS Analysis
#'
#' @description
#' Convenience function for within-subject seed-based PLS (ws-seed PLS).
#' Computes within-subject seed-voxel correlations from trial-level beta
#' estimates, then runs task PLS on the resulting connectivity maps.
#'
#' This implements the ws-seed PLS method from Roberts et al. (2016), which
#' captures *temporal* co-fluctuation between seed and target regions within
#' each individual, avoiding the Simpson's Paradox that can arise with
#' standard across-subject seed PLS.
#'
#' @param beta_lst A list of matrices (one per subject). Each matrix has
#'   rows = trials, columns = voxels. Typically produced by
#'   \code{fmrilss::lss()}.
#' @param seed_lst A list of numeric vectors or matrices (one per subject).
#'   Each contains the seed region's trial-level betas. If a matrix, each
#'   column is a separate seed.
#' @param condition_lst A list of factors or integer vectors (one per subject)
#'   mapping trials to experimental conditions.
#' @param num_subj_lst Integer vector with number of subjects per group.
#'   For a single group, just the total number of subjects. For multiple
#'   groups, a vector (e.g., \code{c(12, 14)}). For SSB designs, supply a
#'   list of integer vectors giving subject counts per condition within group.
#' @param groups Optional subject-level group assignments for trial inputs.
#'   Required when \code{num_subj_lst} uses SSB/list form because subject-group
#'   membership cannot be recovered from per-condition counts alone.
#' @param fisher_z Logical; apply Fisher r-to-z transform (default \code{TRUE}).
#' @param min_trials Minimum trials per condition for valid correlation
#'   (default 3).
#' @param nonrotated Logical; if \code{TRUE}, use non-rotated task PLS
#'   (method 2). Default \code{FALSE} uses standard rotated task PLS
#'   (method 1).
#' @param meancentering_type Mean-centering for task PLS:
#'   \describe{
#'     \item{0}{Within-group centering (default)}
#'     \item{1}{Grand condition mean removal}
#'     \item{2}{Grand mean removal}
#'     \item{3}{All main effects removal}
#'   }
#' @param nperm Number of permutations (default 1000).
#' @param nboot Number of bootstrap samples (default 500).
#' @param ... Additional arguments passed to \code{pls_analysis()}.
#'
#' @return A \code{pls_result} object (class \code{pls_task} or
#'   \code{pls_task_nonrot}). The brain saliences (singular vectors)
#'   represent voxel patterns of within-subject connectivity with the seed.
#'   Bootstrap ratios index reliability of each voxel's connectivity.
#'
#' @details
#' The analysis proceeds in two stages:
#'
#' \enumerate{
#'   \item \strong{Within-subject correlation}: For each subject and condition,
#'     Pearson correlations are computed between the seed's trial-level betas
#'     and every voxel's trial-level betas. This produces one connectivity
#'     map per subject per condition.
#'   \item \strong{Task PLS}: The connectivity maps are stacked and submitted
#'     to task PLS, which identifies latent variables capturing
#'     condition-dependent connectivity patterns that are reliable across
#'     subjects.
#' }
#'
#' Trial-level betas are typically estimated using Least Squares Separate
#' (LSS) via \code{fmrilss::lss()}, which provides stable per-trial
#' activation estimates even with rapid event-related designs.
#'
#' @seealso \code{\link{seed_pls}} for standard across-subject seed PLS,
#'   \code{\link{ws_seed_correlation}} for the correlation computation step,
#'   \code{\link{pls_analysis}} for the underlying PLS engine.
#'
#' @references
#' Roberts, R. P., Hach, S., Tippett, L. J., & Addis, D. R. (2016).
#' The Simpson's paradox and fMRI: Similarities and differences between
#' functional connectivity measures derived from within-subject and
#' across-subject correlations. *NeuroImage*.
#' \doi{10.1016/j.neuroimage.2016.04.028}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Estimate trial-level betas with fmrilss
#' betas <- fmrilss::lss(Y, X, Nuisance = nuisance_mat)
#'
#' # 2. Extract seed betas (e.g., mean of voxels in PCC ROI)
#' seed_betas <- rowMeans(betas[, pcc_voxel_indices])
#'
#' # 3. Run ws-seed PLS
#' result <- ws_seed_pls(
#'   beta_lst = list(betas_subj1, betas_subj2, ...),
#'   seed_lst = list(seed_subj1, seed_subj2, ...),
#'   condition_lst = list(cond_subj1, cond_subj2, ...),
#'   num_subj_lst = 16
#' )
#'
#' # 4. Inspect results
#' significance(result)
#' plot_scores(result, lv = 1)
#' }
ws_seed_pls <- function(beta_lst,
                        seed_lst,
                        condition_lst,
                        num_subj_lst,
                        groups = NULL,
                        fisher_z = TRUE,
                        min_trials = 3L,
                        nonrotated = FALSE,
                        meancentering_type = 0L,
                        nperm = 1000,
                        nboot = 500,
                        nsplit = 0,
                        clim = 95,
                        boot_type = "strat",
                        is_struct = FALSE,
                        progress = TRUE,
                        stacked_designdata = NULL,
                        ...) {
  spec <- pls_spec() |>
    add_trial_data(
      beta_lst = beta_lst,
      seed_lst = seed_lst,
      condition_lst = condition_lst,
      groups = groups,
      fisher_z = fisher_z,
      min_trials = min_trials
    ) |>
    configure(
      method = if (nonrotated) "ws_seed_nonrotated" else "ws_seed",
      nperm = nperm,
      nboot = nboot,
      nsplit = nsplit,
      clim = clim,
      meancentering = meancentering_type,
      boot_type = boot_type,
      is_struct = is_struct
    )

  spec$num_subj_lst <- if (is.list(num_subj_lst)) {
    lapply(num_subj_lst, as.integer)
  } else {
    as.integer(num_subj_lst)
  }

  if (!is.null(stacked_designdata)) {
    spec$stacked_designdata <- stacked_designdata
  }

  run(spec, progress = progress, ...)
}


# --- Internal helpers ---

.prepare_ws_seed_inputs <- function(beta_lst,
                                    seed_lst,
                                    condition_lst,
                                    min_trials = 3L,
                                    expected_conditions = NULL,
                                    allow_missing_conditions = FALSE) {
  n_subj <- length(beta_lst)
  if (n_subj == 0L) {
    stop("beta_lst must contain at least one subject")
  }
  if (length(seed_lst) != n_subj) {
    stop("seed_lst must have the same length as beta_lst (one per subject)")
  }
  if (length(condition_lst) != n_subj) {
    stop("condition_lst must have the same length as beta_lst (one per subject)")
  }

  min_trials <- as.integer(min_trials)
  if (is.na(min_trials) || min_trials < 2L) {
    stop("min_trials must be >= 2 (need at least 2 observations for correlation)")
  }

  beta_lst <- lapply(seq_len(n_subj), function(i) {
    beta_i <- beta_lst[[i]]
    if (!is.matrix(beta_i)) {
      stop(sprintf("beta_lst[[%d]] must be a matrix", i))
    }
    if (!is.numeric(beta_i)) {
      storage.mode(beta_i) <- "double"
    }
    beta_i
  })

  n_vox <- ncol(beta_lst[[1]])
  for (i in seq_len(n_subj)) {
    if (ncol(beta_lst[[i]]) != n_vox) {
      stop(sprintf(
        "beta_lst[[%d]] has %d columns but expected %d (all subjects must have same voxel count)",
        i, ncol(beta_lst[[i]]), n_vox
      ))
    }
  }

  seed_lst <- lapply(seq_len(n_subj), function(i) {
    seed_i <- seed_lst[[i]]
    if (is.vector(seed_i) || is.factor(seed_i)) {
      seed_i <- matrix(as.numeric(seed_i), ncol = 1)
    }
    if (!is.matrix(seed_i)) {
      seed_i <- as.matrix(seed_i)
    }
    if (!is.numeric(seed_i)) {
      storage.mode(seed_i) <- "double"
    }
    if (nrow(seed_i) != nrow(beta_lst[[i]])) {
      stop(sprintf(
        "seed_lst[[%d]] has %d rows but beta_lst[[%d]] has %d rows (must match trial count)",
        i, nrow(seed_i), i, nrow(beta_lst[[i]])
      ))
    }
    seed_i
  })

  n_seeds <- ncol(seed_lst[[1]])
  for (i in seq_len(n_subj)) {
    if (ncol(seed_lst[[i]]) != n_seeds) {
      stop(sprintf(
        "seed_lst[[%d]] has %d seed columns but expected %d (must be consistent across subjects)",
        i, ncol(seed_lst[[i]]), n_seeds
      ))
    }
  }

  cond_info <- .normalize_ws_conditions(
    condition_lst = condition_lst,
    beta_lst = beta_lst,
    expected_conditions = expected_conditions,
    allow_missing_conditions = allow_missing_conditions
  )

  list(
    beta_lst = beta_lst,
    seed_lst = seed_lst,
    condition_lst = cond_info$condition_lst,
    cond_labels = cond_info$cond_labels,
    n_subjects = n_subj,
    n_cond = length(cond_info$cond_labels),
    n_voxels = n_vox,
    n_seeds = n_seeds,
    min_trials = min_trials
  )
}

.normalize_ws_conditions <- function(condition_lst,
                                     beta_lst,
                                     expected_conditions = NULL,
                                     allow_missing_conditions = FALSE) {
  n_subj <- length(condition_lst)
  cond_chr <- vector("list", n_subj)

  for (i in seq_len(n_subj)) {
    cond_i <- condition_lst[[i]]
    if (length(cond_i) != nrow(beta_lst[[i]])) {
      stop(sprintf(
        "condition_lst[[%d]] has length %d but beta_lst[[%d]] has %d rows",
        i, length(cond_i), i, nrow(beta_lst[[i]])
      ))
    }
    cond_chr[[i]] <- as.character(cond_i)
  }

  raw_levels <- character(0)
  for (i in seq_len(n_subj)) {
    raw_levels <- c(raw_levels, setdiff(unique(cond_chr[[i]]), raw_levels))
  }

  cond_labels <- if (is.null(expected_conditions)) raw_levels else as.character(expected_conditions)
  reference_levels <- if (is.null(expected_conditions)) cond_labels else raw_levels

  if (!is.null(expected_conditions) && length(cond_labels) != length(raw_levels)) {
    stop("expected_conditions must have the same length as the condition set implied by condition_lst")
  }

  if (allow_missing_conditions) {
    for (i in seq_len(n_subj)) {
      current_levels <- unique(cond_chr[[i]])
      if (!all(current_levels %in% reference_levels)) {
        stop("condition_lst contains labels not present in the expected condition set")
      }
    }
  } else {
    for (i in seq_len(n_subj)) {
      current_levels <- unique(cond_chr[[i]])
      if (!identical(sort(current_levels), sort(reference_levels))) {
        stop(
          "All subjects must have the same set of condition labels for within-subject seed PLS"
        )
      }
    }
  }

  list(
    condition_lst = lapply(
      cond_chr,
      function(x) factor(x, levels = reference_levels, labels = cond_labels, ordered = TRUE)
    ),
    cond_labels = cond_labels
  )
}

.compute_ws_seed_subject_rows <- function(prep, fisher_z = TRUE) {
  n_subj <- prep$n_subjects
  n_cond <- prep$n_cond
  n_vox <- prep$n_voxels
  n_seeds <- prep$n_seeds
  cond_labels <- prep$cond_labels
  total_cols <- n_vox * n_seeds

  out <- vector("list", n_subj)
  for (i in seq_len(n_subj)) {
    betas_i <- prep$beta_lst[[i]]
    seeds_i <- prep$seed_lst[[i]]
    conds_i <- prep$condition_lst[[i]]
    subj_rows <- setNames(vector("list", n_cond), cond_labels)

    for (ci in seq_len(n_cond)) {
      cond_label <- cond_labels[ci]
      trial_idx <- which(conds_i == cond_label)
      if (length(trial_idx) == 0L) {
        next
      }

      row_vec <- rep(NA_real_, total_cols)
      if (length(trial_idx) >= prep$min_trials) {
        vox_block <- betas_i[trial_idx, , drop = FALSE]
        for (si in seq_len(n_seeds)) {
          seed_vec <- seeds_i[trial_idx, si]
          col_start <- (si - 1L) * n_vox + 1L
          col_end <- si * n_vox
          r_vals <- .ws_cor_vec(seed_vec, vox_block)
          if (fisher_z) {
            r_vals <- .fisher_z(r_vals)
          }
          row_vec[col_start:col_end] <- r_vals
        }
      }
      subj_rows[[cond_label]] <- row_vec
    }
    out[[i]] <- subj_rows
  }
  out
}

#' Correlate a vector with each column of a matrix
#'
#' Fast Pearson correlation of a single vector (seed) with each column
#' (voxel) of a matrix across their shared observations. Zero-variance
#' inputs produce 0.
#'
#' @param x Numeric vector (n observations).
#' @param Y Numeric matrix (n x p).
#' @return Numeric vector of length p.
#' @keywords internal
.ws_cor_vec <- function(x, Y) {
  n <- length(x)
  mx <- mean(x)
  x_c <- x - mx

  my <- colMeans(Y)
  Y_c <- sweep(Y, 2L, my, "-")

  ss_x <- sum(x_c^2)
  if (ss_x < .Machine$double.eps) {
    return(rep(0, ncol(Y)))
  }

  ss_y <- colSums(Y_c^2)
  denom <- sqrt(ss_x * ss_y)

  # Guard against zero-variance voxels
  denom[denom < .Machine$double.eps] <- Inf

  as.numeric(crossprod(x_c, Y_c)) / denom
}


#' Fisher r-to-z Transform
#'
#' @param r Numeric vector of correlation values.
#' @return Fisher z-transformed values. Values of exactly +/-1 are clamped
#'   to +/- (1 - 1e-7) to avoid infinities.
#' @keywords internal
.fisher_z <- function(r) {
  # Clamp to avoid Inf
  r <- pmin(pmax(r, -(1 - 1e-7)), 1 - 1e-7)
  0.5 * log((1 + r) / (1 - r))
}
