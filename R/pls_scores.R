#' Brain and Behavior Score Computation
#'
#' @description
#' Functions for computing brain scores, behavior scores, and related measures.
#'
#' @name pls-scores
NULL

#' Get Behavior Scores (MATLAB parity)
#'
#' @description
#' Port of MATLAB `rri_get_behavscores.m`. Computes brain scores (`usc`),
#' behavior/design scores (`vsc`), and LV correlations (`lvcorrs`) for
#' behavior and multiblock PLS.
#'
#' @param stacked_datamat Stacked brain data matrix (n_obs x n_voxels)
#' @param stacked_behavdata Stacked behavior data matrix (n_obs x n_behav)
#' @param brainlv Brain latent vectors (n_voxels x n_lv)
#' @param behavlv Behavior/design latent vectors (n_behav*num_cond*num_groups x n_lv)
#' @param num_cond Number of conditions
#' @param num_subj_lst Integer vector with subjects per group
#' @param cormode Correlation mode (0, 2, 4, 6)
#'
#' @return List with `usc`, `vsc`, `lvcorrs`
#' @keywords internal
pls_get_behavscores <- function(stacked_datamat,
                                stacked_behavdata,
                                brainlv,
                                behavlv,
                                num_cond,
                                num_subj_lst,
                                cormode = 0L) {
  assert_that(is.matrix(stacked_datamat))
  assert_that(is.matrix(stacked_behavdata))
  assert_that(is.matrix(brainlv))
  assert_that(is.matrix(behavlv))
  assert_that(nrow(stacked_datamat) == nrow(stacked_behavdata))
  assert_that(ncol(stacked_datamat) == nrow(brainlv))
  assert_that(is.count(num_cond))

  is_ssb <- is.list(num_subj_lst)
  if (!is_ssb) {
    assert_that(is.numeric(num_subj_lst))
  } else {
    assert_that(is.list(num_subj_lst), length(num_subj_lst) >= 1)
  }

  num_groups <- length(num_subj_lst)
  n_behav <- ncol(stacked_behavdata)
  n_lv <- ncol(brainlv)
  k <- as.integer(num_cond)

  # Brain scores
  usc <- stacked_datamat %*% brainlv

  # Behavior/design scores
  vsc <- matrix(0, nrow = nrow(stacked_behavdata), ncol = n_lv)

  # LV correlations (behavior loadings)
  lvcorrs <- matrix(0, nrow = num_groups * k * n_behav, ncol = n_lv)

  for (g in seq_len(num_groups)) {
    if (!is_ssb) {
      n <- as.integer(num_subj_lst[g])
      span <- sum(num_subj_lst[seq_len(g - 1)]) * k
      group_n_rows <- n * k
    } else {
      n <- as.integer(num_subj_lst[[g]])
      if (length(n) != k) {
        stop("For ssb designs, each num_subj_lst[[g]] must have length num_cond")
      }
      span <- if (g == 1L) 0L else sum(vapply(num_subj_lst[seq_len(g - 1)], function(x) sum(as.integer(x)), integer(1)))
      group_n_rows <- sum(n)
    }

    # vsc: project behav data onto behavlv by condition within group
    if (!is_ssb) {
      for (cond in seq_len(k)) {
        row_start <- span + 1 + n * (cond - 1)
        row_end <- span + n * cond

        behav_cond <- stacked_behavdata[row_start:row_end, , drop = FALSE]

        lv_row_start <- (g - 1) * (k * n_behav) + (cond - 1) * n_behav + 1
        lv_row_end <- lv_row_start + n_behav - 1

        behavlv_cond <- behavlv[lv_row_start:lv_row_end, , drop = FALSE]

        vsc[row_start:row_end, ] <- behav_cond %*% behavlv_cond
      }
    } else {
      step <- 0L
      for (cond in seq_len(k)) {
        row_start <- span + 1 + step
        row_end <- span + step + n[cond]

        behav_cond <- stacked_behavdata[row_start:row_end, , drop = FALSE]

        lv_row_start <- (g - 1) * (k * n_behav) + (cond - 1) * n_behav + 1
        lv_row_end <- lv_row_start + n_behav - 1
        behavlv_cond <- behavlv[lv_row_start:lv_row_end, , drop = FALSE]

        vsc[row_start:row_end, ] <- behav_cond %*% behavlv_cond
        step <- step + n[cond]
      }
    }

    # lvcorrs: correlation maps between behavior and brain scores
    group_rows <- (span + 1):(span + group_n_rows)
    lvcorrs_group <- pls_corr_maps(
      behavdata = stacked_behavdata[group_rows, , drop = FALSE],
      datamat = usc[group_rows, , drop = FALSE],
      n_subj = n,
      num_cond = k,
      cormode = cormode
    )

    corr_row_start <- (g - 1) * (k * n_behav) + 1
    corr_row_end <- corr_row_start + (k * n_behav) - 1
    lvcorrs[corr_row_start:corr_row_end, ] <- lvcorrs_group
  }

  list(usc = usc, vsc = vsc, lvcorrs = lvcorrs)
}

#' Compute Brain Scores
#'
#' @description
#' Computes brain scores by projecting the data matrix onto the saliences.
#' Brain scores indicate how strongly each observation expresses each latent
#' variable pattern.
#'
#' @param datamat Data matrix (n_obs x n_voxels)
#' @param salience Salience matrix (n_voxels x n_lv)
#'
#' @return Brain scores matrix (n_obs x n_lv)
#' @export
pls_brain_scores <- function(datamat, salience) {
  assert_that(is.matrix(datamat))
  assert_that(is.matrix(salience))
  assert_that(ncol(datamat) == nrow(salience))

  datamat %*% salience
}

#' Compute Design Scores
#'
#' @description
#' Computes design scores by projecting the design matrix onto the design
#' loadings (v from SVD).
#'
#' @param design Design matrix
#' @param v Design loadings from SVD (v matrix)
#'
#' @return Design scores matrix
#' @export
pls_design_scores <- function(design, v) {
  assert_that(is.matrix(design))
  assert_that(is.matrix(v))

  design %*% v
}

#' Get Behavior Scores
#'
#' @description
#' Computes behavior scores for behavior and multiblock PLS.
#' These represent how strongly each behavior measure relates to each LV.
#'
#' @param stacked_behavdata Stacked behavior data
#' @param lvcorrs Latent variable correlations
#' @param num_groups Number of groups
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param n_lv Number of latent variables
#'
#' @return Behavior scores matrix
#' @export
pls_behav_scores <- function(stacked_behavdata, lvcorrs, num_groups,
                              num_subj_lst, num_cond, n_lv) {
  assert_that(is.matrix(stacked_behavdata))
  assert_that(is.matrix(lvcorrs))

  n_behav <- ncol(stacked_behavdata)
  k <- num_cond

  # Initialize behavior scores
  behavscores <- matrix(0, nrow = nrow(stacked_behavdata), ncol = n_lv)

  for (g in seq_len(num_groups)) {
    n <- num_subj_lst[g]
    span <- sum(num_subj_lst[seq_len(g - 1)]) * k

    for (c in seq_len(num_cond)) {
      row_start <- span + 1 + n * (c - 1)
      row_end <- span + n * c

      behav_cond <- stacked_behavdata[row_start:row_end, , drop = FALSE]

      # Get corresponding lvcorrs rows
      lv_row_start <- (g - 1) * (k * n_behav) + (c - 1) * n_behav + 1
      lv_row_end <- lv_row_start + n_behav - 1

      lvcorrs_cond <- lvcorrs[lv_row_start:lv_row_end, , drop = FALSE]

      # Center behavior data
      behav_centered <- scale(behav_cond, center = TRUE, scale = FALSE)

      # Compute scores
      behavscores[row_start:row_end, ] <- behav_centered %*% lvcorrs_cond
    }
  }

  behavscores
}

#' Compute LV Correlations
#'
#' @description
#' Computes correlations between behavior data and brain scores.
#' These are the behavior loadings for behavior PLS.
#'
#' @param stacked_behavdata Behavior data matrix
#' @param brain_scores Brain scores matrix
#' @param num_groups Number of groups
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param cormode Correlation mode
#'
#' @return LV correlations matrix
#' @export
pls_lv_corrs <- function(stacked_behavdata, brain_scores, num_groups,
                          num_subj_lst, num_cond, cormode = 0L) {
  assert_that(is.matrix(stacked_behavdata))
  assert_that(is.matrix(brain_scores))

  n_behav <- ncol(stacked_behavdata)
  n_lv <- ncol(brain_scores)
  k <- num_cond

  lvcorrs <- matrix(0, nrow = num_groups * k * n_behav, ncol = n_lv)

  for (g in seq_len(num_groups)) {
    n <- num_subj_lst[g]
    span <- sum(num_subj_lst[seq_len(g - 1)]) * k

    for (c in seq_len(num_cond)) {
      row_start <- span + 1 + n * (c - 1)
      row_end <- span + n * c

      behav_cond <- stacked_behavdata[row_start:row_end, , drop = FALSE]
      scores_cond <- brain_scores[row_start:row_end, , drop = FALSE]

      # Compute correlation
      corr_cond <- pls_xcor(behav_cond, scores_cond, cormode)

      # Store
      lv_row_start <- (g - 1) * (k * n_behav) + (c - 1) * n_behav + 1
      lv_row_end <- lv_row_start + n_behav - 1
      lvcorrs[lv_row_start:lv_row_end, ] <- corr_cond
    }
  }

  lvcorrs
}

#' Extract Task/Behavior Scores Separately (Multiblock)
#'
#' @description
#' For multiblock PLS, extracts the task block and behavior block components
#' of the design loadings (v) and scores separately.
#'
#' @param v Design loadings from SVD
#' @param num_groups Number of groups
#' @param num_cond Number of conditions
#' @param n_behav Number of behavior measures
#' @param bscan Conditions used in behavior block
#'
#' @return List with task_v, behav_v components
#' @export
pls_split_multiblock_v <- function(v, num_groups, num_cond, n_behav, bscan) {
  assert_that(is.matrix(v))

  n_lv <- ncol(v)
  n_bscan <- length(bscan)
  k <- as.integer(num_cond)
  group_block <- k + n_bscan * n_behav
  task_rows <- num_groups * k
  behav_rows <- num_groups * n_bscan * n_behav

  # In multiblock PLS, v rows are stacked by group, and within each group:
  # [k task rows] then [n_bscan*n_behav behavior rows]. This matches MATLAB.
  task_v <- matrix(0, nrow = task_rows, ncol = n_lv)
  behav_v <- matrix(0, nrow = behav_rows, ncol = n_lv)

  tv_i <- 1L
  bv_i <- 1L
  for (g in seq_len(num_groups)) {
    start <- (g - 1L) * group_block

    tv_rows <- (start + 1L):(start + k)
    bv_rows <- (start + k + 1L):(start + group_block)

    task_v[tv_i:(tv_i + k - 1L), ] <- v[tv_rows, , drop = FALSE]
    behav_v[bv_i:(bv_i + length(bv_rows) - 1L), ] <- v[bv_rows, , drop = FALSE]

    tv_i <- tv_i + k
    bv_i <- bv_i + length(bv_rows)
  }

  list(
    task_v = task_v,
    behav_v = behav_v
  )
}
