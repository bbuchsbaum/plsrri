#' Split-Half Validation for PLS
#'
#' @description
#' Functions for split-half cross-validation to assess reliability of PLS results.
#' Based on Natasha's split-half routine.
#'
#' @name pls-splithalf
NULL

#' Compute Split-Half Row Selectors
#'
#' @description
#' Internal helper implementing the MATLAB `reshape(...); tmp(:)` logic used to
#' pick the first and second half of subjects within each group.
#'
#' @param num_subj_lst Integer vector of subjects per group
#' @param num_cond Integer number of conditions
#'
#' @return List with `num_subj_lst1`, `num_subj_lst2`, `rows1`, `rows2`.
#' @keywords internal
.pls_splithalf_rows <- function(num_subj_lst, num_cond) {
  assert_that(is.numeric(num_subj_lst))
  assert_that(is.count(num_cond))

  num_subj_lst <- as.integer(num_subj_lst)
  k <- as.integer(num_cond)

  num_subj_lst1 <- round(num_subj_lst / 2)
  num_subj_lst2 <- num_subj_lst - num_subj_lst1

  rows1 <- integer(0)
  rows2 <- integer(0)

  for (g in seq_along(num_subj_lst)) {
    n <- num_subj_lst[g]
    n1 <- num_subj_lst1[g]
    n2 <- num_subj_lst2[g]

    tmp <- matrix(seq_len(n * k), nrow = n, ncol = k) # MATLAB reshape is column-major
    tmp1 <- as.integer(as.vector(tmp[seq_len(n1), , drop = FALSE]))
    tmp2 <- as.integer(as.vector(tmp[n1 + seq_len(n2), , drop = FALSE]))

    offset <- sum(num_subj_lst[seq_len(g - 1)]) * k
    rows1 <- c(rows1, offset + tmp1)
    rows2 <- c(rows2, offset + tmp2)
  }

  list(
    num_subj_lst1 = num_subj_lst1,
    num_subj_lst2 = num_subj_lst2,
    rows1 = rows1,
    rows2 = rows2
  )
}

#' Build Inner Subject Reorder Vector
#'
#' @description
#' Internal helper mirroring the MATLAB logic:
#' `offset + n*(cond-1) + gperm` for each group and condition.
#'
#' @param num_subj_lst Integer vector of subjects per group
#' @param num_cond Integer number of conditions
#' @param subject_perm_by_group List of integer permutations, one per group.
#'
#' @return Integer vector of length `sum(num_subj_lst) * num_cond`.
#' @keywords internal
.pls_splithalf_in_reorder <- function(num_subj_lst, num_cond, subject_perm_by_group) {
  assert_that(is.numeric(num_subj_lst))
  assert_that(is.count(num_cond))
  assert_that(is.list(subject_perm_by_group))

  num_subj_lst <- as.integer(num_subj_lst)
  k <- as.integer(num_cond)
  num_groups <- length(num_subj_lst)

  if (length(subject_perm_by_group) != num_groups) {
    stop("subject_perm_by_group must have one permutation per group")
  }

  in_reorder <- integer(0)
  for (g in seq_len(num_groups)) {
    n <- num_subj_lst[g]
    gperm <- as.integer(subject_perm_by_group[[g]])
    if (length(gperm) != n || !identical(sort(gperm), seq_len(n))) {
      stop(sprintf("Group %d subject permutation must be a permutation of 1:%d", g, n))
    }
    offset <- sum(num_subj_lst[seq_len(g - 1)]) * k
    for (cond in seq_len(k)) {
      in_reorder <- c(in_reorder, offset + n * (cond - 1L) + gperm)
    }
  }

  in_reorder
}

#' Get Covariance for Subset of Rows
#'
#' @description
#' Helper function to compute covariance/correlation matrices for a subset of
#' observations. This is mainly used by resampling routines.
#'
#' @keywords internal
pls_get_covcor_subset <- function(stacked_datamat,
                                  stacked_behavdata,
                                  num_groups,
                                  num_subj_lst,
                                  num_cond,
                                  method,
                                  row_idx,
                                  bscan,
                                  meancentering_type,
                                  cormode) {
  assert_that(is.matrix(stacked_datamat))
  if (!is.null(stacked_behavdata)) {
    assert_that(is.matrix(stacked_behavdata))
  }

  row_idx <- as.integer(row_idx)
  dat_sub <- stacked_datamat[row_idx, , drop = FALSE]
  beh_sub <- if (!is.null(stacked_behavdata)) stacked_behavdata[row_idx, , drop = FALSE] else NULL

  total_rows <- nrow(dat_sub)

  pls_get_covcor(
    method = method,
    stacked_datamat = dat_sub,
    stacked_behavdata = beh_sub,
    num_groups = num_groups,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    bscan = bscan,
    meancentering_type = meancentering_type,
    cormode = cormode,
    datamat_reorder = seq_len(total_rows),
    behavdata_reorder = if (method %in% c(3L, 4L, 5L, 6L)) seq_len(total_rows) else NULL,
    datamat_reorder_4beh = if (method %in% c(4L, 6L)) seq_len(total_rows) else NULL
  )
}

#' Run Split-Half Validation
#'
#' @description
#' Performs split-half cross-validation where the sample is randomly split,
#' PLS is run on each half, and the correlation between the resulting
#' weights (u) and loadings (v) is computed.
#'
#' @param stacked_datamat Stacked data matrix
#' @param stacked_behavdata Behavior data matrix (methods 3-6)
#' @param stacked_designdata Design contrast matrix (methods 2, 5, 6)
#' @param num_groups Number of groups
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param method PLS method (1-6)
#' @param num_split Number of split-half iterations
#' @param num_outer_perm Number of outer permutations for significance
#'   (includes the unpermuted reference sample as the first permutation,
#'   MATLAB `missnk_rri_perm_order` style).
#' @param clim Confidence level used for optional CI reporting (percent).
#' @param bscan Conditions for behavior block
#' @param meancentering_type Mean-centering type
#' @param cormode Correlation mode
#' @param is_struct Structure PLS flag (do not permute conditions within-subject)
#' @param outer_reorder Optional matrix of outer permutation orders
#'   (`nrow(stacked_datamat)` x `num_outer_perm`). If provided, overrides the
#'   internally generated outer permutation orders (MATLAB `permsamp` style).
#' @param inner_subject_perms Optional nested list specifying the within-group
#'   subject permutations for each outer permutation and split. If provided,
#'   it must have shape `[[op]][[p]][[g]]` where each entry is a permutation of
#'   `1:num_subj_lst[g]`. This enables deterministic split-half runs for testing.
#' @param progress Show progress
#'
#' @return pls_splithalf_result object
#' @export
pls_splithalf_test <- function(stacked_datamat,
                                stacked_behavdata = NULL,
                                stacked_designdata = NULL,
                                num_groups,
                                num_subj_lst,
                                num_cond,
                                method,
                                num_split,
                                num_outer_perm = 0L,
                                clim = 95,
                                bscan = NULL,
                                meancentering_type = 0L,
                                cormode = 0L,
                                is_struct = FALSE,
                                outer_reorder = NULL,
                                inner_subject_perms = NULL,
                                progress = TRUE) {

  if (is.null(bscan)) {
    bscan <- seq_len(num_cond)
  }

  # MATLAB behavior: requesting split-half (num_split > 0) automatically
  # enables Natasha's non-rotated bootstrap mode.
  nonrotated_boot <- isTRUE(num_split > 0L)

  if (num_outer_perm <= 0L) {
    # MATLAB split-half is defined inside the permutation loop. If the user
    # doesn't request any outer permutations, fall back to the unpermuted
    # reference only (no p-values / CI from the null distribution).
    num_outer_perm <- 1L
  }

  total_rows <- nrow(stacked_datamat)
  k <- as.integer(num_cond)

  split_rows <- .pls_splithalf_rows(num_subj_lst, k)
  num_subj_lst1 <- split_rows$num_subj_lst1
  num_subj_lst2 <- split_rows$num_subj_lst2
  rows1 <- split_rows$rows1
  rows2 <- split_rows$rows2

  # Outer permutation orders (include identity as first column)
  # MATLAB split-half uses missnk_rri_perm_order: identity in column 1, and
  # (unlike rri_perm_order) does not reject sequential order for later columns.
  if (is.null(outer_reorder)) {
    reorder <- .pls_perm_order_missnk(num_subj_lst, k, num_outer_perm, not_in_cond = is_struct)
  } else {
    if (!is.matrix(outer_reorder)) {
      stop("outer_reorder must be a matrix")
    }
    if (nrow(outer_reorder) != total_rows) {
      stop("outer_reorder must have nrow equal to nrow(stacked_datamat)")
    }
    outer_reorder <- apply(outer_reorder, 2, as.integer)
    if (!all(apply(outer_reorder, 2, function(x) identical(sort(x), seq_len(total_rows))))) {
      stop("Each column of outer_reorder must be a permutation of 1:total_rows")
    }
    reorder <- outer_reorder
    num_outer_perm <- ncol(reorder)
  }

  if (!is.null(inner_subject_perms)) {
    if (!is.list(inner_subject_perms) || length(inner_subject_perms) != num_outer_perm) {
      stop("inner_subject_perms must be a list of length num_outer_perm")
    }
    for (op in seq_len(num_outer_perm)) {
      if (!is.list(inner_subject_perms[[op]]) || length(inner_subject_perms[[op]]) != num_split) {
        stop("inner_subject_perms[[op]] must be a list of length num_split")
      }
      for (p in seq_len(num_split)) {
        if (!is.list(inner_subject_perms[[op]][[p]]) || length(inner_subject_perms[[op]][[p]]) != num_groups) {
          stop("inner_subject_perms[[op]][[p]] must be a list of length num_groups")
        }
      }
    }
  }

  if (method %in% c(2L, 5L, 6L)) {
    if (is.null(stacked_designdata)) {
      stop("stacked_designdata is required for non-rotated split-half validation")
    }
    stacked_designdata <- normalize_rows(stacked_designdata, margin = 2L)
  }

  kk <- length(bscan)
  ucorr_distrib <- NULL
  vcorr_distrib <- NULL

  if (progress) {
    pb <- cli::cli_progress_bar("Split-half (outer perm x split)", total = num_outer_perm * num_split)
  }

  for (op in seq_len(num_outer_perm)) {
    datamat_reorder <- reorder[, op]
    behavdata_reorder <- if (method %in% c(3L, 4L, 5L, 6L)) seq_len(total_rows) else NULL
    datamat_reorder_4beh <- if (method %in% c(4L, 6L)) reorder[, op] else NULL

    covcor_op <- pls_get_covcor(
      method = method,
      stacked_datamat = stacked_datamat,
      stacked_behavdata = stacked_behavdata,
      num_groups = num_groups,
      num_subj_lst = num_subj_lst,
      num_cond = k,
      bscan = bscan,
      meancentering_type = meancentering_type,
      cormode = cormode,
      datamat_reorder = datamat_reorder,
      behavdata_reorder = behavdata_reorder,
      datamat_reorder_4beh = datamat_reorder_4beh
    )

    datamatsvd_op <- covcor_op$datamatsvd

    # Outer-permutation LV patterns (u_op, v_op)
    if (method %in% c(2L, 5L, 6L)) {
      v_op <- stacked_designdata
      crossblock <- crossprod(stacked_designdata, datamatsvd_op)
      u_op <- if (nonrotated_boot) {
        # MATLAB: u = normalize(crossblock') when nonrotated_boot is enabled
        normalize_rows(t(crossblock), margin = 2L)
      } else {
        t(crossblock)
      }
    } else {
      r <- nrow(datamatsvd_op)
      c <- ncol(datamatsvd_op)
      if (r <= c) {
        svd_op <- pls_svd(t(datamatsvd_op))
        u_op <- svd_op$u
        v_op <- svd_op$v
      } else {
        svd_op <- pls_svd(datamatsvd_op)
        v_op <- svd_op$u
        u_op <- svd_op$v
      }
    }

    if (is.null(ucorr_distrib)) {
      num_lvs <- min(ncol(u_op), ncol(v_op))
      ucorr_distrib <- matrix(0, nrow = num_outer_perm, ncol = num_lvs)
      vcorr_distrib <- matrix(0, nrow = num_outer_perm, ncol = num_lvs)
    }

    num_lvs <- ncol(ucorr_distrib)
    u_op <- u_op[, seq_len(min(num_lvs, ncol(u_op))), drop = FALSE]
    v_op <- v_op[, seq_len(min(num_lvs, ncol(v_op))), drop = FALSE]

    # Inner split loop: subject permutation within group, split into halves
    for (p in seq_len(num_split)) {
      subject_perm_by_group <- lapply(seq_len(num_groups), function(g) {
        n <- num_subj_lst[g]
        if (!is.null(inner_subject_perms)) {
          as.integer(inner_subject_perms[[op]][[p]][[g]])
        } else {
          sample.int(n, size = n, replace = FALSE)
        }
      })
      in_reorder <- .pls_splithalf_in_reorder(num_subj_lst, k, subject_perm_by_group)

      # Apply outer permutation, then inner subject-randomization, then half selection
      idx1 <- in_reorder[rows1]
      idx2 <- in_reorder[rows2]

      datamat_reorder1 <- datamat_reorder[idx1]
      datamat_reorder2 <- datamat_reorder[idx2]

      behavdata_reorder1 <- if (!is.null(behavdata_reorder)) behavdata_reorder[idx1] else NULL
      behavdata_reorder2 <- if (!is.null(behavdata_reorder)) behavdata_reorder[idx2] else NULL

      datamat_reorder_4beh1 <- if (!is.null(datamat_reorder_4beh)) datamat_reorder_4beh[idx1] else NULL
      datamat_reorder_4beh2 <- if (!is.null(datamat_reorder_4beh)) datamat_reorder_4beh[idx2] else NULL

      datamatsvd_p1 <- pls_get_covcor(
        method = method,
        stacked_datamat = stacked_datamat,
        stacked_behavdata = stacked_behavdata,
        num_groups = num_groups,
        num_subj_lst = num_subj_lst1,
        num_cond = k,
        bscan = bscan,
        meancentering_type = meancentering_type,
        cormode = cormode,
        datamat_reorder = datamat_reorder1,
        behavdata_reorder = behavdata_reorder1,
        datamat_reorder_4beh = datamat_reorder_4beh1
      )$datamatsvd

      datamatsvd_p2 <- pls_get_covcor(
        method = method,
        stacked_datamat = stacked_datamat,
        stacked_behavdata = stacked_behavdata,
        num_groups = num_groups,
        num_subj_lst = num_subj_lst2,
        num_cond = k,
        bscan = bscan,
        meancentering_type = meancentering_type,
        cormode = cormode,
        datamat_reorder = datamat_reorder2,
        behavdata_reorder = behavdata_reorder2,
        datamat_reorder_4beh = datamat_reorder_4beh2
      )$datamatsvd

      u_p1 <- crossprod(datamatsvd_p1, v_op)
      u_p2 <- crossprod(datamatsvd_p2, v_op)
      v_p1 <- datamatsvd_p1 %*% u_op
      v_p2 <- datamatsvd_p2 %*% u_op

      for (lv in seq_len(num_lvs)) {
        ucorr_distrib[op, lv] <- ucorr_distrib[op, lv] +
          pls_xcor(matrix(u_p1[, lv], ncol = 1), matrix(u_p2[, lv], ncol = 1), cormode)[1, 1]
        vcorr_distrib[op, lv] <- vcorr_distrib[op, lv] +
          pls_xcor(matrix(v_p1[, lv], ncol = 1), matrix(v_p2[, lv], ncol = 1), cormode)[1, 1]
      }

      if (progress) {
        cli::cli_progress_update(id = pb)
      }
    }
  }

  if (progress) {
    cli::cli_progress_done(id = pb)
  }

  ucorr_distrib <- ucorr_distrib / num_split
  vcorr_distrib <- vcorr_distrib / num_split

  orig_ucorr <- ucorr_distrib[1, ]
  orig_vcorr <- vcorr_distrib[1, ]

  # P-values: exceedance over the permuted outer distribution
  ucorr_prob <- rep(NA_real_, length(orig_ucorr))
  vcorr_prob <- rep(NA_real_, length(orig_vcorr))
  ucorr_ll <- rep(NA_real_, length(orig_ucorr))
  ucorr_ul <- rep(NA_real_, length(orig_ucorr))
  vcorr_ll <- rep(NA_real_, length(orig_vcorr))
  vcorr_ul <- rep(NA_real_, length(orig_vcorr))

  if (num_outer_perm > 1L) {
    for (lv in seq_along(orig_ucorr)) {
      ucorr_prob[lv] <- sum(ucorr_distrib[2:num_outer_perm, lv] > orig_ucorr[lv]) / (num_outer_perm - 1L)
      vcorr_prob[lv] <- sum(vcorr_distrib[2:num_outer_perm, lv] > orig_vcorr[lv]) / (num_outer_perm - 1L)
    }

    # CI from null distribution percentiles (MATLAB percentile(2:end, 100-clim/clim))
    ucorr_ll <- pls_percentile(ucorr_distrib[2:num_outer_perm, , drop = FALSE], 100 - clim)
    ucorr_ul <- pls_percentile(ucorr_distrib[2:num_outer_perm, , drop = FALSE], clim)
    vcorr_ll <- pls_percentile(vcorr_distrib[2:num_outer_perm, , drop = FALSE], 100 - clim)
    vcorr_ul <- pls_percentile(vcorr_distrib[2:num_outer_perm, , drop = FALSE], clim)
  }

  new_pls_splithalf_result(
    num_outer_perm = num_outer_perm,
    num_split = num_split,
    orig_ucorr = orig_ucorr,
    orig_vcorr = orig_vcorr,
    ucorr_prob = ucorr_prob,
    vcorr_prob = vcorr_prob,
    ucorr_ul = ucorr_ul,
    ucorr_ll = ucorr_ll,
    vcorr_ul = vcorr_ul,
    vcorr_ll = vcorr_ll
  )
}
