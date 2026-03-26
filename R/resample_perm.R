#' Permutation Testing for PLS
#'
#' @description
#' Functions for generating permutation orders and running permutation tests.
#' Ported from MATLAB rri_perm_order.m
#'
#' @name pls-permutation
NULL

#' Generate Permutation Orders
#'
#' @description
#' Generates permutation reordering indices for PLS permutation testing.
#' The method permutes conditions within subjects, then permutes subjects
#' across groups.
#'
#' @param num_subj_lst Integer vector of subjects per group
#' @param num_cond Number of conditions
#' @param num_perm Number of permutations to generate
#' @param not_in_cond Logical, if TRUE don't permute conditions within group
#'   (for structure PLS)
#'
#' @return Matrix of permutation orders (total_rows x num_perm)
#' @export
#'
#' @examples
#' perm_order <- pls_perm_order(c(10, 12), num_cond = 3, num_perm = 100)
pls_perm_order <- function(num_subj_lst, num_cond, num_perm, not_in_cond = FALSE) {
  if (is.list(num_subj_lst)) {
    return(.pls_perm_order_ssb(num_subj_lst, num_cond, num_perm))
  }

  assert_that(is.numeric(num_subj_lst))
  assert_that(is.count(num_cond))
  assert_that(is.count(num_perm))

  k <- num_cond
  num_subj_grp <- sum(num_subj_lst)
  total_rows <- num_subj_grp * k
  num_groups <- length(num_subj_lst)

  perm_order <- matrix(0L, nrow = total_rows, ncol = num_perm)

  for (p in seq_len(num_perm)) {
    cnt <- 0
    duplicated <- TRUE

    while (duplicated && cnt <= 500) {
      cnt <- cnt + 1

      # Build task_group matrix: conditions x subjects
      # Row i contains the row indices for condition i across all subjects
      first <- 1L
      last <- 0L
      task_group <- NULL

      for (g in seq_len(num_groups)) {
        last <- last + k * num_subj_lst[g]
        # MATLAB: reshape(first:last, num_subj_lst(g), num_cond) is column-major.
        tmp <- matrix(first:last, nrow = num_subj_lst[g], ncol = k)
        task_group <- cbind(task_group, t(tmp))
        first <- last + 1L
      }

      origin_task_group <- task_group

      # Permute conditions within each subject (unless is_struct)
      if (!not_in_cond) {
        for (i in seq_len(num_subj_grp)) {
          task_perm <- sample.int(k)
          task_group[, i] <- task_group[task_perm, i]
        }
      }

      # Permute subjects across groups
      group_perm <- sample.int(num_subj_grp)
      task_group <- task_group[, group_perm, drop = FALSE]

      # Check if permutation changes condition averages
      duplicated <- FALSE

      for (c in seq_len(k)) {
        accum <- 0L
        for (g in seq_len(num_groups)) {
          idx_start <- accum + 1L
          idx_end <- accum + num_subj_lst[g]

          perm_sorted <- sort(task_group[c, idx_start:idx_end])
          orig_sorted <- origin_task_group[c, idx_start:idx_end]

          if (identical(perm_sorted, orig_sorted)) {
            duplicated <- TRUE
            break
          }

          accum <- accum + num_subj_lst[g]
        }
        if (duplicated) break
      }

      # Build new permutation order
      new_perm_order <- integer(0)
      for (g in seq_len(num_groups)) {
        start_col <- sum(num_subj_lst[seq_len(g - 1)]) + 1
        end_col <- sum(num_subj_lst[seq_len(g)])

        tmp <- task_group[, start_col:end_col, drop = FALSE]
        tmp <- as.integer(t(tmp))  # Transpose and flatten: subject in condition order
        new_perm_order <- c(new_perm_order, tmp)
      }

      # Check for duplicate with previous permutations
      if (p > 1) {
        for (i in seq_len(p - 1)) {
          if (identical(perm_order[, i], new_perm_order)) {
            duplicated <- TRUE
            break
          }
        }
      }

      # Check for identity permutation
      if (identical(seq_len(total_rows), new_perm_order)) {
        duplicated <- TRUE
      }
    }

    if (cnt > 500) {
      warning("Duplicate permutation orders may be used")
    }

    perm_order[, p] <- new_perm_order
  }

  perm_order
}

#' NK/MissNK Permutation Orders (include unpermuted sample)
#'
#' @description
#' MATLAB parity for `missnk_rri_perm_order.m`: first column is the identity
#' ordering, and sequential order is not treated as invalid for subsequent
#' permutations.
#'
#' @param num_subj_lst Integer vector of subjects per group
#' @param num_cond Number of conditions
#' @param num_perm Number of permutations to generate (includes identity as first)
#' @param not_in_cond Logical, if TRUE don't permute conditions within group
#'
#' @return Matrix of permutation orders (total_rows x num_perm)
#' @keywords internal
.pls_perm_order_missnk <- function(num_subj_lst, num_cond, num_perm, not_in_cond = FALSE) {
  assert_that(is.numeric(num_subj_lst))
  assert_that(is.count(num_cond))
  assert_that(is.count(num_perm))

  k <- as.integer(num_cond)
  num_subj_lst <- as.integer(num_subj_lst)
  num_subj_grp <- sum(num_subj_lst)
  total_rows <- num_subj_grp * k
  num_groups <- length(num_subj_lst)

  perm_order <- matrix(0L, nrow = total_rows, ncol = num_perm)
  perm_order[, 1L] <- seq_len(total_rows)

  if (num_perm == 1L) {
    return(perm_order)
  }

  for (p in 2L:num_perm) {
    cnt <- 0L
    duplicated <- TRUE

    while (duplicated && cnt <= 500L) {
      cnt <- cnt + 1L

      first <- 1L
      last <- 0L
      task_group <- NULL

      for (g in seq_len(num_groups)) {
        last <- last + k * num_subj_lst[g]
        tmp <- matrix(first:last, nrow = num_subj_lst[g], ncol = k)
        task_group <- cbind(task_group, t(tmp))
        first <- last + 1L
      }

      origin_task_group <- task_group

      if (!isTRUE(not_in_cond)) {
        for (i in seq_len(num_subj_grp)) {
          task_perm <- sample.int(k)
          task_group[, i] <- task_group[task_perm, i]
        }
      }

      group_perm <- sample.int(num_subj_grp)
      task_group <- task_group[, group_perm, drop = FALSE]

      duplicated <- FALSE
      if (k > 1L) {
        for (c in seq_len(k)) {
          accum <- 0L
          for (g in seq_len(num_groups)) {
            idx_start <- accum + 1L
            idx_end <- accum + num_subj_lst[g]
            perm_sorted <- sort(task_group[c, idx_start:idx_end])
            orig_sorted <- origin_task_group[c, idx_start:idx_end]
            if (identical(perm_sorted, orig_sorted)) {
              duplicated <- TRUE
              break
            }
            accum <- accum + num_subj_lst[g]
          }
          if (duplicated) break
        }
      }

      new_perm_order <- integer(0)
      for (g in seq_len(num_groups)) {
        start_col <- sum(num_subj_lst[seq_len(g - 1)]) + 1L
        end_col <- sum(num_subj_lst[seq_len(g)])
        tmp <- task_group[, start_col:end_col, drop = FALSE]
        tmp <- as.integer(t(tmp))
        new_perm_order <- c(new_perm_order, tmp)
      }

      if (!duplicated) {
        for (i in seq_len(p - 1L)) {
          if (identical(perm_order[, i], new_perm_order)) {
            duplicated <- TRUE
            break
          }
        }
      }
    }

    if (cnt > 500L) {
      warning("Duplicate permutation orders may be used (missnk)")
    }

    perm_order[, p] <- new_perm_order
  }

  perm_order
}

#' SSB Permutation Orders (Unequal n per condition)
#'
#' @keywords internal
.pls_perm_order_ssb <- function(num_subj_lst, num_cond, num_perm) {
  assert_that(is.list(num_subj_lst))
  assert_that(is.count(num_cond))
  assert_that(is.count(num_perm))

  total_rows <- sum(vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1)))
  subj_group <- as.integer(unlist(num_subj_lst))

  perm_order <- matrix(0L, nrow = total_rows, ncol = num_perm)

  for (p in seq_len(num_perm)) {
    cnt <- 0L
    duplicated <- TRUE

    while (duplicated && cnt <= 500L) {
      cnt <- cnt + 1L

      new_perm_order <- sample.int(total_rows)

      # MATLAB ssb_rri_perm_order has a weak duplication check; mimic by
      # rejecting permutations where any block happens to be already sorted.
      duplicated <- FALSE
      accum <- 0L
      for (blk in seq_along(subj_group)) {
        n <- subj_group[blk]
        seg <- new_perm_order[(accum + 1L):(accum + n)]
        if (identical(seg, sort(seg))) {
          duplicated <- TRUE
          break
        }
        accum <- accum + n
      }

      # Check duplicates with previous permutations
      if (!duplicated && p > 1L) {
        for (i in seq_len(p - 1L)) {
          if (identical(perm_order[, i], new_perm_order)) {
            duplicated <- TRUE
            break
          }
        }
      }

      # Reject identity permutation
      if (!duplicated && identical(seq_len(total_rows), new_perm_order)) {
        duplicated <- TRUE
      }
    }

    if (cnt > 500L) {
      warning("Duplicate permutation orders may be used (ssb)")
    }

    perm_order[, p] <- new_perm_order
  }

  perm_order
}

#' Run Permutation Test
#'
#' @description
#' Runs permutation test to assess significance of singular values.
#'
#' @param stacked_datamat Stacked data matrix
#' @param stacked_behavdata Behavior data matrix (methods 3-6)
#' @param stacked_designdata Design contrast matrix (methods 2, 5, 6)
#' @param num_groups Number of groups
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param method PLS method (1-6)
#' @param num_perm Number of permutations
#' @param observed_s Observed singular values
#' @param bscan Conditions for behavior block
#' @param meancentering_type Mean-centering type
#' @param cormode Correlation mode
#' @param is_struct Structure PLS flag
#' @param progress Show progress
#'
#' @return pls_perm_result object
#' @keywords internal
pls_permutation_test <- function(stacked_datamat,
                                  stacked_behavdata = NULL,
                                  stacked_designdata = NULL,
                                  num_groups,
                                  num_subj_lst,
                                  num_cond,
                                  method,
                                  num_perm,
                                  observed_s,
                                  observed_v = NULL,
                                  org_s = NULL,
                                  org_v = NULL,
                                  bscan = NULL,
                                  meancentering_type = 0L,
                                  cormode = 0L,
                                  is_struct = FALSE,
                                  permsamp = NULL,
                                  Tpermsamp = NULL,
                                  Bpermsamp = NULL,
                                  progress = TRUE) {

  total_rows <- nrow(stacked_datamat)
  n_lv <- length(observed_s)

  if (is.null(bscan)) {
    bscan <- seq_len(num_cond)
  }

  normalize_perm_matrix <- function(x, name) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!is.matrix(x) || !is.numeric(x)) {
      stop(sprintf("%s must be a numeric matrix", name))
    }
    if (nrow(x) != total_rows || ncol(x) != num_perm) {
      stop(sprintf(
        "%s has shape [%d x %d] but expected [%d x %d]",
        name, nrow(x), ncol(x), total_rows, num_perm
      ))
    }
    if (any(!is.finite(x))) {
      stop(sprintf("%s must contain finite indices", name))
    }
    storage.mode(x) <- "integer"
    x
  }

  # MATLAB compatibility: for behavior/multiblock methods, permsamp is the
  # behavior permutation matrix when Bpermsamp is not explicitly provided.
  if (method %in% c(3L, 4L, 5L, 6L) && is.null(Bpermsamp) && !is.null(permsamp)) {
    Bpermsamp <- permsamp
  }

  if (method %in% c(4L, 6L)) {
    # Multiblock: permute task block with rri_perm_order, and permute only
    # bscan rows for behavior data (rri_randperm_notall). Brain data for the
    # behavior block is NOT permuted.
    if (is.null(Tpermsamp)) {
      Tpermsamp <- pls_perm_order(num_subj_lst, num_cond, num_perm, not_in_cond = is_struct)
    }
    if (is.null(Bpermsamp)) {
      Bpermsamp <- replicate(num_perm, pls_randperm_notall(num_subj_lst, num_cond, bscan))
      Bpermsamp <- matrix(Bpermsamp, nrow = total_rows, ncol = num_perm)
    }
  } else if (method %in% c(3L, 5L)) {
    # Behavior PLS: permute behavior rows only (randperm of observations)
    if (is.null(Bpermsamp)) {
      Bpermsamp <- replicate(num_perm, sample.int(total_rows))
      Bpermsamp <- matrix(Bpermsamp, nrow = total_rows, ncol = num_perm)
    }
  } else {
    # Task PLS: permute datamat rows (rri_perm_order)
    if (is.null(permsamp)) {
      permsamp <- pls_perm_order(num_subj_lst, num_cond, num_perm, not_in_cond = is_struct)
    }
  }

  permsamp <- normalize_perm_matrix(permsamp, "permsamp")
  Tpermsamp <- normalize_perm_matrix(Tpermsamp, "Tpermsamp")
  Bpermsamp <- normalize_perm_matrix(Bpermsamp, "Bpermsamp")

  # Exact compiled fast path for the common balanced task-PLS case.
  if (.plsrri_fast_paths_enabled() &&
      identical(as.integer(method), 1L) &&
      !is.list(num_subj_lst) &&
      identical(as.integer(meancentering_type), 0L) &&
      !any(!is.finite(stacked_datamat)) &&
      !is.null(permsamp)) {
    sp <- perm_test_task_cpp(
      stacked_datamat = stacked_datamat,
      permsamp = permsamp,
      observed_s = as.numeric(observed_s),
      num_groups = as.integer(num_groups),
      num_subj_lst = as.integer(num_subj_lst),
      num_cond = as.integer(num_cond),
      meancentering_type = as.integer(meancentering_type)
    )

    return(new_pls_perm_result(
      num_perm = num_perm,
      sp = as.integer(sp),
      sprob = as.integer(sp) / num_perm,
      permsamp = permsamp,
      Tpermsamp = Tpermsamp,
      Bpermsamp = Bpermsamp
    ))
  }

  # Count how many permuted singular values >= observed
  sp <- rep(0L, n_lv)

  if (progress) {
    pb <- cli::cli_progress_bar("Permutation test", total = num_perm)
  }

  for (p in seq_len(num_perm)) {
    # Determine reorder indices per method
    datamat_reorder <- seq_len(total_rows)
    behavdata_reorder <- NULL
    datamat_reorder_4beh <- NULL

    if (method %in% c(4L, 6L)) {
      datamat_reorder <- Tpermsamp[, p]
      behavdata_reorder <- Bpermsamp[, p]
      datamat_reorder_4beh <- seq_len(total_rows)
    } else if (method %in% c(3L, 5L)) {
      datamat_reorder <- seq_len(total_rows)
      behavdata_reorder <- Bpermsamp[, p]
    } else {
      datamat_reorder <- permsamp[, p]
    }

    covcor <- pls_get_covcor(
      method = method,
      stacked_datamat = stacked_datamat,
      stacked_behavdata = stacked_behavdata,
      num_groups = num_groups,
      num_subj_lst = num_subj_lst,
      num_cond = num_cond,
      bscan = bscan,
      meancentering_type = meancentering_type,
      cormode = cormode,
      datamat_reorder = datamat_reorder,
      behavdata_reorder = behavdata_reorder,
      datamat_reorder_4beh = datamat_reorder_4beh
    )

    datamatsvd_perm <- covcor$datamatsvd
    datamatsvd_unnorm_perm <- covcor$datamatsvd_unnorm

    # Compute singular values for permuted data
    if (method %in% c(2L, 5L, 6L)) {
      crossblock <- crossprod(stacked_designdata, datamatsvd_perm)
      s_perm <- sqrt(rowSums(crossblock^2))
    } else {
      if (is.null(observed_v)) {
        stop("observed_v is required for rotated (SVD-based) permutation tests")
      }

      # SVD then Procrustes-align pv to observed_v (MATLAB rri_bootprocrust)
      r <- nrow(datamatsvd_perm)
      c <- ncol(datamatsvd_perm)
      n_keep <- min(n_lv, min(r, c))

      if (r <= c) {
        svd_result <- svd(t(datamatsvd_perm), nu = n_keep, nv = n_keep)
        pv <- svd_result$v
        d <- svd_result$d
      } else {
        svd_result <- svd(datamatsvd_perm, nu = n_keep, nv = n_keep)
        pv <- svd_result$u
        d <- svd_result$d
      }

      pv <- pv[, seq_len(n_keep), drop = FALSE]
      d <- d[seq_len(n_keep)]

      rotatemat <- pls_bootprocrust(observed_v[, seq_len(n_keep), drop = FALSE], pv)
      pv_scaled <- pv %*% diag(d, nrow = n_keep) %*% rotatemat

      s_perm <- sqrt(colSums(pv_scaled^2))

      # Multiblock SSQ adjustment uses unnormalized cross-block SSQ
      if (method == 4L) {
        if (is.null(org_s)) {
          stop("org_s is required for multiblock permutation test")
        }

        ptotal_s <- sum(datamatsvd_unnorm_perm^2)
        per <- s_perm^2 / sum(s_perm^2)
        s_perm <- sqrt(per * ptotal_s)

        # compare against adjusted observed singular values
        observed_s <- org_s
      }
    }

    # Truncate to same number of LVs
    s_perm <- s_perm[seq_len(min(length(s_perm), n_lv))]
    if (length(s_perm) < n_lv) {
      s_perm <- c(s_perm, rep(0, n_lv - length(s_perm)))
    }

    # Count
    sp <- sp + as.integer(s_perm >= observed_s)

    if (progress) {
      cli::cli_progress_update(id = pb)
    }
  }

  if (progress) {
    cli::cli_progress_done(id = pb)
  }

  # Compute probabilities
  sprob <- sp / num_perm

  new_pls_perm_result(
    num_perm = num_perm,
    sp = sp,
    sprob = sprob,
    permsamp = if (!is.null(permsamp)) permsamp else Bpermsamp,
    Tpermsamp = Tpermsamp,
    Bpermsamp = Bpermsamp
  )
}
