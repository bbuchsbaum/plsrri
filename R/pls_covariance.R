#' Covariance/Correlation Matrix Construction
#'
#' @description
#' Constructs the cross-block covariance or correlation matrices used in PLS.
#' Ported from MATLAB rri_get_covcor.m
#'
#' @name pls-covariance
NULL

#' Correlation Maps for Behavior PLS
#'
#' @description
#' Computes correlation between behavior data and brain data within each
#' condition, stacked across all conditions.
#'
#' @param behavdata Behavior matrix (n_obs x n_behav)
#' @param datamat Brain data matrix (n_obs x n_voxels)
#' @param n_subj Number of subjects
#' @param num_cond Number of conditions
#' @param cormode Correlation mode (0, 2, 4, or 6)
#'
#' @return Correlation matrix (n_behav * n_cond x n_voxels)
#' @export
pls_corr_maps <- function(behavdata, datamat, n_subj, num_cond, cormode = 0L) {
  assert_that(is.matrix(behavdata))
  assert_that(is.matrix(datamat))
  assert_that(nrow(behavdata) == nrow(datamat))
  assert_that(is.numeric(n_subj), length(n_subj) >= 1)

  n_behav <- ncol(behavdata)
  n_voxels <- ncol(datamat)

  corrs <- matrix(0, nrow = n_behav * num_cond, ncol = n_voxels)

  if (length(n_subj) == 1L) {
    n_subj <- as.integer(n_subj)
    for (k in seq_len(num_cond)) {
      row_start <- 1 + n_subj * (k - 1)
      row_end <- n_subj * k

      behav_cond <- behavdata[row_start:row_end, , drop = FALSE]
      data_cond <- datamat[row_start:row_end, , drop = FALSE]

      corr_idx_start <- 1 + n_behav * (k - 1)
      corr_idx_end <- n_behav * k

      corrs[corr_idx_start:corr_idx_end, ] <- pls_xcor(behav_cond, data_cond, cormode)
    }
  } else {
    n_subj_cond <- as.integer(n_subj)
    if (length(n_subj_cond) != num_cond) {
      stop("For ssb designs, n_subj must have length num_cond")
    }
    if (sum(n_subj_cond) != nrow(datamat)) {
      stop("For ssb designs, sum(n_subj) must equal number of rows in datamat")
    }

    step <- 0L
    for (k in seq_len(num_cond)) {
      row_start <- 1 + step
      row_end <- step + n_subj_cond[k]

      behav_cond <- behavdata[row_start:row_end, , drop = FALSE]
      data_cond <- datamat[row_start:row_end, , drop = FALSE]

      corr_idx_start <- 1 + n_behav * (k - 1)
      corr_idx_end <- n_behav * k

      corrs[corr_idx_start:corr_idx_end, ] <- pls_xcor(behav_cond, data_cond, cormode)
      step <- step + n_subj_cond[k]
    }
  }

  corrs
}

#' Correlation Maps for Subset of Conditions
#'
#' @description
#' Computes correlation maps for a subset of conditions (used in multiblock PLS).
#'
#' @param behavdata Behavior matrix
#' @param datamat Brain data matrix
#' @param n_subj Number of subjects
#' @param bscan Vector of condition indices to use
#' @param cormode Correlation mode
#'
#' @return Correlation matrix for selected conditions
#' @export
pls_corr_maps_notall <- function(behavdata, datamat, n_subj, bscan, cormode = 0L) {
  assert_that(is.matrix(behavdata))
  assert_that(is.matrix(datamat))
  assert_that(is.numeric(n_subj), length(n_subj) >= 1)

  n_behav <- ncol(behavdata)
  n_voxels <- ncol(datamat)
  num_bscan <- length(bscan)
  bscan <- as.integer(bscan)

  corrs <- matrix(0, nrow = n_behav * num_bscan, ncol = n_voxels)

  if (length(n_subj) == 1L) {
    n_subj <- as.integer(n_subj)
    for (i in seq_along(bscan)) {
      k <- bscan[i]
      row_start <- 1 + n_subj * (k - 1)
      row_end <- n_subj * k

      behav_cond <- behavdata[row_start:row_end, , drop = FALSE]
      data_cond <- datamat[row_start:row_end, , drop = FALSE]

      corr_idx_start <- 1 + n_behav * (i - 1)
      corr_idx_end <- n_behav * i

      corrs[corr_idx_start:corr_idx_end, ] <- pls_xcor(behav_cond, data_cond, cormode)
    }
  } else {
    n_subj_cond <- as.integer(n_subj)
    if (sum(n_subj_cond) != nrow(datamat)) {
      stop("For ssb designs, sum(n_subj) must equal number of rows in datamat")
    }
    if (length(n_subj_cond) < max(bscan)) {
      stop("For ssb designs, n_subj must have at least max(bscan) elements")
    }

    cum <- c(0L, cumsum(n_subj_cond))
    for (i in seq_along(bscan)) {
      k <- bscan[i]
      row_start <- cum[k] + 1L
      row_end <- cum[k + 1L]

      behav_cond <- behavdata[row_start:row_end, , drop = FALSE]
      data_cond <- datamat[row_start:row_end, , drop = FALSE]

      corr_idx_start <- 1 + n_behav * (i - 1L)
      corr_idx_end <- n_behav * i

      corrs[corr_idx_start:corr_idx_end, ] <- pls_xcor(behav_cond, data_cond, cormode)
    }
  }

  corrs
}

#' Get Covariance/Correlation for SVD
#'
#' @description
#' Main function for preparing the cross-block matrix for SVD decomposition.
#' Handles all 6 PLS methods with appropriate mean-centering and correlation modes.
#' Ported from MATLAB rri_get_covcor.m
#'
#' @param method PLS method (1-6)
#' @param stacked_datamat Stacked brain data matrix
#' @param stacked_behavdata Stacked behavior data matrix (methods 3-6)
#' @param num_groups Number of groups
#' @param num_subj_lst Vector of subjects per group
#' @param num_cond Number of conditions
#' @param bscan Conditions for behavior block (multiblock)
#' @param meancentering_type Mean-centering type (0-3)
#' @param cormode Correlation mode (0, 2, 4, 6)
#' @param datamat_reorder Optional reordering indices for datamat
#' @param behavdata_reorder Optional reordering indices for behavdata
#' @param datamat_reorder_4beh Optional reordering for behavior block datamat
#' @param compute_smeanmat Logical, compute `stacked_smeanmat` for brain-score CIs
#'
#' @return List with:
#' \describe{
#'   \item{datamatsvd}{Cross-block matrix for SVD}
#'   \item{datamatsvd_unnorm}{Unnormalized version (multiblock)}
#'   \item{datamatcorrs_lst}{Correlation matrices by group (behavior PLS)}
#'   \item{stacked_smeanmat}{Matrix for brain score CIs (task PLS with bootstrap)}
#' }
#'
#' @export
pls_get_covcor <- function(method,
                            stacked_datamat,
                            stacked_behavdata = NULL,
                            num_groups,
                            num_subj_lst,
                            num_cond,
                            bscan = seq_len(num_cond),
                            meancentering_type = 0L,
                            cormode = 0L,
                            datamat_reorder = NULL,
                            behavdata_reorder = NULL,
                            datamat_reorder_4beh = NULL,
                            compute_smeanmat = FALSE) {

  assert_that(is.matrix(stacked_datamat))
  assert_that(method %in% 1:6)

  total_rows <- nrow(stacked_datamat)

  # Default reorder is identity

if (is.null(datamat_reorder)) {
    datamat_reorder <- seq_len(total_rows)
  }
  if (is.null(behavdata_reorder)) {
    behavdata_reorder <- seq_len(total_rows)
  }
  if (is.null(datamat_reorder_4beh)) {
    datamat_reorder_4beh <- seq_len(total_rows)
  }

  n_features <- ncol(stacked_datamat)
  k <- num_cond

  is_ssb <- is.list(num_subj_lst)
  group_n_rows <- if (!is_ssb) {
    as.integer(num_subj_lst) * k
  } else {
    vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1))
  }

  # Pre-compute grand means for meancentering types 1-3
  grand_mean <- NULL
  group_mean <- NULL
  cond_mean <- NULL

  if (meancentering_type == 1L) {
    grand_mean <- matrix(0, nrow = num_cond, ncol = n_features)
    for (g in seq_len(num_groups)) {
      span <- sum(group_n_rows[seq_len(g - 1)])
      datamat <- stacked_datamat[datamat_reorder, , drop = FALSE]
      datamat <- datamat[(1 + span):(group_n_rows[g] + span), , drop = FALSE]

      if (!is_ssb) {
        n <- as.integer(num_subj_lst[g])
        grand_mean <- grand_mean + pls_task_mean(datamat, n)
      } else {
        n <- as.integer(num_subj_lst[[g]])
        grand_mean <- grand_mean + pls_task_mean_ssb(datamat, n)
      }
    }
    grand_mean <- grand_mean / num_groups

  } else if (meancentering_type == 2L) {
    all_data <- stacked_datamat[datamat_reorder, , drop = FALSE]
    grand_mean <- colMeans(all_data)

  } else if (meancentering_type == 3L) {
    cond_group_mean <- array(0, dim = c(num_cond, num_groups, n_features))
    for (g in seq_len(num_groups)) {
      span <- sum(group_n_rows[seq_len(g - 1)])
      datamat <- stacked_datamat[datamat_reorder, , drop = FALSE]
      datamat <- datamat[(1 + span):(group_n_rows[g] + span), , drop = FALSE]

      if (!is_ssb) {
        n <- as.integer(num_subj_lst[g])
        cond_group_mean[, g, ] <- pls_task_mean(datamat, n)
      } else {
        n <- as.integer(num_subj_lst[[g]])
        cond_group_mean[, g, ] <- pls_task_mean_ssb(datamat, n)
      }
    }

    cond_mean <- apply(cond_group_mean, c(2, 3), mean)  # groups x features
    group_mean <- apply(cond_group_mean, c(1, 3), mean)  # conds x features
    grand_mean <- colMeans(cond_mean)
  }

  # Initialize outputs
  datamatsvd <- NULL
  datamatsvd_unnorm <- NULL
  datamatcorrs_lst <- list()
  stacked_smeanmat <- NULL

  # Loop across groups
  for (g in seq_len(num_groups)) {
    span <- sum(group_n_rows[seq_len(g - 1)])

    datamat <- stacked_datamat[datamat_reorder, , drop = FALSE]
    datamat <- datamat[(1 + span):(group_n_rows[g] + span), , drop = FALSE]

    if (!is_ssb) {
      n <- as.integer(num_subj_lst[g])
      n_cond <- n
    } else {
      n <- as.integer(num_subj_lst[[g]])
      n_cond <- n
    }

    # Get behavior block datamat for multiblock
    if (method %in% c(4L, 6L)) {
      datamat_4beh <- stacked_datamat[datamat_reorder_4beh, , drop = FALSE]
      datamat_4beh <- datamat_4beh[(1 + span):(group_n_rows[g] + span), , drop = FALSE]
    }

    # Get behavior data
    if (method %in% c(3L, 4L, 5L, 6L)) {
      behavdata <- stacked_behavdata[behavdata_reorder, , drop = FALSE]
      behavdata <- behavdata[(1 + span):(group_n_rows[g] + span), , drop = FALSE]
    }

    # Compute correlation/covariance based on method
    TBdatamatcorrs <- NULL

    if (method == 1L) {
      # Mean-Centering Task PLS
      task_mean <- if (!is_ssb) pls_task_mean(datamat, n) else pls_task_mean_ssb(datamat, n)

      if (meancentering_type == 0L) {
        group_data_mean <- colMeans(datamat)
        datamatcorrs <- task_mean - matrix(group_data_mean, nrow = k,
                                            ncol = n_features, byrow = TRUE)
        if (isTRUE(compute_smeanmat)) {
          smeanmat <- datamat - matrix(group_data_mean, nrow = nrow(datamat),
                                       ncol = n_features, byrow = TRUE)
        }
      } else if (meancentering_type == 1L) {
        datamatcorrs <- task_mean - grand_mean
        if (isTRUE(compute_smeanmat)) {
          if (!is_ssb) {
            smeanmat <- do.call(rbind, lapply(seq_len(num_cond), function(cond_idx) {
              idx <- (cond_idx - 1L) * n + seq_len(n)
              datamat[idx, , drop = FALSE] - matrix(grand_mean[cond_idx, ],
                                                    nrow = n, ncol = n_features, byrow = TRUE)
            }))
          } else {
            smeanmat <- NULL
            step <- 0L
            for (cond_idx in seq_len(num_cond)) {
              idx <- step + seq_len(n_cond[cond_idx])
              smeanmat <- rbind(
                smeanmat,
                datamat[idx, , drop = FALSE] - matrix(grand_mean[cond_idx, ],
                                                      nrow = length(idx), ncol = n_features, byrow = TRUE)
              )
              step <- step + n_cond[cond_idx]
            }
          }
        }
      } else if (meancentering_type == 2L) {
        datamatcorrs <- task_mean - matrix(grand_mean, nrow = k,
                                            ncol = n_features, byrow = TRUE)
        if (isTRUE(compute_smeanmat)) {
          smeanmat <- datamat - matrix(grand_mean, nrow = nrow(datamat),
                                       ncol = n_features, byrow = TRUE)
        }
      } else if (meancentering_type == 3L) {
        datamatcorrs <- task_mean - group_mean -
          matrix(cond_mean[g, ], nrow = k, ncol = n_features, byrow = TRUE) +
          matrix(grand_mean, nrow = k, ncol = n_features, byrow = TRUE)
        if (isTRUE(compute_smeanmat)) {
          if (!is_ssb) {
            smeanmat <- do.call(rbind, lapply(seq_len(num_cond), function(cond_idx) {
              idx <- (cond_idx - 1L) * n + seq_len(n)
              datamat[idx, , drop = FALSE] -
                matrix(group_mean[cond_idx, ], nrow = n, ncol = n_features, byrow = TRUE) -
                matrix(cond_mean[g, ], nrow = n, ncol = n_features, byrow = TRUE) +
                matrix(grand_mean, nrow = n, ncol = n_features, byrow = TRUE)
            }))
          } else {
            smeanmat <- NULL
            step <- 0L
            for (cond_idx in seq_len(num_cond)) {
              idx <- step + seq_len(n_cond[cond_idx])
              smeanmat <- rbind(
                smeanmat,
                datamat[idx, , drop = FALSE] -
                  matrix(group_mean[cond_idx, ], nrow = length(idx), ncol = n_features, byrow = TRUE) -
                  matrix(cond_mean[g, ], nrow = length(idx), ncol = n_features, byrow = TRUE) +
                  matrix(grand_mean, nrow = length(idx), ncol = n_features, byrow = TRUE)
              )
              step <- step + n_cond[cond_idx]
            }
          }
        }
      }

    } else if (method == 2L) {
      # Non-Rotated Task PLS
      datamatcorrs <- if (!is_ssb) pls_task_mean(datamat, n) else pls_task_mean_ssb(datamat, n)
      if (isTRUE(compute_smeanmat)) {
        if (meancentering_type == 0L) {
          group_data_mean <- colMeans(datamat)
          smeanmat <- datamat - matrix(group_data_mean, nrow = nrow(datamat),
                                       ncol = n_features, byrow = TRUE)
        } else if (meancentering_type == 1L) {
          if (!is_ssb) {
            smeanmat <- do.call(rbind, lapply(seq_len(num_cond), function(cond_idx) {
              idx <- (cond_idx - 1L) * n + seq_len(n)
              datamat[idx, , drop = FALSE] - matrix(grand_mean[cond_idx, ],
                                                    nrow = n, ncol = n_features, byrow = TRUE)
            }))
          } else {
            smeanmat <- NULL
            step <- 0L
            for (cond_idx in seq_len(num_cond)) {
              idx <- step + seq_len(n_cond[cond_idx])
              smeanmat <- rbind(
                smeanmat,
                datamat[idx, , drop = FALSE] - matrix(grand_mean[cond_idx, ],
                                                      nrow = length(idx), ncol = n_features, byrow = TRUE)
              )
              step <- step + n_cond[cond_idx]
            }
          }
        } else if (meancentering_type == 2L) {
          smeanmat <- datamat - matrix(grand_mean, nrow = nrow(datamat),
                                       ncol = n_features, byrow = TRUE)
        } else if (meancentering_type == 3L) {
          if (!is_ssb) {
            smeanmat <- do.call(rbind, lapply(seq_len(num_cond), function(cond_idx) {
              idx <- (cond_idx - 1L) * n + seq_len(n)
              datamat[idx, , drop = FALSE] -
                matrix(group_mean[cond_idx, ], nrow = n, ncol = n_features, byrow = TRUE) -
                matrix(cond_mean[g, ], nrow = n, ncol = n_features, byrow = TRUE) +
                matrix(grand_mean, nrow = n, ncol = n_features, byrow = TRUE)
            }))
          } else {
            smeanmat <- NULL
            step <- 0L
            for (cond_idx in seq_len(num_cond)) {
              idx <- step + seq_len(n_cond[cond_idx])
              smeanmat <- rbind(
                smeanmat,
                datamat[idx, , drop = FALSE] -
                  matrix(group_mean[cond_idx, ], nrow = length(idx), ncol = n_features, byrow = TRUE) -
                  matrix(cond_mean[g, ], nrow = length(idx), ncol = n_features, byrow = TRUE) +
                  matrix(grand_mean, nrow = length(idx), ncol = n_features, byrow = TRUE)
              )
              step <- step + n_cond[cond_idx]
            }
          }
        }
      }

    } else if (method %in% c(3L, 5L)) {
      # Behavior PLS (rotated or non-rotated)
      datamatcorrs <- pls_corr_maps(behavdata, datamat, n, k, cormode)
      datamatcorrs_lst <- c(datamatcorrs_lst, list(datamatcorrs))

    } else if (method == 4L) {
      # Regular Multiblock PLS
      task_mean <- if (!is_ssb) pls_task_mean(datamat, n) else pls_task_mean_ssb(datamat, n)

      if (meancentering_type == 0L) {
        group_data_mean <- colMeans(datamat)
        Tdatamatcorrs <- task_mean - matrix(group_data_mean, nrow = k,
                                             ncol = n_features, byrow = TRUE)
        if (isTRUE(compute_smeanmat)) {
          smeanmat <- datamat - matrix(group_data_mean, nrow = nrow(datamat),
                                       ncol = n_features, byrow = TRUE)
        }
      } else if (meancentering_type == 1L) {
        Tdatamatcorrs <- task_mean - grand_mean
        if (isTRUE(compute_smeanmat)) {
          if (!is_ssb) {
            smeanmat <- do.call(rbind, lapply(seq_len(num_cond), function(cond_idx) {
              idx <- (cond_idx - 1L) * n + seq_len(n)
              datamat[idx, , drop = FALSE] - matrix(grand_mean[cond_idx, ],
                                                    nrow = n, ncol = n_features, byrow = TRUE)
            }))
          } else {
            smeanmat <- NULL
            step <- 0L
            for (cond_idx in seq_len(num_cond)) {
              idx <- step + seq_len(n_cond[cond_idx])
              smeanmat <- rbind(
                smeanmat,
                datamat[idx, , drop = FALSE] - matrix(grand_mean[cond_idx, ],
                                                      nrow = length(idx), ncol = n_features, byrow = TRUE)
              )
              step <- step + n_cond[cond_idx]
            }
          }
        }
      } else if (meancentering_type == 2L) {
        Tdatamatcorrs <- task_mean - matrix(grand_mean, nrow = k,
                                             ncol = n_features, byrow = TRUE)
        if (isTRUE(compute_smeanmat)) {
          smeanmat <- datamat - matrix(grand_mean, nrow = nrow(datamat),
                                       ncol = n_features, byrow = TRUE)
        }
      } else if (meancentering_type == 3L) {
        Tdatamatcorrs <- task_mean - group_mean -
          matrix(cond_mean[g, ], nrow = k, ncol = n_features, byrow = TRUE) +
          matrix(grand_mean, nrow = k, ncol = n_features, byrow = TRUE)
        if (isTRUE(compute_smeanmat)) {
          if (!is_ssb) {
            smeanmat <- do.call(rbind, lapply(seq_len(num_cond), function(cond_idx) {
              idx <- (cond_idx - 1L) * n + seq_len(n)
              datamat[idx, , drop = FALSE] -
                matrix(group_mean[cond_idx, ], nrow = n, ncol = n_features, byrow = TRUE) -
                matrix(cond_mean[g, ], nrow = n, ncol = n_features, byrow = TRUE) +
                matrix(grand_mean, nrow = n, ncol = n_features, byrow = TRUE)
            }))
          } else {
            smeanmat <- NULL
            step <- 0L
            for (cond_idx in seq_len(num_cond)) {
              idx <- step + seq_len(n_cond[cond_idx])
              smeanmat <- rbind(
                smeanmat,
                datamat[idx, , drop = FALSE] -
                  matrix(group_mean[cond_idx, ], nrow = length(idx), ncol = n_features, byrow = TRUE) -
                  matrix(cond_mean[g, ], nrow = length(idx), ncol = n_features, byrow = TRUE) +
                  matrix(grand_mean, nrow = length(idx), ncol = n_features, byrow = TRUE)
              )
              step <- step + n_cond[cond_idx]
            }
          }
        }
      }

      Bdatamatcorrs <- pls_corr_maps_notall(behavdata, datamat_4beh, n, bscan, cormode)
      datamatcorrs_lst <- c(datamatcorrs_lst, list(Bdatamatcorrs))

      # Stack and normalize
      TBdatamatcorrs <- rbind(Tdatamatcorrs, Bdatamatcorrs)
      datamatcorrs <- rbind(normalize_rows(Tdatamatcorrs),
                            normalize_rows(Bdatamatcorrs))

    } else if (method == 6L) {
      # Non-Rotated Multiblock PLS
      Tdatamatcorrs <- if (!is_ssb) pls_task_mean(datamat, n) else pls_task_mean_ssb(datamat, n)
      Bdatamatcorrs <- pls_corr_maps_notall(behavdata, datamat_4beh, n, bscan, cormode)
      datamatcorrs_lst <- c(datamatcorrs_lst, list(Bdatamatcorrs))

      TBdatamatcorrs <- rbind(Tdatamatcorrs, Bdatamatcorrs)
      datamatcorrs <- rbind(normalize_rows(Tdatamatcorrs),
                            normalize_rows(Bdatamatcorrs))

      if (isTRUE(compute_smeanmat)) {
        if (meancentering_type == 0L) {
          group_data_mean <- colMeans(datamat)
          smeanmat <- datamat - matrix(group_data_mean, nrow = nrow(datamat),
                                       ncol = n_features, byrow = TRUE)
        } else if (meancentering_type == 1L) {
          if (!is_ssb) {
            smeanmat <- do.call(rbind, lapply(seq_len(num_cond), function(cond_idx) {
              idx <- (cond_idx - 1L) * n + seq_len(n)
              datamat[idx, , drop = FALSE] - matrix(grand_mean[cond_idx, ],
                                                    nrow = n, ncol = n_features, byrow = TRUE)
            }))
          } else {
            smeanmat <- NULL
            step <- 0L
            for (cond_idx in seq_len(num_cond)) {
              idx <- step + seq_len(n_cond[cond_idx])
              smeanmat <- rbind(
                smeanmat,
                datamat[idx, , drop = FALSE] - matrix(grand_mean[cond_idx, ],
                                                      nrow = length(idx), ncol = n_features, byrow = TRUE)
              )
              step <- step + n_cond[cond_idx]
            }
          }
        } else if (meancentering_type == 2L) {
          smeanmat <- datamat - matrix(grand_mean, nrow = nrow(datamat),
                                       ncol = n_features, byrow = TRUE)
        } else if (meancentering_type == 3L) {
          if (!is_ssb) {
            smeanmat <- do.call(rbind, lapply(seq_len(num_cond), function(cond_idx) {
              idx <- (cond_idx - 1L) * n + seq_len(n)
              datamat[idx, , drop = FALSE] -
                matrix(group_mean[cond_idx, ], nrow = n, ncol = n_features, byrow = TRUE) -
                matrix(cond_mean[g, ], nrow = n, ncol = n_features, byrow = TRUE) +
                matrix(grand_mean, nrow = n, ncol = n_features, byrow = TRUE)
            }))
          } else {
            smeanmat <- NULL
            step <- 0L
            for (cond_idx in seq_len(num_cond)) {
              idx <- step + seq_len(n_cond[cond_idx])
              smeanmat <- rbind(
                smeanmat,
                datamat[idx, , drop = FALSE] -
                  matrix(group_mean[cond_idx, ], nrow = length(idx), ncol = n_features, byrow = TRUE) -
                  matrix(cond_mean[g, ], nrow = length(idx), ncol = n_features, byrow = TRUE) +
                  matrix(grand_mean, nrow = length(idx), ncol = n_features, byrow = TRUE)
              )
              step <- step + n_cond[cond_idx]
            }
          }
        }
      }
    }

    # Stack results
    datamatsvd_unnorm <- rbind(datamatsvd_unnorm, TBdatamatcorrs)
    datamatsvd <- rbind(datamatsvd, datamatcorrs)

    if (isTRUE(compute_smeanmat) && method %in% c(1L, 2L, 4L, 6L)) {
      stacked_smeanmat <- rbind(stacked_smeanmat, smeanmat)
    }
  }

  list(
    datamatsvd = datamatsvd,
    datamatsvd_unnorm = datamatsvd_unnorm,
    datamatcorrs_lst = datamatcorrs_lst,
    stacked_smeanmat = stacked_smeanmat
  )
}
