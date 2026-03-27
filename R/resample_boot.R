#' Bootstrap Testing for PLS
#'
#' @description
#' Functions for generating bootstrap orders and computing bootstrap ratios.
#' Ported from MATLAB rri_boot_order.m and related functions.
#'
#' @name pls-bootstrap
NULL

#' Low-variability check for bootstrap behavior vectors (MATLAB rri_islowvariability)
#'
#' @keywords internal
.pls_islowvariability <- function(bodat, behavdata) {
  orig_u <- unique(behavdata)
  counts <- vapply(orig_u, function(u) {
    if (is.na(u)) {
      sum(is.na(bodat))
    } else {
      sum(bodat == u, na.rm = TRUE)
    }
  }, integer(1))

  if (length(behavdata) == 0L) {
    return(FALSE)
  }

  (max(counts) / length(behavdata)) >= 0.5
}

#' Precompute group/condition row positions in stacked ordering
#'
#' Returns positions in 1:total_rows for each group and condition. These are
#' positions into a bootstrap order vector, matching MATLAB's indexing logic.
#'
#' @keywords internal
.pls_group_condition_positions <- function(num_subj_lst, num_cond) {
  k <- as.integer(num_cond)
  is_ssb <- is.list(num_subj_lst)

  out <- vector("list", length(num_subj_lst))
  offset <- 0L

  for (g in seq_along(num_subj_lst)) {
    out[[g]] <- vector("list", k)

    if (!is_ssb) {
      n <- as.integer(num_subj_lst[g])
      for (c in seq_len(k)) {
        out[[g]][[c]] <- offset + (c - 1L) * n + seq_len(n)
      }
      offset <- offset + n * k
    } else {
      n_vec <- as.integer(num_subj_lst[[g]])
      if (length(n_vec) != k) {
        stop("For ssb designs, each num_subj_lst[[g]] must have length num_cond")
      }

      step <- 0L
      for (c in seq_len(k)) {
        n <- n_vec[c]
        out[[g]][[c]] <- offset + step + seq_len(n)
        step <- step + n
      }
      offset <- offset + sum(n_vec)
    }
  }

  out
}

#' Check Bootstrap Feasibility
#'
#' @description
#' Checks if exhaustive bootstrap enumeration is possible (for small samples)
#' and computes the minimum subjects per group.
#'
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param num_boot Requested bootstrap samples
#' @param incl_seq Include sequential order in samples
#'
#' @return List with min_subj_per_group, is_boot_samples, boot_samples, new_num_boot
#' @export
pls_boot_check <- function(num_subj_lst, num_cond, num_boot, incl_seq = FALSE) {
  num_groups <- length(num_subj_lst)

  if (min(num_subj_lst) < 3) {
    stop("Bootstrap analysis requires that each group must have at least 3 subjects")
  }

  # Minimum subjects to sample from each group
  min_subj_per_group <- max(2, ceiling(min(num_subj_lst) / 2))

  is_boot_samples <- rep(FALSE, num_groups)
  boot_samples <- vector("list", num_groups)
  new_num_boot <- num_boot

  # MATLAB rri_boot_check: only enumerate when *all* groups have n <= 8.
  # In that case, enumeration is used for every group, regardless of num_boot.
  if (all(num_subj_lst <= 8)) {
    for (g in seq_len(num_groups)) {
      n <- num_subj_lst[g]

      # Generate all possible bootstrap samples (excluding identity)
      # Using nondecreasing sequences (MATLAB rri_boot_samples)
      all_samples <- generate_boot_samples(n, min_subj_per_group)

      # Total possible samples includes the identity only when incl_seq is TRUE.
      n_possible <- nrow(all_samples) + if (isTRUE(incl_seq)) 1L else 0L

      if (n_possible < new_num_boot) {
        new_num_boot <- n_possible
      }

      if (nrow(all_samples) > 0) {
        all_samples <- all_samples[sample.int(nrow(all_samples)), , drop = FALSE]
      }

      is_boot_samples[g] <- TRUE
      boot_samples[[g]] <- all_samples
    }
  }

  list(
    min_subj_per_group = min_subj_per_group,
    is_boot_samples = is_boot_samples,
    boot_samples = boot_samples,
    new_num_boot = new_num_boot
  )
}

#' Generate All Bootstrap Samples (Helper)
#'
#' @description
#' Generates all possible bootstrap samples of size n from n items
#' that have at least min_unique unique elements.
#'
#' @param n Sample size
#' @param min_unique Minimum unique elements required
#'
#' @return Matrix where each row is a bootstrap sample
#' @keywords internal
generate_boot_samples <- function(n, min_unique) {
  assert_that(is.count(n))
  assert_that(is.count(min_unique))

  # Generate all nondecreasing sequences of length n with values in 1..n.
  # Count is choose(2n-1, n) for sampling n from n with replacement.
  out <- vector("list", 0L)
  current <- integer(n)

  idx <- 0L
  gen <- function(pos, start_val) {
    if (pos > n) {
      idx <<- idx + 1L
      out[[idx]] <<- current
      return(invisible(NULL))
    }

    for (val in start_val:n) {
      current[pos] <<- val
      gen(pos + 1L, val)
    }
  }

  gen(1L, 1L)

  mat <- do.call(rbind, out)
  storage.mode(mat) <- "integer"

  # Filter by minimum unique elements
  keep <- apply(mat, 1, function(x) length(unique(x)) >= min_unique)
  mat <- mat[keep, , drop = FALSE]

  # Remove the sequential (identity) sample (1..n). In this representation,
  # it's the only sample with all unique subjects.
  identity <- seq_len(n)
  not_identity <- apply(mat, 1, function(x) !identical(as.integer(x), identity))
  mat <- mat[not_identity, , drop = FALSE]

  mat
}

#' Generate Bootstrap Orders
#'
#' @description
#' Generates bootstrap resampling orders for PLS bootstrap testing.
#' Resampling is done within groups.
#'
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param num_boot Number of bootstrap samples
#' @param bscan Conditions for behavior block
#' @param incl_seq Include sequential order
#' @param boot_type "strat" (stratified) or "nonstrat" (non-stratified)
#'
#' @return Matrix of bootstrap orders (total_rows x num_boot)
#' @export
pls_boot_order <- function(num_subj_lst, num_cond, num_boot,
                            bscan = NULL, incl_seq = FALSE, boot_type = "strat") {
  if (is.list(num_subj_lst)) {
    return(.pls_boot_order_ssb(
      num_subj_lst = num_subj_lst,
      num_cond = num_cond,
      num_boot = num_boot,
      bscan = bscan,
      incl_seq = incl_seq,
      boot_type = boot_type
    ))
  }

  assert_that(is.numeric(num_subj_lst))
  assert_that(is.count(num_cond))
  assert_that(is.count(num_boot))

  if (is.null(bscan)) {
    bscan <- seq_len(num_cond)
  }

  num_cond0 <- as.integer(num_cond)
  bscan <- as.integer(bscan)
  num_cond_sub <- length(bscan)

  # First generate orders for the selected conditions as if they were the
  # only conditions present, then map back to the full row space. This
  # matches MATLAB rri_boot_order's bscan handling.
  boot_order_sub <- .pls_boot_order_core(
    num_subj_lst = num_subj_lst,
    num_cond = num_cond_sub,
    num_boot = num_boot,
    incl_seq = incl_seq,
    boot_type = boot_type
  )

  new_num_boot <- ncol(boot_order_sub)

  # If bscan includes all conditions, we're done.
  if (num_cond_sub == num_cond0) {
    return(boot_order_sub)
  }

  total_rows0 <- sum(num_subj_lst) * num_cond0
  template <- make_row_indices(num_subj_lst, num_cond0, condition = bscan)

  # Map subset indices to original row indices
  boot_order_mapped <- matrix(
    template[as.vector(boot_order_sub)],
    nrow = length(template),
    ncol = new_num_boot
  )

  # Fill a full identity order, then replace only template rows
  boot_order_full <- matrix(rep(seq_len(total_rows0), new_num_boot),
                            nrow = total_rows0, ncol = new_num_boot)
  boot_order_full[template, ] <- boot_order_mapped

  boot_order_full
}

#' Core Bootstrap Order Generator for SSB designs
#'
#' @keywords internal
.pls_boot_order_ssb <- function(num_subj_lst, num_cond, num_boot,
                                bscan = NULL, incl_seq = FALSE, boot_type = "strat") {
  assert_that(is.list(num_subj_lst))
  assert_that(is.count(num_cond))
  assert_that(is.count(num_boot))
  assert_that(boot_type %in% c("strat", "nonstrat"))

  if (boot_type != "strat") {
    stop("ssb bootstrap currently supports only boot_type = 'strat'")
  }

  if (is.null(bscan)) {
    bscan <- seq_len(num_cond)
  }
  bscan <- as.integer(bscan)

  total_rows0 <- sum(vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1)))
  boot_order <- matrix(0L, nrow = total_rows0, ncol = num_boot)

  # Determine minimum unique samples required (MATLAB ssb_rri_boot_check logic)
  selected_counts <- unlist(lapply(num_subj_lst, function(x) as.integer(x)[bscan]))
  if (length(selected_counts) == 0) {
    stop("bscan selects no conditions")
  }
  if (min(selected_counts) < 3L) {
    stop("Bootstrap analysis requires each selected condition have at least 3 subjects (ssb)")
  }
  min_unique <- max(2L, ceiling(min(selected_counts) / 2))

  for (p in seq_len(num_boot)) {
    if (incl_seq && p == 1L) {
      boot_order[, p] <- seq_len(total_rows0)
      next
    }

    cnt <- 0L
    duplicated <- TRUE

    while (duplicated && cnt <= 500L) {
      cnt <- cnt + 1L

      new_order <- seq_len(total_rows0)
      offset <- 0L

      for (g in seq_along(num_subj_lst)) {
        n_vec <- as.integer(num_subj_lst[[g]])
        if (length(n_vec) != num_cond) {
          stop("For ssb designs, each num_subj_lst[[g]] must have length num_cond")
        }

        step <- 0L
        for (c in seq_len(num_cond)) {
          n <- n_vec[c]
          idx <- offset + step + seq_len(n)

          if (c %in% bscan) {
            repeat {
              samp <- sample(idx, size = length(idx), replace = TRUE)
              if (length(unique(samp)) >= min_unique) break
            }
            new_order[idx] <- samp
          }

          step <- step + n
        }

        offset <- offset + sum(n_vec)
      }

      duplicated <- FALSE

      # Reject identity if not including sequential
      if (!incl_seq && identical(new_order, seq_len(total_rows0))) {
        duplicated <- TRUE
      }

      if (!duplicated && p > 1L) {
        for (i in seq_len(p - 1L)) {
          if (identical(boot_order[, i], new_order)) {
            duplicated <- TRUE
            break
          }
        }
      }
    }

    if (cnt > 500L) {
      warning("Duplicate bootstrap orders may be used (ssb)")
    }

    boot_order[, p] <- new_order
  }

  boot_order
}

#' Core Bootstrap Order Generator (No bscan mapping)
#'
#' @keywords internal
.pls_boot_order_core <- function(num_subj_lst, num_cond, num_boot,
                                 incl_seq = FALSE, boot_type = "strat") {
  total_subj <- sum(num_subj_lst)
  num_groups <- length(num_subj_lst)
  k <- as.integer(num_cond)
  total_rows <- k * total_subj

  if (boot_type == "nonstrat") {
    # Non-stratified: sample from all subjects regardless of group membership
    return(pls_boot_order_nonstrat(num_subj_lst, k, num_boot))
  }

  # Check if we can enumerate all samples
  boot_check <- pls_boot_check(num_subj_lst, num_cond, num_boot, incl_seq)
  new_num_boot <- boot_check$new_num_boot

  # Generate subject reordering for each group
  tmp_boot_order <- matrix(0L, nrow = total_subj, ncol = new_num_boot)

  # Include sequential (original) order explicitly as the first sample.
  # This is required for MATLAB-parity bootstrap (method 1 includes orig
  # sample via incl_seq).
  if (incl_seq && new_num_boot >= 1) {
    tmp_boot_order[, 1] <- seq_len(total_subj)
  }

  for (p in seq_len(new_num_boot)) {
    if (incl_seq && p == 1L) {
      next
    }

    subj_order <- vector("list", num_groups)
    not_done <- TRUE
    cnt <- 0

    while (not_done && cnt <= 500) {
      cnt <- cnt + 1
      start_subj <- 1L

      for (g in seq_len(num_groups)) {
        n <- num_subj_lst[g]

        if (boot_check$is_boot_samples[g]) {
          # Use pre-enumerated samples
          sample_row <- p
          if (incl_seq) {
            # When incl_seq is TRUE, column 1 is the sequential (identity) sample,
            # and boot_samples excludes that identity row.
            sample_row <- p - 1L
          }

          new_subj_order <- boot_check$boot_samples[[g]][sample_row, ]
          not_done <- FALSE
        } else {
          # Random sampling with replacement
          repeat {
            new_subj_order <- sample.int(n, size = n, replace = TRUE)

            # Ensure at least min_subj_per_group unique subjects
            if (length(unique(new_subj_order)) >= boot_check$min_subj_per_group) {
              break
            }
          }
          not_done <- TRUE
        }

        subj_order[[g]] <- new_subj_order + start_subj - 1L
        start_subj <- start_subj + n
      }

      # Check for duplicate samples
      if (!all(boot_check$is_boot_samples)) {
        combined <- unlist(subj_order)

        not_done <- FALSE
        if (p > 1) {
          for (i in seq_len(p - 1)) {
            if (identical(tmp_boot_order[, i], combined)) {
              not_done <- TRUE
              break
            }
          }
        }

        # Don't allow identity if not including sequential
        if (!incl_seq && identical(combined, seq_len(total_subj))) {
          not_done <- TRUE
        }
      }
    }

    if (cnt > 500) {
      warning("Duplicate bootstrap orders may be used")
    }

    tmp_boot_order[, p] <- unlist(subj_order)
  }

  # Build row index mapping: conditions x subjects
  first <- 1L
  last <- 0L
  row_idx <- NULL

  for (g in seq_len(num_groups)) {
    last <- last + k * num_subj_lst[g]
    tmp <- matrix(first:last, nrow = num_subj_lst[g], ncol = k, byrow = TRUE)
    row_idx <- cbind(row_idx, t(tmp))
    first <- last + 1L
  }

  # Apply subject reordering to get full row reordering
  boot_order <- matrix(0L, nrow = total_rows, ncol = new_num_boot)

  for (p in seq_len(new_num_boot)) {
    reordered_idx <- row_idx[, tmp_boot_order[, p], drop = FALSE]

    # Flatten in group order
    flat_order <- integer(0)
    for (g in seq_len(num_groups)) {
      start_col <- sum(num_subj_lst[seq_len(g - 1)]) + 1
      end_col <- sum(num_subj_lst[seq_len(g)])

      tmp <- reordered_idx[, start_col:end_col, drop = FALSE]
      flat_order <- c(flat_order, as.integer(t(tmp)))
    }

    boot_order[, p] <- flat_order
  }

  boot_order
}

#' Non-Stratified Bootstrap Orders
#'
#' @keywords internal
pls_boot_order_nonstrat <- function(num_subj_lst, num_cond, num_boot) {
  total_subj <- sum(num_subj_lst)
  num_groups <- length(num_subj_lst)
  k <- num_cond
  total_rows <- k * total_subj

  boot_order <- matrix(0L, nrow = total_rows, ncol = num_boot)

  for (p in seq_len(num_boot)) {
    # Sample subjects (ignoring group membership)
    subj_order <- sample.int(total_subj, size = total_subj, replace = TRUE)

    # Map to rows
    # Build row mapping
    first <- 1L
    last <- 0L
    row_idx <- NULL

    for (g in seq_len(num_groups)) {
      last <- last + k * num_subj_lst[g]
      tmp <- matrix(first:last, nrow = num_subj_lst[g], ncol = k, byrow = TRUE)
      row_idx <- cbind(row_idx, t(tmp))
      first <- last + 1L
    }

    # Apply reordering
    reordered_idx <- row_idx[, subj_order, drop = FALSE]

    flat_order <- integer(0)
    for (g in seq_len(num_groups)) {
      start_col <- sum(num_subj_lst[seq_len(g - 1)]) + 1
      end_col <- sum(num_subj_lst[seq_len(g)])

      tmp <- reordered_idx[, start_col:end_col, drop = FALSE]
      flat_order <- c(flat_order, as.integer(t(tmp)))
    }

    boot_order[, p] <- flat_order
  }

  boot_order
}

#' Run Bootstrap Test
#'
#' @description
#' Runs bootstrap test to compute bootstrap ratios and confidence intervals.
#'
#' @param stacked_datamat Stacked data matrix
#' @param stacked_behavdata Behavior data matrix (methods 3-6)
#' @param stacked_designdata Design contrast matrix (methods 2, 5, 6)
#' @param num_groups Number of groups
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param method PLS method (1-6)
#' @param num_boot Number of bootstrap samples
#' @param observed_u Observed u (saliences)
#' @param observed_v Observed v (loadings)
#' @param bscan Conditions for behavior block
#' @param meancentering_type Mean-centering type
#' @param cormode Correlation mode
#' @param boot_type Bootstrap type
#' @param clim Confidence level
#' @param parallel_config Optional parallel execution config list with
#'   `backend` and `workers`. This currently affects task-only bootstrap.
#' @param progress Show progress
#'
#' @return pls_boot_result object
#' @keywords internal
pls_bootstrap_test <- function(stacked_datamat,
                                stacked_behavdata = NULL,
                                stacked_designdata = NULL,
                                num_groups,
                                num_subj_lst,
                                num_cond,
                                method,
                                num_boot,
                                observed_u,
                                observed_v,
                                observed_s,
                                observed_lvcorrs = NULL,
                                bscan = NULL,
                                meancentering_type = 0L,
                                cormode = 0L,
                                boot_type = "strat",
                                nonrotated_boot = FALSE,
                                bootsamp = NULL,
                                bootsamp_4beh = NULL,
                                clim = 95,
                                parallel_config = NULL,
                                progress = TRUE) {

  total_rows <- nrow(stacked_datamat)
  n_features <- nrow(observed_u)
  n_lv <- ncol(observed_u)

  if (is.null(bscan)) {
    bscan <- seq_len(num_cond)
  }

  if (method %in% c(3L, 4L, 5L, 6L) && is.null(observed_lvcorrs)) {
    stop("observed_lvcorrs is required for behavior/multiblock bootstrap CIs")
  }

  is_ssb <- is.list(num_subj_lst)
  group_n_rows <- if (!is_ssb) {
    as.integer(num_subj_lst) * as.integer(num_cond)
  } else {
    vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1))
  }

  # Include original un-resampled order only for mean-centering task PLS,
  # or when using Natasha's non-rotated bootstrap (MATLAB parity).
  incl_seq <- isTRUE(method == 1L || nonrotated_boot)

  normalize_boot_matrix <- function(x, name, strict_cols = FALSE) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!is.matrix(x) || !is.numeric(x)) {
      stop(sprintf("%s must be a numeric matrix", name))
    }
    if (nrow(x) != total_rows) {
      stop(sprintf("%s has %d rows but expected %d", name, nrow(x), total_rows))
    }
    if (strict_cols && ncol(x) != num_boot) {
      stop(sprintf("%s has %d columns but expected %d", name, ncol(x), num_boot))
    }
    if (any(!is.finite(x))) {
      stop(sprintf("%s must contain finite indices", name))
    }
    storage.mode(x) <- "integer"
    x
  }

  supplied_bootsamp <- !is.null(bootsamp)
  supplied_bootsamp_4beh <- !is.null(bootsamp_4beh)

  # Bootstrap orders for task block (all conditions).
  if (is.null(bootsamp)) {
    bootsamp <- pls_boot_order(
      num_subj_lst = num_subj_lst,
      num_cond = num_cond,
      num_boot = num_boot,
      bscan = seq_len(num_cond),
      incl_seq = incl_seq,
      boot_type = boot_type
    )
  }

  # Bootstrap orders for behavior block (bscan conditions only; multiblock only).
  if (method %in% c(3L, 4L, 5L, 6L)) {
    if (is.null(bootsamp_4beh)) {
      bootsamp_4beh <- if (method %in% c(4L, 6L)) {
        pls_boot_order(
          num_subj_lst = num_subj_lst,
          num_cond = num_cond,
          num_boot = num_boot,
          bscan = bscan,
          incl_seq = incl_seq,
          boot_type = boot_type
        )
      } else {
        bootsamp
      }
    }
  }

  bootsamp <- normalize_boot_matrix(bootsamp, "bootsamp", strict_cols = supplied_bootsamp)
  bootsamp_4beh <- normalize_boot_matrix(
    bootsamp_4beh,
    "bootsamp_4beh",
    strict_cols = supplied_bootsamp_4beh
  )

  # Match number of boot samples across task and behavior orders
  actual_num_boot <- ncol(bootsamp)
  if (!is.null(bootsamp_4beh)) {
    actual_num_boot <- min(actual_num_boot, ncol(bootsamp_4beh))
    bootsamp_4beh <- bootsamp_4beh[, seq_len(actual_num_boot), drop = FALSE]
  }
  bootsamp <- bootsamp[, seq_len(actual_num_boot), drop = FALSE]
  parallel_cfg <- .plsrri_normalize_parallel_config(parallel_config)

  k <- as.integer(num_cond)
  kk <- length(bscan)

  # Indices for behavior block conditions (bscan) across all groups
  row_idx <- NULL
  if (method %in% c(3L, 4L, 5L, 6L)) {
    row_idx <- make_row_indices(num_subj_lst, num_cond, condition = bscan)
  }

  num_subj_lst_4beh <- if (is_ssb) {
    lapply(num_subj_lst, function(x) as.integer(x)[as.integer(bscan)])
  } else {
    num_subj_lst
  }

  # --- Behavior bootstrap diagnostics / safeguards (MATLAB) ---
  num_LowVariability_behav_boots <- NULL
  badbeh <- NULL
  countnewtotal <- 0L

  # MATLAB uses a scalar is_boot_samples flag: enumeration only when *all*
  # groups have n <= 8. In that case, do not attempt bad-behavior resampling.
  is_boot_samples_all <- if (!is_ssb) {
    all(as.integer(num_subj_lst) <= 8L)
  } else {
    all(as.integer(unlist(num_subj_lst, use.names = FALSE)) <= 8L)
  }

  if (method %in% c(3L, 4L, 5L, 6L)) {
    n_behav <- ncol(stacked_behavdata)
    num_LowVariability_behav_boots <- integer(n_behav)

    for (bw in seq_len(n_behav)) {
      for (p in seq_len(actual_num_boot)) {
        vv <- stacked_behavdata[bootsamp_4beh[, p], bw]
        if (.pls_islowvariability(vv, stacked_behavdata[, bw])) {
          num_LowVariability_behav_boots[bw] <- num_LowVariability_behav_boots[bw] + 1L
        }
      }
    }

    badbeh <- vector("list", num_groups)
  }

  cond_pos <- .pls_group_condition_positions(num_subj_lst, num_cond)

  # --- Initialize distrib / CI targets (MATLAB rri_distrib) ---
  orig_corr <- NULL
  corr_distrib <- NULL
  llcorr <- ulcorr <- llcorr_adj <- ulcorr_adj <- NULL

  orig_usc <- NULL
  usc2 <- NULL
  usc_distrib <- NULL
  task_distrib <- NULL
  llusc <- ulusc <- llusc_adj <- ulusc_adj <- NULL
  Tprop <- NULL

  if (method %in% c(3L, 4L, 5L, 6L)) {
    orig_corr <- observed_lvcorrs
    corr_distrib <- array(0, dim = c(nrow(orig_corr), ncol(orig_corr), actual_num_boot + 1L))
    corr_distrib[, , 1] <- orig_corr
  }

  # smeanmat is required to compute usc2 for method 1/2, and task CIs for
  # method 1 (distrib) and method 4 (Tdistrib).
  stacked_smeanmat_obs <- NULL
  if (method %in% c(1L, 2L, 4L)) {
    covcor_obs <- pls_get_covcor(
      method = method,
      stacked_datamat = stacked_datamat,
      stacked_behavdata = stacked_behavdata,
      num_groups = num_groups,
      num_subj_lst = num_subj_lst,
      num_cond = num_cond,
      bscan = bscan,
      meancentering_type = meancentering_type,
      cormode = cormode,
      datamat_reorder = seq_len(total_rows),
      behavdata_reorder = if (method %in% c(3L, 4L, 5L, 6L)) seq_len(total_rows) else NULL,
      datamat_reorder_4beh = seq_len(total_rows),
      compute_smeanmat = TRUE
    )
    stacked_smeanmat_obs <- covcor_obs$stacked_smeanmat
  }

  if (method %in% c(1L, 2L, 4L, 6L)) {
    if (method %in% c(2L, 6L)) {
      usc_task <- stacked_datamat %*% normalize_rows(observed_u, margin = 2L)
      usc2 <- if (method == 2L) stacked_smeanmat_obs %*% observed_u else NULL
    } else {
      usc_task <- stacked_datamat %*% normalize_rows(observed_u, margin = 2L)
      usc2 <- stacked_smeanmat_obs %*% normalize_rows(observed_u, margin = 2L)
    }

    # orig_usc is always condition means stacked across groups
    orig_usc <- NULL
    first <- 1L
    last <- 0L
    for (g in seq_len(num_groups)) {
      n <- if (!is_ssb) as.integer(num_subj_lst[g]) else as.integer(num_subj_lst[[g]])
      last <- last + group_n_rows[g]
      if (method %in% c(2L, 6L)) {
        orig_usc <- rbind(
          orig_usc,
          if (!is_ssb) {
            pls_task_mean(usc_task[first:last, , drop = FALSE], n)
          } else {
            pls_task_mean_ssb(usc_task[first:last, , drop = FALSE], n)
          }
        )
      } else {
        orig_usc <- rbind(
          orig_usc,
          if (!is_ssb) {
            pls_task_mean(usc2[first:last, , drop = FALSE], n)
          } else {
            pls_task_mean_ssb(usc2[first:last, , drop = FALSE], n)
          }
        )
      }
      first <- last + 1L
    }

    if (method %in% c(4L, 6L)) {
      task_distrib <- array(0, dim = c(nrow(orig_usc), ncol(orig_usc), actual_num_boot + 1L))
      task_distrib[, , 1] <- orig_usc
    } else {
      usc_distrib <- array(0, dim = c(nrow(orig_usc), ncol(orig_usc), actual_num_boot + 1L))
      usc_distrib[, , 1] <- orig_usc
    }
  }

  # Accumulate scaled bootstrap saliences (MATLAB accumulates u*s after Procrustes)
  if (method == 1L) {
    u_sum <- matrix(0, nrow = n_features, ncol = n_lv)
    u_sq <- matrix(0, nrow = n_features, ncol = n_lv)
  } else if (method %in% c(2L, 5L, 6L)) {
    u_sum <- observed_u
    u_sq <- observed_u^2
  } else {
    original_u <- observed_u %*% diag(observed_s, nrow = length(observed_s))
    u_sum <- original_u
    u_sq <- original_u^2
  }

  # MATLAB: when nonrotated_boot is enabled, bootstrap accumulation starts from 0
  # for all methods and uses a method-1-style SE denominator.
  if (isTRUE(nonrotated_boot)) {
    u_sum <- matrix(0, nrow = n_features, ncol = n_lv)
    u_sq <- matrix(0, nrow = n_features, ncol = n_lv)
  }

  task_boot_ci_summary <- function(u_boot_scaled, covcor_boot) {
    if (isTRUE(nonrotated_boot) && method %in% c(1L, 2L)) {
      stacked_smeanmat_boot <- covcor_boot$stacked_smeanmat
      tmp_usc2 <- stacked_smeanmat_boot %*% normalize_rows(u_boot_scaled, margin = 2L)
    } else if (method == 1L) {
      stacked_smeanmat_boot <- covcor_boot$stacked_smeanmat
      tmp_usc2 <- stacked_smeanmat_boot %*% normalize_rows(u_boot_scaled, margin = 2L)
    } else {
      tmp_usc2 <- stacked_datamat %*% normalize_rows(u_boot_scaled, margin = 2L)
    }

    tmp_orig_usc <- NULL
    first <- 1L
    last <- 0L
    for (g in seq_len(num_groups)) {
      n <- if (!is_ssb) as.integer(num_subj_lst[g]) else as.integer(num_subj_lst[[g]])
      last <- last + group_n_rows[g]
      tmp_orig_usc <- rbind(
        tmp_orig_usc,
        if (!is_ssb) {
          pls_task_mean(tmp_usc2[first:last, , drop = FALSE], n)
        } else {
          pls_task_mean_ssb(tmp_usc2[first:last, , drop = FALSE], n)
        }
      )
      first <- last + 1L
    }

    tmp_orig_usc
  }

  task_boot_ci_from_scores <- function(tmp_usc2) {
    tmp_orig_usc <- NULL
    first <- 1L
    last <- 0L
    for (g in seq_len(num_groups)) {
      n <- if (!is_ssb) as.integer(num_subj_lst[g]) else as.integer(num_subj_lst[[g]])
      last <- last + group_n_rows[g]
      tmp_orig_usc <- rbind(
        tmp_orig_usc,
        if (!is_ssb) {
          pls_task_mean(tmp_usc2[first:last, , drop = FALSE], n)
        } else {
          pls_task_mean_ssb(tmp_usc2[first:last, , drop = FALSE], n)
        }
      )
      first <- last + 1L
    }

    tmp_orig_usc
  }

  canonicalize_bootstrap_svd <- function(u_scaled, v_scaled, dvals) {
    if (method != 1L || length(dvals) == 0L) {
      return(list(
        u_scaled = u_scaled,
        v_scaled = v_scaled
      ))
    }

    lead <- max(abs(dvals), na.rm = TRUE)
    if (!is.finite(lead) || lead <= 0) {
      u_scaled[,] <- 0
      if (!is.null(v_scaled)) {
        v_scaled[,] <- 0
      }
      return(list(
        u_scaled = u_scaled,
        v_scaled = v_scaled
      ))
    }

    tol <- 1e-12 * lead
    null_cols <- which(!is.finite(dvals) | abs(dvals) <= tol)
    if (length(null_cols) > 0L) {
      u_scaled[, null_cols] <- 0
      if (!is.null(v_scaled)) {
        v_scaled[, null_cols] <- 0
      }
    }

    list(
      u_scaled = u_scaled,
      v_scaled = v_scaled
    )
  }

  use_reduced_task_boot <- .plsrri_fast_paths_enabled("bootstrap") &&
    as.integer(method) %in% c(1L, 2L) &&
    !isTRUE(nonrotated_boot) &&
    is.null(stacked_behavdata) &&
    !is.list(num_subj_lst) &&
    identical(as.integer(meancentering_type), 0L) &&
    !any(!is.finite(stacked_datamat))

  reduced_task_scores <- NULL
  reduced_task_loadings <- NULL
  reduced_boot_cpp <- NULL
  if (use_reduced_task_boot) {
    task_svd <- pls_svd(stacked_datamat, handle_missing = FALSE)
    reduced_task_scores <- task_svd$u %*% diag(task_svd$d, nrow = length(task_svd$d))
    reduced_task_loadings <- task_svd$v
    reduced_boot_cpp <- get("boot_test_task_reduced_cpp", envir = asNamespace("plsrri"))
  }

  task_boot_sample <- function(b) {
    datamat_reorder <- bootsamp[, b]
    if (use_reduced_task_boot) {
      covcor_reduced <- pls_get_covcor(
        method = method,
        stacked_datamat = reduced_task_scores,
        stacked_behavdata = NULL,
        num_groups = num_groups,
        num_subj_lst = num_subj_lst,
        num_cond = num_cond,
        bscan = bscan,
        meancentering_type = meancentering_type,
        cormode = cormode,
        datamat_reorder = datamat_reorder,
        behavdata_reorder = NULL,
        datamat_reorder_4beh = NULL,
        compute_smeanmat = isTRUE(method == 1L)
      )

      if (method == 1L) {
        datamatsvd_boot <- covcor_reduced$datamatsvd
        r <- nrow(datamatsvd_boot)
        c <- ncol(datamatsvd_boot)
        n_keep <- min(n_lv, min(r, c))

        if (r <= c) {
          svd_result <- svd(t(datamatsvd_boot), nu = n_keep, nv = n_keep)
          pu <- svd_result$u
          pv <- svd_result$v
        } else {
          svd_result <- svd(datamatsvd_boot, nu = n_keep, nv = n_keep)
          pv <- svd_result$u
          pu <- svd_result$v
        }

        d <- svd_result$d[seq_len(n_keep)]
        pv <- pv[, seq_len(n_keep), drop = FALSE]
        pu <- pu[, seq_len(n_keep), drop = FALSE]

        rotatemat <- pls_bootprocrust(observed_v[, seq_len(n_keep), drop = FALSE], pv)
        u_reduced_scaled <- matrix(0, nrow = ncol(reduced_task_loadings), ncol = n_lv)
        u_reduced_scaled[, seq_len(n_keep)] <- pu %*% diag(d, nrow = n_keep) %*% rotatemat
        canon <- canonicalize_bootstrap_svd(u_reduced_scaled, NULL, d)
        u_reduced_scaled <- canon$u_scaled

        list(
          u_boot_scaled = reduced_task_loadings %*% u_reduced_scaled,
          tmp_orig_usc = task_boot_ci_from_scores(
            covcor_reduced$stacked_smeanmat %*% normalize_rows(u_reduced_scaled, margin = 2L)
          )
        )
      } else {
        u_reduced_scaled <- t(crossprod(stacked_designdata, covcor_reduced$datamatsvd))
        u_boot_scaled <- reduced_task_loadings %*% u_reduced_scaled
        list(
          u_boot_scaled = u_boot_scaled,
          tmp_orig_usc = task_boot_ci_summary(u_boot_scaled, covcor_reduced)
        )
      }
    } else {
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
      behavdata_reorder = NULL,
      datamat_reorder_4beh = NULL,
      compute_smeanmat = isTRUE(method == 1L || (nonrotated_boot && method == 2L))
    )

    datamatsvd_boot <- covcor$datamatsvd

    if (isTRUE(nonrotated_boot)) {
      u_boot_scaled <- crossprod(datamatsvd_boot, observed_v)
    } else if (method == 2L) {
      crossblock <- crossprod(stacked_designdata, datamatsvd_boot)
      u_boot_scaled <- t(crossblock)
    } else {
      r <- nrow(datamatsvd_boot)
      c <- ncol(datamatsvd_boot)
      n_keep <- min(n_lv, min(r, c))

      if (r <= c) {
        svd_result <- svd(t(datamatsvd_boot), nu = n_keep, nv = n_keep)
        pu <- svd_result$u
        pv <- svd_result$v
      } else {
        svd_result <- svd(datamatsvd_boot, nu = n_keep, nv = n_keep)
        pv <- svd_result$u
        pu <- svd_result$v
      }

      d <- svd_result$d[seq_len(n_keep)]
      pv <- pv[, seq_len(n_keep), drop = FALSE]
      pu <- pu[, seq_len(n_keep), drop = FALSE]

      rotatemat <- pls_bootprocrust(observed_v[, seq_len(n_keep), drop = FALSE], pv)
      u_boot_scaled <- matrix(0, nrow = n_features, ncol = n_lv)
      u_boot_scaled[, seq_len(n_keep)] <- pu %*% diag(d, nrow = n_keep) %*% rotatemat
      canon <- canonicalize_bootstrap_svd(u_boot_scaled, NULL, d)
      u_boot_scaled <- canon$u_scaled
    }

    list(
      u_boot_scaled = u_boot_scaled,
      tmp_orig_usc = task_boot_ci_summary(u_boot_scaled, covcor)
    )
    }
  }

  use_task_boot_helper <- method %in% c(1L, 2L) &&
    is.null(stacked_behavdata) &&
    !is.null(usc_distrib)

  if (use_task_boot_helper) {
    if (use_reduced_task_boot && !parallel_cfg$enabled) {
      reduced_chunk_eval <- function(cols) {
        reduced_boot_cpp(
          task_scores = reduced_task_scores,
          task_loadings = reduced_task_loadings,
          stacked_datamat = stacked_datamat,
          bootsamp = bootsamp[, cols, drop = FALSE],
          observed_v = observed_v,
          stacked_designdata = if (method == 2L) stacked_designdata else
            matrix(0, nrow = num_groups * num_cond, ncol = 0L),
          num_groups = as.integer(num_groups),
          num_subj_lst = as.integer(num_subj_lst),
          num_cond = as.integer(num_cond),
          method = as.integer(method)
        )
      }

      if (parallel_cfg$enabled) {
        chunks <- .plsrri_split_indices(actual_num_boot, parallel_cfg$workers)
        boot_parts <- .plsrri_parallel_lapply(chunks, reduced_chunk_eval, parallel_cfg)

        sample_idx <- 1L
        for (part in boot_parts) {
          n_chunk <- dim(part$usc_distrib_boot)[3]
          u_sum <- u_sum + part$u_sum
          u_sq <- u_sq + part$u_sq
          usc_distrib[, , sample_idx:(sample_idx + n_chunk - 1L) + 1L] <- part$usc_distrib_boot
          sample_idx <- sample_idx + n_chunk
        }
      } else {
        part <- reduced_chunk_eval(seq_len(actual_num_boot))
        u_sum <- u_sum + part$u_sum
        u_sq <- u_sq + part$u_sq
        usc_distrib[, , seq_len(actual_num_boot) + 1L] <- part$usc_distrib_boot
      }
    } else {
      if (parallel_cfg$enabled) {
        chunks <- .plsrri_split_indices(actual_num_boot, parallel_cfg$workers)
        boot_parts <- .plsrri_parallel_lapply(
          chunks,
          function(cols) lapply(cols, task_boot_sample),
          parallel_cfg
        )

        sample_idx <- 1L
        for (chunk in boot_parts) {
          for (sample in chunk) {
            u_boot_scaled <- sample$u_boot_scaled
            u_sum <- u_sum + u_boot_scaled
            u_sq <- u_sq + u_boot_scaled^2
            usc_distrib[, , sample_idx + 1L] <- sample$tmp_orig_usc
            sample_idx <- sample_idx + 1L
          }
        }
      } else {
        if (progress) {
          pb <- cli::cli_progress_bar("Bootstrap test", total = actual_num_boot)
        }

        for (b in seq_len(actual_num_boot)) {
          sample <- task_boot_sample(b)
          u_boot_scaled <- sample$u_boot_scaled
          u_sum <- u_sum + u_boot_scaled
          u_sq <- u_sq + u_boot_scaled^2
          usc_distrib[, , b + 1L] <- sample$tmp_orig_usc

          if (progress) {
            cli::cli_progress_update(id = pb)
          }
        }

        if (progress) {
          cli::cli_progress_done(id = pb)
        }
      }
    }
  } else {

    if (progress) {
      pb <- cli::cli_progress_bar("Bootstrap test", total = actual_num_boot)
    }

    for (b in seq_len(actual_num_boot)) {
    datamat_reorder <- bootsamp[, b]
    behavdata_reorder <- NULL
    datamat_reorder_4beh <- NULL

    if (!is.null(bootsamp_4beh)) {
      behavdata_reorder <- bootsamp_4beh[, b]
      datamat_reorder_4beh <- bootsamp_4beh[, b]
    }

    # MATLAB "badbeh" check: if any condition-by-behavior SD is 0 within a
    # bootstrap sample, re-draw that bootstrap order (behavior/multiblock only).
    if (method %in% c(3L, 4L, 5L, 6L) && !is_boot_samples_all &&
        boot_type == "strat" && !is.null(behavdata_reorder)) {
      repeat {
        needs_resample <- FALSE
        badbehav_by_group <- vector("list", num_groups)

        for (g in seq_len(num_groups)) {
          stdmat <- matrix(NA_real_, nrow = k, ncol = ncol(stacked_behavdata))
          for (c in seq_len(k)) {
            pos <- cond_pos[[g]][[c]]
            vv <- stacked_behavdata[behavdata_reorder[pos], , drop = FALSE]
            stdmat[c, ] <- apply(vv, 2, stats::sd)
          }

          is_zero <- (stdmat == 0)
          if (any(is_zero, na.rm = TRUE)) {
            needs_resample <- TRUE
            badbehav <- matrix(0L, nrow = k, ncol = ncol(stacked_behavdata))

            idx <- which(is_zero, arr.ind = TRUE)
            for (i in seq_len(nrow(idx))) {
              badbehav[idx[i, 1], idx[i, 2]] <- badbehav[idx[i, 1], idx[i, 2]] + 1L
            }

            badbehav_by_group[[g]] <- badbehav
          }
        }

        if (!needs_resample) break

        countnewtotal <- countnewtotal + 1L
        if (countnewtotal > actual_num_boot) {
          # MATLAB: give up after too many attempts.
          break
        }

        # Persist the badbeh matrices for this resample attempt
        for (g in seq_len(num_groups)) {
          if (!is.null(badbehav_by_group[[g]])) {
            if (is.null(badbeh[[g]])) badbeh[[g]] <- vector("list", 0L)
            badbeh[[g]][[countnewtotal]] <- badbehav_by_group[[g]]
          }
        }

        # Re-draw a single bootstrap order for the behavior block.
        new_order <- pls_boot_order(
          num_subj_lst = num_subj_lst,
          num_cond = num_cond,
          num_boot = 1L,
          bscan = if (method %in% c(4L, 6L)) bscan else seq_len(num_cond),
          incl_seq = incl_seq,
          boot_type = boot_type
        )[, 1]

        bootsamp_4beh[, b] <- new_order
        behavdata_reorder <- new_order
        datamat_reorder_4beh <- new_order

        # For behavior PLS (methods 3/5), keep datamat and behav reorder aligned.
        if (method %in% c(3L, 5L)) {
          bootsamp[, b] <- new_order
          datamat_reorder <- new_order
        }
      }
    }

    # Apply bootstrap reordering
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
      datamat_reorder_4beh = datamat_reorder_4beh,
      compute_smeanmat = isTRUE(method %in% c(1L, 4L) ||
                                  (nonrotated_boot && method %in% c(2L, 6L)))
    )

    datamatsvd_boot <- covcor$datamatsvd

    # Compute scaled bootstrap u (u*s) with LV alignment
    v_boot_scaled <- NULL
    if (isTRUE(nonrotated_boot)) {
      # Natasha's non-rotated bootstrap: do not re-SVD. Instead project the
      # observed LV patterns onto the bootstrapped cross-block matrix.
      #
      # MATLAB: u_p = datamatsvd' * v; v_p = datamatsvd * u;
      u_boot_scaled <- crossprod(datamatsvd_boot, observed_v)
      v_boot_scaled <- datamatsvd_boot %*% observed_u
    } else if (method %in% c(2L, 5L, 6L)) {
      crossblock <- crossprod(stacked_designdata, datamatsvd_boot)
      u_boot_scaled <- t(crossblock)
    } else {
      r <- nrow(datamatsvd_boot)
      c <- ncol(datamatsvd_boot)
      n_keep <- min(n_lv, min(r, c))

      if (r <= c) {
        svd_result <- svd(t(datamatsvd_boot), nu = n_keep, nv = n_keep)
        pu <- svd_result$u
        pv <- svd_result$v
      } else {
        svd_result <- svd(datamatsvd_boot, nu = n_keep, nv = n_keep)
        pv <- svd_result$u
        pu <- svd_result$v
      }

      d <- svd_result$d[seq_len(n_keep)]
      pv <- pv[, seq_len(n_keep), drop = FALSE]
      pu <- pu[, seq_len(n_keep), drop = FALSE]

      rotatemat <- pls_bootprocrust(observed_v[, seq_len(n_keep), drop = FALSE], pv)
      pu_scaled <- pu %*% diag(d, nrow = n_keep) %*% rotatemat
      pv_scaled <- pv %*% diag(d, nrow = n_keep) %*% rotatemat
      canon <- canonicalize_bootstrap_svd(pu_scaled, pv_scaled, d)
      pu_scaled <- canon$u_scaled
      pv_scaled <- canon$v_scaled

      u_boot_scaled <- matrix(0, nrow = n_features, ncol = n_lv)
      u_boot_scaled[, seq_len(n_keep)] <- pu_scaled

      v_boot_scaled <- matrix(0, nrow = nrow(observed_v), ncol = n_lv)
      v_boot_scaled[, seq_len(n_keep)] <- pv_scaled
    }

    # Accumulate
    u_sum <- u_sum + u_boot_scaled
    u_sq <- u_sq + u_boot_scaled^2

    # --- Record distrib / Tdistrib for CIs (MATLAB) ---
    if (isTRUE(nonrotated_boot) && method %in% c(1L, 2L, 4L, 6L)) {
      # Natasha's non-rotated bootstrap records task distributions using the
      # mean-centered stacked_smeanmat for methods 1/2/4/6.
      stacked_smeanmat_boot <- covcor$stacked_smeanmat
      tmp_usc2 <- stacked_smeanmat_boot %*% normalize_rows(u_boot_scaled, margin = 2L)

      tmp_orig_usc <- NULL
      first <- 1L
      last <- 0L
      for (g in seq_len(num_groups)) {
        n <- if (!is_ssb) as.integer(num_subj_lst[g]) else as.integer(num_subj_lst[[g]])
        last <- last + group_n_rows[g]
        tmp_orig_usc <- rbind(
          tmp_orig_usc,
          if (!is_ssb) {
            pls_task_mean(tmp_usc2[first:last, , drop = FALSE], n)
          } else {
            pls_task_mean_ssb(tmp_usc2[first:last, , drop = FALSE], n)
          }
        )
        first <- last + 1L
      }

      if (method %in% c(4L, 6L)) {
        task_distrib[, , b + 1L] <- tmp_orig_usc
      } else {
        usc_distrib[, , b + 1L] <- tmp_orig_usc
      }
    } else if (method %in% c(1L, 4L)) {
      # Task brain score distribution (mean-centered)
      stacked_smeanmat_boot <- covcor$stacked_smeanmat
      tmp_usc2 <- stacked_smeanmat_boot %*% normalize_rows(u_boot_scaled, margin = 2L)

      tmp_orig_usc <- NULL
      first <- 1L
      last <- 0L
      for (g in seq_len(num_groups)) {
        n <- if (!is_ssb) as.integer(num_subj_lst[g]) else as.integer(num_subj_lst[[g]])
        last <- last + group_n_rows[g]
        tmp_orig_usc <- rbind(
          tmp_orig_usc,
          if (!is_ssb) {
            pls_task_mean(tmp_usc2[first:last, , drop = FALSE], n)
          } else {
            pls_task_mean_ssb(tmp_usc2[first:last, , drop = FALSE], n)
          }
        )
        first <- last + 1L
      }

      if (method == 4L) {
        task_distrib[, , b + 1L] <- tmp_orig_usc
      } else {
        usc_distrib[, , b + 1L] <- tmp_orig_usc
      }
    } else if (method %in% c(2L, 6L)) {
      # Task brain score distribution (non-rotated): apply boot saliences to
      # the original stacked datamat (MATLAB)
      tmp_usc <- stacked_datamat %*% normalize_rows(u_boot_scaled, margin = 2L)

      tmp_orig_usc <- NULL
      first <- 1L
      last <- 0L
      for (g in seq_len(num_groups)) {
        n <- if (!is_ssb) as.integer(num_subj_lst[g]) else as.integer(num_subj_lst[[g]])
        last <- last + group_n_rows[g]
        tmp_orig_usc <- rbind(
          tmp_orig_usc,
          if (!is_ssb) {
            pls_task_mean(tmp_usc[first:last, , drop = FALSE], n)
          } else {
            pls_task_mean_ssb(tmp_usc[first:last, , drop = FALSE], n)
          }
        )
        first <- last + 1L
      }

      if (method == 6L) {
        task_distrib[, , b + 1L] <- tmp_orig_usc
      } else {
        usc_distrib[, , b + 1L] <- tmp_orig_usc
      }
    }

    if (method %in% c(3L, 4L, 5L, 6L)) {
      idx_data <- datamat_reorder_4beh[row_idx]
      idx_behav <- behavdata_reorder[row_idx]
      data_p <- stacked_datamat[idx_data, , drop = FALSE]
      behav_p <- stacked_behavdata[idx_behav, , drop = FALSE]

      if (method %in% c(2L, 5L, 6L)) {
        brainlv_boot <- normalize_rows(u_boot_scaled, margin = 2L)
        behavlv_boot <- stacked_designdata
      } else {
        brainlv_boot <- normalize_rows(u_boot_scaled, margin = 2L)
        behavlv_boot <- normalize_rows(v_boot_scaled, margin = 2L)
      }

      bcorr <- pls_get_behavscores(
        stacked_datamat = data_p,
        stacked_behavdata = behav_p,
        brainlv = brainlv_boot,
        behavlv = behavlv_boot,
        num_cond = kk,
        num_subj_lst = num_subj_lst_4beh,
        cormode = cormode
      )$lvcorrs

      corr_distrib[, , b + 1L] <- bcorr
    }

      if (progress) {
        cli::cli_progress_update(id = pb)
      }
    }

    if (progress) {
      cli::cli_progress_done(id = pb)
    }
  }

    # Compute standard error (MATLAB-style)
  if (isTRUE(nonrotated_boot)) {
    denom_n <- actual_num_boot
    denom_var <- actual_num_boot - 1L
  } else {
    denom_n <- if (method == 1L) actual_num_boot else (actual_num_boot + 1L)
    denom_var <- if (method == 1L) (actual_num_boot - 1L) else actual_num_boot
  }

  u_sum2 <- (u_sum^2) / denom_n
  u_se <- sqrt(abs(u_sq - u_sum2) / denom_var)

  # Replace non-positive SE with 1 (avoid Inf BSR)
  bad_se <- which(u_se <= 0 | !is.finite(u_se))
  if (length(bad_se) > 0) {
    u_se[bad_se] <- 1
  }

  # Bootstrap ratio
  if (method %in% c(2L, 5L, 6L)) {
    if (isTRUE(nonrotated_boot)) {
      original_u <- observed_u %*% diag(observed_s, nrow = length(observed_s))
      compare_u <- original_u / u_se
    } else {
      compare_u <- observed_u / u_se
    }
  } else {
    original_u <- observed_u %*% diag(observed_s, nrow = length(observed_s))
    compare_u <- original_u / u_se
  }
  if (length(bad_se) > 0) {
    compare_u[bad_se] <- 0
  }
  compare_u[!is.finite(compare_u)] <- 0

  # --- Distribution-based confidence intervals (MATLAB rri_distrib) ---
  ll <- 100 - clim
  ul <- clim
  climNi <- 0.5 * (1 - (clim * 0.01))

  prop <- NULL
  if (method %in% c(1L, 2L)) {
    ci <- pls_distrib_ci(usc_distrib, ll = ll, ul = ul,
                         num_boot = actual_num_boot, climNi = climNi, orig = orig_usc)
    llusc <- ci$ll
    ulusc <- ci$ul
    prop <- ci$prop
    llusc_adj <- ci$ll_adj
    ulusc_adj <- ci$ul_adj
  } else if (method %in% c(3L, 4L, 5L, 6L)) {
    ci <- pls_distrib_ci(corr_distrib, ll = ll, ul = ul,
                         num_boot = actual_num_boot, climNi = climNi, orig = orig_corr)
    llcorr <- ci$ll
    ulcorr <- ci$ul
    prop <- ci$prop
    llcorr_adj <- ci$ll_adj
    ulcorr_adj <- ci$ul_adj

    if (method %in% c(4L, 6L)) {
      tci <- pls_distrib_ci(task_distrib, ll = ll, ul = ul,
                            num_boot = actual_num_boot, climNi = climNi, orig = orig_usc)
      llusc <- tci$ll
      ulusc <- tci$ul
      Tprop <- tci$prop
      llusc_adj <- tci$ll_adj
      ulusc_adj <- tci$ul_adj
    }
  }

  new_pls_boot_result(
    num_boot = actual_num_boot,
    boot_type = boot_type,
    compare_u = compare_u,
    u_se = u_se,
    clim = clim,
    bootsamp = bootsamp,
    bootsamp_4beh = bootsamp_4beh,
    distrib = if (method %in% c(1L, 2L)) usc_distrib else corr_distrib,
    prop = prop,
    usc2 = if (method %in% c(1L, 2L)) usc2 else NULL,
    orig_usc = if (method %in% c(1L, 2L, 4L, 6L)) orig_usc else NULL,
    ulusc = ulusc,
    llusc = llusc,
    ulusc_adj = ulusc_adj,
    llusc_adj = llusc_adj,
    orig_corr = orig_corr,
    ulcorr = ulcorr,
    llcorr = llcorr,
    ulcorr_adj = ulcorr_adj,
    llcorr_adj = llcorr_adj,
    Tdistrib = task_distrib,
    Tprop = Tprop,
    nonrotated_boot = nonrotated_boot,
    num_LowVariability_behav_boots = num_LowVariability_behav_boots,
    badbeh = badbeh,
    countnewtotal = countnewtotal
  )
}
