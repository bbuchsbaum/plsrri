#' Core PLS Analysis Engine
#'
#' @description
#' Main function that runs PLS analysis on one or more data matrices.
#' Supports all 6 PLS methods from the McIntosh Lab package.
#' Ported from MATLAB pls_analysis.m
#'
#' @name pls-analysis
NULL

#' Run PLS Analysis
#'
#' @description
#' Performs PLS (Partial Least Squares) analysis on neuroimaging or other
#' multivariate data. Supports task PLS, behavior PLS, and multiblock PLS
#' with both rotated and non-rotated variants.
#'
#' @param datamat_lst List of data matrices. Each element is a matrix where
#'   rows are observations (subjects x conditions) and columns are features
#'   (voxels). Each list element represents one group.
#' @param num_subj_lst Integer vector with number of subjects per group.
#' @param num_cond Integer, number of conditions.
#' @param method Integer 1-6 specifying PLS method:
#' \describe{
#'   \item{1}{Mean-Centering Task PLS (default)}
#'   \item{2}{Non-Rotated Task PLS}
#'   \item{3}{Regular Behavior PLS}
#'   \item{4}{Regular Multiblock PLS}
#'   \item{5}{Non-Rotated Behavior PLS}
#'   \item{6}{Non-Rotated Multiblock PLS}
#' }
#' @param num_perm Number of permutations (0 = no permutation test).
#' @param num_boot Number of bootstrap samples (0 = no bootstrap).
#' @param num_split Number of split-half validations (0 = no split-half).
#' @param clim Confidence level for bootstrap (0-100, default 95).
#' @param stacked_behavdata Behavior data matrix (required for methods 3-6).
#'   Rows match stacked datamat, columns are behavior measures.
#' @param stacked_designdata Design contrast matrix (required for methods 2, 5, 6).
#' @param bscan Integer vector of conditions for behavior block in multiblock
#'   PLS (methods 4, 6). Default is all conditions.
#' @param meancentering_type Mean-centering type (0-3):
#' \describe{
#'   \item{0}{Remove within-group mean (default)}
#'   \item{1}{Remove grand condition mean}
#'   \item{2}{Remove grand mean}
#'   \item{3}{Remove all main effects}
#' }
#' @param cormode Correlation mode for behavior PLS (0, 2, 4, or 6):
#' \describe{
#'   \item{0}{Pearson correlation (default)}
#'   \item{2}{Covariance}
#'   \item{4}{Cosine angle}
#'   \item{6}{Dot product}
#' }
#' @param boot_type Bootstrap type: "strat" (stratified, default) or "nonstrat".
#' @param is_struct Logical, TRUE for structure PLS (don't permute conditions).
#' @param permsamp Optional permutation reordering matrix (`total_rows x num_perm`).
#'   For methods 3-6 this is treated as behavior-block permutation order.
#' @param Tpermsamp Optional task-block permutation matrix (`total_rows x num_perm`)
#'   for multiblock methods (4, 6).
#' @param Bpermsamp Optional behavior-block permutation matrix (`total_rows x num_perm`)
#'   for multiblock/behavior methods.
#' @param bootsamp Optional bootstrap reordering matrix (`total_rows x num_boot`).
#' @param bootsamp_4beh Optional behavior bootstrap matrix (`total_rows x num_boot`)
#'   for behavior/multiblock methods.
#' @param progress Logical, show progress messages.
#'
#' @return A `pls_result` object containing:
#' \describe{
#'   \item{method}{Integer, the PLS method used}
#'   \item{u}{Salience matrix (voxels x LVs)}
#'   \item{s}{Singular values}
#'   \item{v}{Design/behavior loadings}
#'   \item{usc}{Brain scores}
#'   \item{vsc}{Design/behavior scores}
#'   \item{perm_result}{Permutation test results (if num_perm > 0)}
#'   \item{boot_result}{Bootstrap results (if num_boot > 0)}
#'   \item{splithalf_result}{Split-half results (if num_split > 0)}
#' }
#'
#' @export
#'
#' @examples
#' # Simple task PLS with 2 groups, 3 conditions
#' set.seed(42)
#' datamat1 <- matrix(rnorm(20 * 3 * 100), 60, 100)  # 20 subj x 3 cond
#' datamat2 <- matrix(rnorm(15 * 3 * 100), 45, 100)  # 15 subj x 3 cond
#'
#' result <- pls_analysis(
#'   datamat_lst = list(datamat1, datamat2),
#'   num_subj_lst = c(20, 15),
#'   num_cond = 3,
#'   method = 1
#' )
pls_analysis <- function(datamat_lst,
                          num_subj_lst,
                          num_cond,
                          method = 1L,
                          num_perm = 0L,
                          num_boot = 0L,
                          num_split = 0L,
                          clim = 95,
                          stacked_behavdata = NULL,
                          stacked_designdata = NULL,
                          bscan = NULL,
                          meancentering_type = 0L,
                          cormode = 0L,
                          boot_type = "strat",
                          is_struct = FALSE,
                          permsamp = NULL,
                          Tpermsamp = NULL,
                          Bpermsamp = NULL,
                          bootsamp = NULL,
                          bootsamp_4beh = NULL,
                          progress = TRUE) {

  # --- Input Validation ---
  assert_that(is.list(datamat_lst), length(datamat_lst) >= 1)
  assert_that(is.count(num_cond))
  assert_that(method %in% 1:6)
  assert_that(num_perm >= 0)
  assert_that(num_boot >= 0)
  assert_that(clim >= 0, clim <= 100)
  assert_that(meancentering_type %in% 0:3)
  assert_that(cormode %in% c(0L, 2L, 4L, 6L))
  assert_that(boot_type %in% c("strat", "nonstrat"))

  # MATLAB behavior: requesting split-half (num_split > 0) automatically
  # enables Natasha's non-rotated bootstrap mode.
  nonrotated_boot <- isTRUE(num_split > 0L)

  num_groups <- length(datamat_lst)
  if (!(is.numeric(num_subj_lst) || is.list(num_subj_lst))) {
    stop("num_subj_lst must be a numeric vector (balanced) or list (ssb)")
  }
  if (length(num_subj_lst) != num_groups) {
    stop("num_subj_lst must have same length as datamat_lst")
  }

  num_perm <- as.integer(num_perm)
  num_boot <- as.integer(num_boot)
  num_split <- as.integer(num_split)
  method <- as.integer(method)
  k <- as.integer(num_cond)

  is_ssb <- is.list(num_subj_lst)
  if (!is_ssb) {
    num_subj_lst <- as.integer(num_subj_lst)
  } else {
    num_subj_lst <- lapply(num_subj_lst, as.integer)
  }

  if (is_ssb && num_split > 0L) {
    stop("Split-half validation currently requires balanced designs (numeric num_subj_lst).")
  }

  # Default bscan
  if (is.null(bscan)) {
    bscan <- seq_len(num_cond)
  }
  bscan <- as.integer(bscan)

  # Validate group matrix dimensions against num_subj_lst
  for (g in seq_len(num_groups)) {
    if (!is.matrix(datamat_lst[[g]])) {
      stop("Each element of datamat_lst must be a matrix")
    }

    n_rows <- nrow(datamat_lst[[g]])
    if (!is_ssb) {
      expected_rows <- num_subj_lst[g] * k
      if (n_rows != expected_rows) {
        stop(sprintf(
          "Group %d datamat has %d rows but expected %d (subjects=%d x conditions=%d)",
          g, n_rows, expected_rows, num_subj_lst[g], k
        ))
      }
    } else {
      n_vec <- num_subj_lst[[g]]
      if (length(n_vec) != k) {
        stop(sprintf("Group %d num_subj_lst must have length num_cond (%d)", g, k))
      }
      expected_rows <- sum(n_vec)
      if (n_rows != expected_rows) {
        stop(sprintf(
          "Group %d datamat has %d rows but expected %d (sum(num_subj_lst[[%d]]))",
          g, n_rows, expected_rows, g
        ))
      }
    }
  }

  # Method-specific validation
  if (method %in% c(3L, 4L, 5L, 6L)) {
    if (is.null(stacked_behavdata)) {
      stop("stacked_behavdata is required for behavior/multiblock PLS (methods 3-6)")
    }
  }

  # Validate optional resampling matrices are matrix-like/numeric up front.
  if (!is.null(permsamp) && (!is.matrix(permsamp) || !is.numeric(permsamp))) {
    stop("permsamp must be a numeric matrix")
  }
  if (!is.null(Tpermsamp) && (!is.matrix(Tpermsamp) || !is.numeric(Tpermsamp))) {
    stop("Tpermsamp must be a numeric matrix")
  }
  if (!is.null(Bpermsamp) && (!is.matrix(Bpermsamp) || !is.numeric(Bpermsamp))) {
    stop("Bpermsamp must be a numeric matrix")
  }
  if (!is.null(bootsamp) && (!is.matrix(bootsamp) || !is.numeric(bootsamp))) {
    stop("bootsamp must be a numeric matrix")
  }
  if (!is.null(bootsamp_4beh) && (!is.matrix(bootsamp_4beh) || !is.numeric(bootsamp_4beh))) {
    stop("bootsamp_4beh must be a numeric matrix")
  }

  if (method %in% c(2L, 5L, 6L)) {
    if (is.null(stacked_designdata)) {
      stop("stacked_designdata is required for non-rotated PLS (methods 2, 5, 6)")
    }
  }

  # Single condition handling
  if (k == 1L && method %in% c(1L, 2L, 4L, 6L)) {
    meancentering_type <- 1L
    if (progress) {
      cli::cli_alert_info("Single condition Task PLS: meancentering_type set to 1")
    }
  }

  # --- Stack Data Matrices ---
  if (progress) cli::cli_alert_info("Stacking data matrices...")
  stacked_datamat <- do.call(rbind, datamat_lst)
  total_rows <- nrow(stacked_datamat)
  n_features <- ncol(stacked_datamat)

  normalize_resampling_matrix <- function(x, name, expected_rows, expected_cols) {
    if (is.null(x)) {
      return(NULL)
    }
    if (nrow(x) != expected_rows || ncol(x) != expected_cols) {
      stop(sprintf(
        "%s has shape [%d x %d] but expected [%d x %d]",
        name, nrow(x), ncol(x), expected_rows, expected_cols
      ))
    }
    if (any(!is.finite(x))) {
      stop(sprintf("%s must contain finite indices", name))
    }
    storage.mode(x) <- "integer"
    x
  }

  permsamp <- normalize_resampling_matrix(permsamp, "permsamp", total_rows, num_perm)
  Tpermsamp <- normalize_resampling_matrix(Tpermsamp, "Tpermsamp", total_rows, num_perm)
  Bpermsamp <- normalize_resampling_matrix(Bpermsamp, "Bpermsamp", total_rows, num_perm)
  bootsamp <- normalize_resampling_matrix(bootsamp, "bootsamp", total_rows, num_boot)
  bootsamp_4beh <- normalize_resampling_matrix(bootsamp_4beh, "bootsamp_4beh", total_rows, num_boot)

  # MATLAB compatibility: for behavior/multiblock methods, permsamp is the
  # behavior permutation matrix when Bpermsamp is not supplied.
  if (method %in% c(3L, 4L, 5L, 6L) && is.null(Bpermsamp) && !is.null(permsamp)) {
    Bpermsamp <- permsamp
  }

  # Validate behavior data dimensions
  if (method %in% c(3L, 4L, 5L, 6L)) {
    if (nrow(stacked_behavdata) != total_rows) {
      stop(sprintf("stacked_behavdata has %d rows but expected %d",
                   nrow(stacked_behavdata), total_rows))
    }
  }

  # Validate and normalize design data
  if (method %in% c(2L, 5L, 6L)) {
    n_behav <- if (method %in% c(5L, 6L)) ncol(stacked_behavdata) else 0L

    expected_rows_subset <- switch(
      as.character(method),
      "2" = num_groups * k,
      "5" = num_groups * k * n_behav,
      "6" = num_groups * k + num_groups * length(bscan) * n_behav
    )

    # MATLAB accepts a full design matrix for method 6 (all conditions for the
    # behavior block), then subsets to bscan internally.
    expected_rows_full_m6 <- if (method == 6L) {
      num_groups * k + num_groups * k * n_behav
    } else {
      NA_integer_
    }

    if (method == 6L && nrow(stacked_designdata) == expected_rows_full_m6) {
      # Subset to bscan to match rri_get_covcor(..., bscan, ...)
      Ti <- rep(1L, k)
      Bi <- matrix(0L, nrow = n_behav, ncol = k)
      Bi[, bscan] <- 1L
      keep_one_group <- c(Ti, as.vector(Bi))
      keep <- rep(as.logical(keep_one_group), times = num_groups)
      stacked_designdata <- stacked_designdata[keep, , drop = FALSE]
    }

    if (nrow(stacked_designdata) != expected_rows_subset) {
      stop(sprintf("stacked_designdata has %d rows but expected %d",
                   nrow(stacked_designdata), expected_rows_subset))
    }

    # Normalize design data (columns to unit length; MATLAB normalize())
    stacked_designdata <- normalize_rows(stacked_designdata, margin = 2L)
  }

  # --- Calculate Cross-Block Matrix ---
  if (progress) cli::cli_alert_info("Computing covariance/correlation matrix...")

  datamat_reorder <- seq_len(total_rows)
  behavdata_reorder <- if (method %in% c(3:6)) seq_len(total_rows) else NULL

  covcor <- pls_get_covcor(
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
    behavdata_reorder = behavdata_reorder
  )

  datamatsvd <- covcor$datamatsvd
  datamatsvd_unnorm <- covcor$datamatsvd_unnorm
  datamatcorrs_lst <- covcor$datamatcorrs_lst

  # --- SVD or Non-Rotated Computation ---
  if (progress) cli::cli_alert_info("Computing latent variables...")

  if (method %in% c(2L, 5L, 6L)) {
    # Non-rotated PLS: project design onto cross-block (MATLAB parity)
    crossblock <- crossprod(stacked_designdata, datamatsvd)

    if (nonrotated_boot) {
      # MATLAB: u = normalize(crossblock') when nonrotated_boot is enabled.
      u <- normalize_rows(t(crossblock), margin = 2L)
      normalized_u <- normalize_rows(u, margin = 2L)
      lvintercorrs <- crossprod(normalized_u)
    } else {
      u <- t(crossblock)
      # NOTE: MATLAB stores unnormalized u (= crossblock') for standard
      # non-rotated PLS. Scores use normalize(u).
      lvintercorrs <- crossprod(u)
    }
    s <- sqrt(rowSums(crossblock^2))
    v <- stacked_designdata

  } else {
    # Standard SVD
    r <- nrow(datamatsvd)
    c <- ncol(datamatsvd)

    if (r <= c) {
      svd_result <- pls_svd(t(datamatsvd))
      u <- svd_result$u
      s <- svd_result$d
      v <- svd_result$v
    } else {
      svd_result <- pls_svd(datamatsvd)
      v <- svd_result$u
      s <- svd_result$d
      u <- svd_result$v
    }

    lvintercorrs <- NULL
  }

  org_s <- s
  org_v <- v

  # --- Multiblock SSQ Adjustment ---
  if (method %in% c(4L, 6L)) {
    total_s <- sum(datamatsvd_unnorm^2)
    per <- s^2 / sum(s^2)
    org_s <- sqrt(per * total_s)
    org_v <- v %*% diag(org_s, nrow = length(org_s))
  }

  # --- Compute Scores ---
  if (progress) cli::cli_alert_info("Computing scores...")

  vsc <- NULL
  lvcorrs <- NULL
  usc <- NULL
  group_sizes <- if (!is_ssb) num_subj_lst * k else vapply(num_subj_lst, sum, integer(1))
  group_offsets <- c(0L, cumsum(group_sizes[-num_groups]))

  if (method %in% c(1L, 2L)) {
    # Task PLS scores
    if (method == 1L) {
      usc <- stacked_datamat %*% u
    } else {
      usc <- stacked_datamat %*% normalize_rows(u, margin = 2)
    }

    # Expand v to match subject structure
    num_col <- ncol(v)
    vsc <- matrix(0, nrow = total_rows, ncol = num_col)

    for (g in seq_len(num_groups)) {
      offset <- group_offsets[g]
      n_vec <- if (!is_ssb) rep(num_subj_lst[g], k) else num_subj_lst[[g]]

      v_group <- v[((g - 1) * k + 1):(g * k), , drop = FALSE]

      step <- 0L
      for (cond in seq_len(k)) {
        n <- n_vec[cond]
        subj_rows <- (offset + step + 1L):(offset + step + n)
        vsc[subj_rows, ] <- matrix(v_group[cond, ], nrow = n, ncol = num_col, byrow = TRUE)
        step <- step + n
      }
    }

  } else if (method %in% c(3L, 5L)) {
    # Behavior PLS scores (MATLAB rri_get_behavscores)
    behav_scores <- pls_get_behavscores(
      stacked_datamat = stacked_datamat,
      stacked_behavdata = stacked_behavdata,
      brainlv = u,
      behavlv = v,
      num_cond = k,
      num_subj_lst = num_subj_lst,
      cormode = cormode
    )

    usc <- behav_scores$usc
    vsc <- behav_scores$vsc
    lvcorrs <- behav_scores$lvcorrs

  } else if (method %in% c(4L, 6L)) {
    # Multiblock PLS scores (MATLAB parity)
    n_behav <- ncol(stacked_behavdata)
    kk <- length(bscan)

    tb_split <- pls_split_multiblock_v(
      v = v,
      num_groups = num_groups,
      num_cond = k,
      n_behav = n_behav,
      bscan = bscan
    )

    Tv <- tb_split$task_v
    Bv <- tb_split$behav_v

    # Task vsc expansion (as in task PLS)
    num_col <- ncol(Tv)
    Tvsc <- matrix(0, nrow = total_rows, ncol = num_col)

    for (g in seq_len(num_groups)) {
      offset <- group_offsets[g]
      n_vec <- if (!is_ssb) rep(num_subj_lst[g], k) else num_subj_lst[[g]]
      Tv_group <- Tv[((g - 1) * k + 1):(g * k), , drop = FALSE]

      for (cond in seq_len(k)) {
        n <- n_vec[cond]
        start <- offset + sum(n_vec[seq_len(cond - 1L)]) + 1L
        end <- start + n - 1L
        Tvsc[start:end, ] <- matrix(Tv_group[cond, ], nrow = n, ncol = num_col, byrow = TRUE)
      }
    }

    # Row indices for bscan conditions across groups (subject-in-condition order)
    row_idx <- make_row_indices(num_subj_lst, k, condition = bscan)

    # Task brain scores use normalized u
    Tusc <- stacked_datamat %*% normalize_rows(u, margin = 2L)

    num_subj_lst_bscan <- if (!is_ssb) num_subj_lst else lapply(num_subj_lst, function(x) x[bscan])

    behav_scores <- pls_get_behavscores(
      stacked_datamat = stacked_datamat[row_idx, , drop = FALSE],
      stacked_behavdata = stacked_behavdata[row_idx, , drop = FALSE],
      brainlv = u,
      behavlv = Bv,
      num_cond = kk,
      num_subj_lst = num_subj_lst_bscan,
      cormode = cormode
    )

    Busc <- behav_scores$usc
    Bvsc <- behav_scores$vsc
    lvcorrs <- behav_scores$lvcorrs

    usc <- rbind(Tusc, Busc)
    vsc <- rbind(Tvsc, Bvsc)
  }

  # --- Permutation Test ---
  perm_result <- NULL
  if (num_perm > 0) {
    if (progress) cli::cli_alert_info("Running permutation test ({num_perm} permutations)...")

    perm_result <- pls_permutation_test(
      stacked_datamat = stacked_datamat,
      stacked_behavdata = stacked_behavdata,
      stacked_designdata = stacked_designdata,
      num_groups = num_groups,
      num_subj_lst = num_subj_lst,
      num_cond = k,
      method = method,
      num_perm = num_perm,
      observed_s = s,
      observed_v = v,
      org_s = org_s,
      org_v = org_v,
      bscan = bscan,
      meancentering_type = meancentering_type,
      cormode = cormode,
      is_struct = is_struct,
      permsamp = permsamp,
      Tpermsamp = Tpermsamp,
      Bpermsamp = Bpermsamp,
      progress = progress
    )
  }

  # --- Bootstrap Test ---
  boot_result <- NULL
  if (num_boot > 0) {
    if (progress) cli::cli_alert_info("Running bootstrap test ({num_boot} samples)...")

    boot_result <- pls_bootstrap_test(
      stacked_datamat = stacked_datamat,
      stacked_behavdata = stacked_behavdata,
      stacked_designdata = stacked_designdata,
      num_groups = num_groups,
      num_subj_lst = num_subj_lst,
      num_cond = k,
      method = method,
      num_boot = num_boot,
      observed_u = u,
      observed_v = v,
      observed_s = s,
      observed_lvcorrs = lvcorrs,
      bscan = bscan,
      meancentering_type = meancentering_type,
      cormode = cormode,
      boot_type = boot_type,
      nonrotated_boot = nonrotated_boot,
      bootsamp = bootsamp,
      bootsamp_4beh = bootsamp_4beh,
      clim = clim,
      progress = progress
    )
  }

  # --- Split-Half Validation ---
  splithalf_result <- NULL
  if (num_split > 0) {
    if (progress) cli::cli_alert_info("Running split-half validation ({num_split} iterations)...")

    splithalf_result <- pls_splithalf_test(
      stacked_datamat = stacked_datamat,
      stacked_behavdata = stacked_behavdata,
      stacked_designdata = stacked_designdata,
      num_groups = num_groups,
      num_subj_lst = num_subj_lst,
      num_cond = k,
      method = method,
      num_split = num_split,
      num_outer_perm = if (num_perm > 0) num_perm + 1L else 0L,
      clim = clim,
      bscan = bscan,
      meancentering_type = meancentering_type,
      cormode = cormode,
      is_struct = is_struct,
      progress = progress
    )
  }

  # --- Build Result Object ---
  if (progress) cli::cli_alert_success("PLS analysis complete")

  result <- new_pls_result(
    method = method,
    u = u,
    s = s,
    v = v,
    usc = usc,
    vsc = vsc,
    datamatcorrs_lst = if (method %in% 3:6) datamatcorrs_lst else NULL,
    lvcorrs = lvcorrs,
    perm_result = perm_result,
    boot_result = boot_result,
    splithalf_result = splithalf_result,
    num_subj_lst = num_subj_lst,
    num_cond = k,
    bscan = if (method %in% c(4L, 6L)) bscan else NULL,
    stacked_designdata = stacked_designdata,
    stacked_behavdata = stacked_behavdata,
    other_input = list(
      meancentering_type = meancentering_type,
      cormode = cormode,
      nonrotated_boot = nonrotated_boot,
      org_s = org_s,
      org_v = org_v
    ),
    is_struct = is_struct
  )

  if (!is.null(lvintercorrs)) {
    result$lvintercorrs <- lvintercorrs
  }

  if (method %in% c(4L, 6L)) {
    result$TBv <- list(tb_split$task_v, tb_split$behav_v)
    result$TBusc <- list(Tusc, Busc)
    result$TBvsc <- list(Tvsc, Bvsc)
  }

  result
}
