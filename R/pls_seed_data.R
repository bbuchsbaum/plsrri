#' Seed (ROI) data extraction helpers
#'
#' @description
#' Internal utilities for computing seed/ROI data matrices used by Seed PLS.
#' These support both voxel-wise features and event-related voxel-by-lag feature
#' layouts (where the 4th NIfTI dimension encodes lags and PLS folds voxel-by-lag
#' into columns in MATLAB-compatible order).
#'
#' @name seed_data_helpers
#' @keywords internal
NULL

.infer_seed_feature_layout <- function(datamat_lst, mask, feature_layout = NULL) {
  if (is.list(feature_layout) && !is.null(feature_layout$kind)) {
    return(feature_layout)
  }

  mask_arr <- as.array(mask)
  if (length(dim(mask_arr)) < 3) stop("mask must be a 3D array/volume", call. = FALSE)
  mask_logical <- mask_arr > 0
  n_vox <- as.integer(sum(mask_logical))
  if (n_vox < 1L) stop("mask contains zero voxels", call. = FALSE)

  if (!is.list(datamat_lst) || length(datamat_lst) == 0 || !all(vapply(datamat_lst, is.matrix, logical(1)))) {
    stop("datamat_lst must be a non-empty list of matrices", call. = FALSE)
  }

  n_cols <- unique(vapply(datamat_lst, ncol, integer(1)))
  if (length(n_cols) != 1L) stop("All group matrices must have the same number of columns", call. = FALSE)
  n_cols <- as.integer(n_cols[1])

  if (n_cols == n_vox) {
    return(list(kind = "voxel", n_voxels = n_vox, n_lags = 1L, lag_labels = 0L, fold_order = "voxel"))
  }

  if (n_cols %% n_vox == 0L) {
    n_lags <- as.integer(n_cols / n_vox)
    return(list(
      kind = "voxel_lag",
      fold_order = "voxel_lag",
      n_voxels = n_vox,
      n_lags = n_lags,
      lag_labels = as.integer(seq_len(n_lags) - 1L)
    ))
  }

  stop(sprintf(
    "Cannot infer feature layout: data has %d columns but mask has %d voxels",
    n_cols, n_vox
  ), call. = FALSE)
}

.seed_roi_indices <- function(mask, labels, roi_ids) {
  mask_arr <- as.array(mask)
  if (length(dim(mask_arr)) < 3) stop("mask must be a 3D array/volume", call. = FALSE)
  mask_logical <- mask_arr > 0

  labels_arr <- as.array(labels)
  if (length(dim(labels_arr)) < 3) stop("labels must be a 3D array/volume", call. = FALSE)
  if (!identical(dim(labels_arr)[1:3], dim(mask_arr)[1:3])) {
    stop("labels and mask must have the same spatial dimensions", call. = FALSE)
  }

  lab_vec <- labels_arr[mask_logical]
  roi_cols <- lapply(roi_ids, function(id) which(lab_vec == id))
  empty <- which(vapply(roi_cols, length, integer(1)) == 0L)
  if (length(empty) > 0L) {
    stop("No voxels found in mask for ROI id(s): ", paste(roi_ids[empty], collapse = ", "), call. = FALSE)
  }
  roi_cols
}

.drop_zero_var_columns <- function(x, tol = 0) {
  if (!is.matrix(x) || ncol(x) == 0) return(x)
  v <- apply(x, 2, stats::var, na.rm = TRUE)
  keep <- is.finite(v) & v > tol
  dropped <- which(!keep)
  if (length(dropped) > 0) {
    cli::cli_alert_warning("Dropping {length(dropped)} seed column(s) with zero/NA variance.")
    x <- x[, keep, drop = FALSE]
  }
  x
}

.seed_colnames <- function(roi_labels, lag_labels = NULL) {
  roi_labels <- as.character(roi_labels)
  if (is.null(lag_labels) || length(lag_labels) <= 1L) {
    return(roi_labels)
  }
  lag_labels <- as.character(lag_labels)
  as.vector(vapply(roi_labels, function(r) paste0(r, "_lag", lag_labels), character(length(lag_labels))))
}

#' Compute Seed/ROI Data Matrix from a Label Volume
#'
#' @description
#' Given voxelwise data matrices and a mask, compute one column per ROI (voxel
#' layout) or one column per ROI-by-lag (voxel-by-lag layout) as the mean within the
#' ROI for each observation (row).
#'
#' @param datamat_lst List of group matrices (rows = observations).
#' @param mask 3D brain mask volume/array. Non-zero values define included voxels.
#' @param labels 3D label volume/array with integer ROI labels (0 = background).
#' @param roi_ids Optional integer vector of ROI IDs to extract (defaults to all non-zero labels).
#' @param roi_labels Optional character labels for ROIs (defaults to `seed_<id>`).
#' @param feature_layout Optional feature layout list (e.g., from `pls_result$feature_layout`).
#' @param na_rm Logical, passed to `rowMeans()`.
#' @param drop_zero_var Logical, drop seed columns with zero/NA variance (common for baseline lag).
#'
#' @return Numeric matrix with nrow = sum(nrow(datamat_lst)).
#' @keywords internal
pls_seed_data_from_labels <- function(datamat_lst,
                                      mask,
                                      labels,
                                      roi_ids = NULL,
                                      roi_labels = NULL,
                                      lags = NULL,
                                      feature_layout = NULL,
                                      na_rm = TRUE,
                                      drop_zero_var = TRUE) {
  layout <- .infer_seed_feature_layout(datamat_lst, mask, feature_layout = feature_layout)

  labels_arr <- as.array(labels)
  mask_arr <- as.array(mask)
  if (length(dim(labels_arr)) < 3 || length(dim(mask_arr)) < 3) {
    stop("mask and labels must be 3D volumes/arrays", call. = FALSE)
  }

  if (is.null(roi_ids)) {
    mask_logical <- mask_arr > 0
    ids <- sort(unique(as.integer(labels_arr[mask_logical])))
    ids <- ids[ids != 0L & !is.na(ids)]
    roi_ids <- ids
  }

  roi_ids <- suppressWarnings(as.integer(roi_ids))
  roi_ids <- roi_ids[!is.na(roi_ids)]
  if (length(roi_ids) == 0L) stop("roi_ids must contain at least one valid ROI id", call. = FALSE)

  if (is.null(roi_labels)) {
    roi_labels <- paste0("seed_", roi_ids)
  }
  roi_labels <- as.character(roi_labels)
  if (length(roi_labels) != length(roi_ids)) {
    stop("roi_labels must have the same length as roi_ids", call. = FALSE)
  }

  roi_cols <- .seed_roi_indices(mask = mask, labels = labels, roi_ids = roi_ids)

  kind <- as.character(layout$kind)[1]
  if (identical(kind, "voxel")) {
    seed_by_group <- lapply(datamat_lst, function(mat) {
      vapply(
        roi_cols,
        function(cols) rowMeans(mat[, cols, drop = FALSE], na.rm = na_rm),
        numeric(nrow(mat))
      )
    })
    seed <- do.call(rbind, seed_by_group)
    colnames(seed) <- roi_labels
    return(.drop_zero_var_columns(seed, tol = 0))
  }

  if (!identical(kind, "voxel_lag")) {
    stop("Unsupported feature layout kind for seed extraction: ", kind, call. = FALSE)
  }

  n_vox <- suppressWarnings(as.integer(layout$n_voxels))
  n_lag <- suppressWarnings(as.integer(layout$n_lags))
  if (is.na(n_vox) || is.na(n_lag) || n_vox < 1L || n_lag < 1L) {
    stop("Invalid voxel_lag layout", call. = FALSE)
  }

  lag_labels <- layout$lag_labels
  if (is.null(lag_labels)) lag_labels <- seq_len(n_lag) - 1L
  lag_labels <- as.integer(lag_labels)

  keep_pos <- seq_len(n_lag)
  if (!is.null(lags)) {
    lags <- suppressWarnings(as.integer(lags))
    lags <- lags[!is.na(lags)]
    if (length(lags) == 0L) stop("lags must contain at least one valid lag label", call. = FALSE)
    keep_pos <- match(lags, lag_labels)
    if (anyNA(keep_pos)) {
      bad <- lags[is.na(keep_pos)]
      stop("Unknown lag label(s): ", paste(bad, collapse = ", "),
           ". Available: ", paste(lag_labels, collapse = ", "), call. = FALSE)
    }
  }

  seed_by_group <- lapply(datamat_lst, function(mat) {
    if (ncol(mat) != n_vox * n_lag) {
      stop(sprintf(
        "Data matrix has %d columns but expected %d (voxels x lags)",
        ncol(mat), n_vox * n_lag
      ), call. = FALSE)
    }

    out <- matrix(NA_real_, nrow = nrow(mat), ncol = length(roi_ids) * length(keep_pos))
    col_idx <- 1L
    for (ri in seq_along(roi_ids)) {
      vox <- roi_cols[[ri]]
      for (li in keep_pos) {
        cols <- (vox - 1L) * n_lag + li
        out[, col_idx] <- rowMeans(mat[, cols, drop = FALSE], na.rm = na_rm)
        col_idx <- col_idx + 1L
      }
    }

    out
  })

  seed <- do.call(rbind, seed_by_group)
  colnames(seed) <- .seed_colnames(roi_labels, lag_labels = lag_labels[keep_pos])
  .drop_zero_var_columns(seed, tol = 0)
}
