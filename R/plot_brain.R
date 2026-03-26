#' Brain Slice Plots for PLS
#'
#' @description
#' Functions for visualizing brain maps (saliences, BSR) using neuroim2's
#' plotting infrastructure. This provides thin wrappers around
#' `neuroim2::plot_montage`, `neuroim2::plot_overlay`, and `neuroim2::plot_ortho`.
#'
#' @name plot-brain
NULL

#' Plot Brain Salience or BSR Map
#'
#' @description
#' Creates brain slice plots for PLS saliences or bootstrap ratios.
#' Uses neuroim2's plotting functions for visualization.
#'
#' @param x A `pls_result` object
#' @param lv Latent variable to plot (default 1)
#' @param what What to plot: "salience" or "bsr"
#' @param lag Optional lag label to plot for voxel×lag feature layouts (e.g., 0).
#' @param threshold Threshold for BSR (e.g., 3 for |BSR| > 3)
#' @param view View type: "montage" (default), "ortho", or "overlay"
#' @param background For "overlay" view: a NeuroVol to use as background.
#'   If NULL and overlay is requested, falls back to montage.
#' @param slices Slice indices (NULL = auto, passed to neuroim2)
#' @param along Axis for slicing (3 = axial, 2 = coronal, 1 = sagittal)
#' @param cmap Color map name (default "inferno" for statistical maps)
#' @param ncol Number of columns for montage layout
#' @param title Plot title (NULL = auto-generate)
#' @param ... Additional arguments passed to neuroim2 plotting functions
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- quick_pls(list(d1, d2), c(20, 18), 3, nboot = 100)
#' plot_brain(result, lv = 1, what = "bsr", threshold = 3)
#' plot_brain(result, lv = 1, what = "bsr", view = "ortho")
#' }
plot_brain <- function(x,
                        lv = 1,
                        what = "bsr",
                        lag = NULL,
                        threshold = NULL,
                        view = "montage",
                        background = NULL,
                        slices = NULL,
                        along = 3L,
                        cmap = "inferno",
                        ncol = 6L,
                        title = NULL,
                        ...) {

  UseMethod("plot_brain")
}

#' @export
plot_brain.pls_result <- function(x,
                                   lv = 1,
                                   what = "bsr",
                                   lag = NULL,
                                   threshold = NULL,
                                   view = "montage",
                                   background = NULL,
                                   slices = NULL,
                                   along = 3L,
                                   cmap = "inferno",
                                   ncol = 6L,
                                   title = NULL,
                                   ...) {

  what <- match.arg(what, c("salience", "bsr"))
  view <- match.arg(view, c("montage", "ortho", "overlay"))

  if (is.null(x$mask)) {
    stop("Brain plotting requires a mask. Use pls_spec(mask = ...) to set one.")
  }

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for brain plotting")
  }

  # Get the data
  if (what == "bsr") {
    if (is.null(x$boot_result)) {
      stop("BSR requires bootstrap. Run with num_boot > 0.")
    }
    values <- bsr(x, lv = lv)
    default_title <- sprintf("Bootstrap Ratio (LV%d)", lv)
	  } else {
	    values <- salience(x, lv = lv)
	    default_title <- sprintf("Salience (LV%d)", lv)
	  }

	  # Unpack single LV to a numeric vector
	  if (is.matrix(values) && ncol(values) == 1) {
	    values <- values[, 1]
	  } else if (is.matrix(values) && ncol(values) > 1) {
	    stop("plot_brain currently supports a single LV at a time", call. = FALSE)
	  }

	  # Handle voxel × lag layouts (event-related)
	  layout <- x$feature_layout
	  if (is.list(layout) && identical(layout$kind, "voxel_lag") && !is.null(layout$n_lags) && layout$n_lags > 1) {
	    values <- .slice_voxel_lag(values, layout = layout, lag = lag)
	    lag_label <- attr(values, "lag_label")
	    if (!is.null(lag_label)) {
	      default_title <- sprintf("%s, lag %s", default_title, lag_label)
	    }
	  }

	  # Apply threshold
	  if (!is.null(threshold) && what == "bsr") {
	    values[abs(values) < threshold] <- NA
	    default_title <- sprintf("%s, |BSR| > %g", default_title, threshold)
	  }

  # Convert to NeuroVol
  mask <- x$mask
  vol <- .values_to_neurovol(values, mask)

  # Create title
  if (is.null(title)) {
    title <- default_title
  }

  # Dispatch to appropriate neuroim2 plotting function
  if (view == "montage") {
    p <- neuroim2::plot_montage(
      vol,
      zlevels = slices,
      along = along,
      cmap = cmap,
      ncol = ncol,
      title = title,
      ...
    )
  } else if (view == "ortho") {
    # For ortho, slices should be a single coordinate
    coord <- if (!is.null(slices) && length(slices) == 3) {
      slices
    } else {
      NULL  # Let neuroim2 pick the center
    }
    p <- neuroim2::plot_ortho(
      vol,
      coord = coord,
      cmap = cmap,
      ...
    )
  } else if (view == "overlay") {
    if (is.null(background)) {
      warning("No background provided for overlay view, using montage instead")
      p <- neuroim2::plot_montage(
        vol,
        zlevels = slices,
        along = along,
        cmap = cmap,
        ncol = ncol,
        title = title,
        ...
      )
    } else {
      p <- neuroim2::plot_overlay(
        bgvol = background,
        overlay = vol,
        zlevels = slices,
        along = along,
        ov_cmap = cmap,
        ov_thresh = threshold %||% 0,
        ncol = ncol,
        title = title,
        ...
      )
    }
  }

  p
}

#' Convert Values Vector to NeuroVol
#'
#' @description
#' Helper to convert a vector of values (in mask order) to a NeuroVol.
#'
#' @param values Numeric vector of values

#' @param mask A NeuroVol mask
#'
#' @return A NeuroVol
#' @keywords internal
.values_to_neurovol <- function(values, mask) {
  values <- as.numeric(values)
  mask_arr <- as.array(mask) > 0
  n_mask <- sum(mask_arr)
  if (length(values) != n_mask) {
    stop("values length (", length(values), ") does not match mask voxels (", n_mask, ")", call. = FALSE)
  }
  vol_array <- array(NA_real_, dim = dim(mask))
  vol_array[mask_arr] <- values
  neuroim2::NeuroVol(vol_array, neuroim2::space(mask))
}

.slice_voxel_lag <- function(values, layout, lag = NULL) {
  stopifnot(is.numeric(values))

  n_vox <- suppressWarnings(as.integer(layout$n_voxels))
  n_lag <- suppressWarnings(as.integer(layout$n_lags))
  if (is.na(n_vox) || is.na(n_lag) || n_vox < 1L || n_lag < 1L) {
    stop("Invalid feature_layout for voxel_lag slicing", call. = FALSE)
  }
  if (length(values) != n_vox * n_lag) {
    stop("Expected ", n_vox * n_lag, " features (voxels \u00d7 lags) but got ", length(values), call. = FALSE)
  }

  lag_labels <- layout$lag_labels
  if (is.null(lag_labels)) lag_labels <- seq_len(n_lag) - 1L
  lag_labels <- as.integer(lag_labels)

  if (is.null(lag)) lag <- lag_labels[1]
  lag <- as.integer(lag)[1]
  pos <- match(lag, lag_labels)
  if (is.na(pos)) {
    stop("Unknown lag: ", lag, ". Available lags: ", paste(lag_labels, collapse = ", "), call. = FALSE)
  }

  idx <- pos + (seq_len(n_vox) - 1L) * n_lag
  out <- values[idx]
  attr(out, "lag_label") <- lag
  out
}

#' Plot Brain Overlay
#'
#' @description
#' Convenience function for overlaying PLS results on an anatomical background.
#' This is a thin wrapper around `neuroim2::plot_overlay`.
#'
#' @param x A `pls_result` object
#' @param background A NeuroVol to use as the anatomical background
#' @param lv Latent variable to plot (default 1)
#' @param what What to plot: "salience" or "bsr"
#' @param lag Optional lag label to plot for voxel×lag feature layouts (e.g., 0).
#' @param threshold Threshold for overlay (e.g., 3 for |BSR| > 3)
#' @param slices Slice indices (NULL = auto)
#' @param along Axis for slicing (3 = axial, 2 = coronal, 1 = sagittal)
#' @param ov_cmap Color map for overlay (default "inferno")
#' @param ov_alpha Overlay transparency (default 0.7)
#' @param ncol Number of columns
#' @param title Plot title
#' @param ... Additional arguments passed to `neuroim2::plot_overlay`
#'
#' @return A grid of ggplot panels
#' @export
plot_brain_overlay <- function(x,
                                background,
                                lv = 1,
                                what = "bsr",
                                lag = NULL,
                                threshold = 2,
                                slices = NULL,
                                along = 3L,
                                ov_cmap = "inferno",
                                ov_alpha = 0.7,
                                ncol = 3L,
                                title = NULL,
                                ...) {

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for brain plotting")
  }

  what <- match.arg(what, c("salience", "bsr"))

  if (is.null(x$mask)) {
    stop("Brain plotting requires a mask.")
  }

  # Get values
  if (what == "bsr") {
    if (is.null(x$boot_result)) {
      stop("BSR requires bootstrap. Run with num_boot > 0.")
    }
    values <- bsr(x, lv = lv)
    default_title <- sprintf("Bootstrap Ratio (LV%d)", lv)
	  } else {
	    values <- salience(x, lv = lv)
	    default_title <- sprintf("Salience (LV%d)", lv)
	  }

	  if (is.matrix(values) && ncol(values) == 1) {
	    values <- values[, 1]
	  }

	  layout <- x$feature_layout
	  if (is.list(layout) && identical(layout$kind, "voxel_lag") && !is.null(layout$n_lags) && layout$n_lags > 1) {
	    values <- .slice_voxel_lag(values, layout = layout, lag = lag)
	    lag_label <- attr(values, "lag_label")
	    if (!is.null(lag_label)) default_title <- sprintf("%s, lag %s", default_title, lag_label)
	  }
	
	  # Convert to NeuroVol
	  overlay_vol <- .values_to_neurovol(values, x$mask)

  if (is.null(title)) {
    title <- default_title
  }

  neuroim2::plot_overlay(
    bgvol = background,
    overlay = overlay_vol,
    zlevels = slices,
    along = along,
    ov_cmap = ov_cmap,
    ov_thresh = threshold,
    ov_alpha = ov_alpha,
    ncol = ncol,
    title = title,
    ...
  )
}

#' Plot Orthogonal Brain Views
#'
#' @description
#' Convenience function for orthogonal (3-plane) visualization of PLS results.
#' This is a thin wrapper around `neuroim2::plot_ortho`.
#'
#' @param x A `pls_result` object
#' @param lv Latent variable to plot (default 1)
#' @param what What to plot: "salience" or "bsr"
#' @param lag Optional lag label to plot for voxel×lag feature layouts (e.g., 0).
#' @param threshold Threshold for masking (e.g., 3 for |BSR| > 3)
#' @param coord Coordinate for crosshairs (NULL = center of volume)
#' @param unit Coordinate unit: "index" (voxels) or "mm"
#' @param cmap Color map (default "inferno")
#' @param crosshair Show crosshairs (default TRUE)
#' @param annotate Show orientation labels (default TRUE)
#' @param ... Additional arguments passed to `neuroim2::plot_ortho`
#'
#' @return A list of ggplot panels (axial, coronal, sagittal)
#' @export
plot_brain_ortho <- function(x,
                              lv = 1,
                              what = "bsr",
                              lag = NULL,
                              threshold = NULL,
                              coord = NULL,
                              unit = "index",
                              cmap = "inferno",
                              crosshair = TRUE,
                              annotate = TRUE,
                              ...) {

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for brain plotting")
  }

  what <- match.arg(what, c("salience", "bsr"))

  if (is.null(x$mask)) {
    stop("Brain plotting requires a mask.")
  }

  # Get values
  if (what == "bsr") {
    if (is.null(x$boot_result)) {
      stop("BSR requires bootstrap. Run with num_boot > 0.")
    }
    values <- bsr(x, lv = lv)
	  } else {
	    values <- salience(x, lv = lv)
	  }

	  if (is.matrix(values) && ncol(values) == 1) {
	    values <- values[, 1]
	  }

	  layout <- x$feature_layout
	  if (is.list(layout) && identical(layout$kind, "voxel_lag") && !is.null(layout$n_lags) && layout$n_lags > 1) {
	    values <- .slice_voxel_lag(values, layout = layout, lag = lag)
	  }
	
	  # Apply threshold
	  if (!is.null(threshold)) {
	    values[abs(values) < threshold] <- NA
	  }

  # Convert to NeuroVol
  vol <- .values_to_neurovol(values, x$mask)

  neuroim2::plot_ortho(
    vol,
    coord = coord,
    unit = unit,
    cmap = cmap,
    crosshair = crosshair,
    annotate = annotate,
    ...
  )
}

#' Plot Brain Summary
#'
#' @description
#' Creates a summary showing brain maps for all significant LVs.
#' Uses `neuroim2::plot_montage` for each significant LV.
#'
#' @param x A `pls_result` object
#' @param p_threshold P-value threshold for significance (default 0.05)
#' @param bsr_threshold BSR threshold for masking (default 3)
#' @param n_slices Number of slices per LV (default 9)
#' @param lag Optional lag label to plot for voxel×lag feature layouts (e.g., 0).
#' @param ... Additional arguments passed to `neuroim2::plot_montage`
#'
#' @return A list of plots (one per significant LV), or NULL if no significant LVs
#' @export
plot_brain_summary <- function(x,
                                p_threshold = 0.05,
                                bsr_threshold = 3,
                                n_slices = 9,
                                lag = NULL,
                                ...) {

  if (is.null(x$perm_result)) {
    stop("Requires permutation test. Run with num_perm > 0.")
  }

  if (is.null(x$mask)) {
    stop("Brain plotting requires a mask.")
  }

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for brain plotting")
  }

  sig_lvs <- which(x$perm_result$sprob < p_threshold)

 if (length(sig_lvs) == 0) {
    message("No significant LVs at p < ", p_threshold)
    return(invisible(NULL))
  }

	  plots <- lapply(sig_lvs, function(lv) {
	    p_val <- x$perm_result$sprob[lv]
	
	    # Get values and threshold
	    values <- bsr(x, lv = lv)
	    if (is.matrix(values) && ncol(values) == 1) {
	      values <- values[, 1]
	    }
	    layout <- x$feature_layout
	    if (is.list(layout) && identical(layout$kind, "voxel_lag") && !is.null(layout$n_lags) && layout$n_lags > 1) {
	      values <- .slice_voxel_lag(values, layout = layout, lag = lag)
	    }
	    values[abs(values) < bsr_threshold] <- NA

    # Convert to NeuroVol
    vol <- .values_to_neurovol(values, x$mask)

    # Auto-select slices with content
    slices <- .auto_select_slices(vol, n_slices)

    neuroim2::plot_montage(
      vol,
      zlevels = slices,
      title = sprintf("LV%d (p = %.3f, |BSR| > %g)", lv, p_val, bsr_threshold),
      ...
    )
  })

  names(plots) <- paste0("LV", sig_lvs)

  # Optionally combine with patchwork if available
  if (requireNamespace("patchwork", quietly = TRUE) && length(plots) > 1) {
    combined <- patchwork::wrap_plots(plots, ncol = 1)
    return(combined)
  }

  if (length(plots) == 1) {
    return(plots[[1]])
  }

  plots
}

#' Auto-Select Slices with Content
#'
#' @description
#' Selects slices that contain non-NA/non-zero values for optimal visualization.
#'
#' @param vol A NeuroVol
#' @param n_slices Number of slices to select
#' @param along Axis (default 3 = axial)
#'
#' @return Integer vector of slice indices
#' @keywords internal
.auto_select_slices <- function(vol, n_slices = 9, along = 3L) {
  dims <- dim(vol)
  n_total <- dims[along]

  # Get the volume data
  vol_data <- as.array(vol)

  # Sum absolute values across each slice
  slice_activity <- apply(abs(vol_data), along, function(x) sum(!is.na(x) & x != 0))

  active_slices <- which(slice_activity > 0)

  if (length(active_slices) == 0) {
    # Fall back to evenly spaced slices in middle 60%
    return(unique(round(seq(n_total * 0.2, n_total * 0.8, length.out = n_slices))))
  }

  # Select n_slices evenly from active range
  range_start <- min(active_slices)
  range_end <- max(active_slices)

  unique(round(seq(range_start, range_end, length.out = n_slices)))
}

#' Helper: NULL-coalescing operator
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
