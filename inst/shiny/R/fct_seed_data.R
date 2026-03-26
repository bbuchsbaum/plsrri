# Seed Data Helpers
# Pure helpers for Seed PLS UI (no Shiny dependencies)

#' Load an atlas via neuroatlas (GUI helper)
#'
#' @param atlas One of "schaefer", "glasser", "aseg"
#' @param outspace Optional neuroim2::NeuroSpace to resample into
#' @param parcels Schaefer parcels ("100"-"1000")
#' @param networks Schaefer networks ("7" or "17")
#' @param resolution Schaefer resolution ("1" or "2")
#' @return An atlas object from neuroatlas (list with $atlas + metadata)
#' @keywords internal
load_seed_atlas <- function(atlas,
                            outspace = NULL,
                            parcels = "200",
                            networks = "17",
                            resolution = "2") {
  if (!requireNamespace("neuroatlas", quietly = TRUE)) {
    stop("Package 'neuroatlas' is required for atlas-based seed selection.")
  }

  atlas <- as.character(atlas)[1]

  switch(
    atlas,
    schaefer = neuroatlas::get_schaefer_atlas(
      parcels = as.character(parcels)[1],
      networks = as.character(networks)[1],
      resolution = as.character(resolution)[1],
      outspace = outspace,
      smooth = FALSE
    ),
    glasser = neuroatlas::get_glasser_atlas(outspace = outspace),
    aseg = neuroatlas::get_aseg_atlas(outspace = outspace),
    stop("Unknown atlas: ", atlas)
  )
}

#' Compute seed data matrix from atlas ROIs
#'
#' @description
#' Given voxelwise data matrices and a brain mask, compute one column per ROI as
#' the mean across voxels within that ROI for each observation (row).
#'
#' @param datamat_lst List of group matrices (rows = observations, cols = voxels).
#' @param mask A neuroim2::NeuroVol (or array) defining voxel inclusion.
#' @param atlas A neuroim2 volume (or array) with integer ROI labels.
#' @param roi_ids Integer ROI IDs to extract.
#' @param roi_labels Optional character labels for columns (defaults to roi_ids).
#' @param na_rm Logical, passed to rowMeans.
#' @return Numeric matrix with nrow = sum(nrow(datamat_lst)), ncol = length(roi_ids).
#' @keywords internal
compute_seed_data_from_atlas <- function(datamat_lst,
                                        mask,
                                        atlas,
                                        roi_ids,
                                        roi_labels = NULL,
                                        na_rm = TRUE) {
  if (!is.list(datamat_lst) || length(datamat_lst) == 0 || !all(vapply(datamat_lst, is.matrix, logical(1)))) {
    stop("datamat_lst must be a non-empty list of matrices")
  }

  roi_ids <- suppressWarnings(as.integer(roi_ids))
  roi_ids <- roi_ids[!is.na(roi_ids)]
  if (length(roi_ids) == 0) stop("roi_ids must contain at least one valid ROI id")

  mask_arr <- as.array(mask)
  if (length(dim(mask_arr)) < 3) stop("mask must be a 3D array/volume")
  mask_logical <- mask_arr > 0
  n_vox <- sum(mask_logical)
  if (n_vox <= 0) stop("mask contains zero voxels")

  for (g in seq_along(datamat_lst)) {
    if (ncol(datamat_lst[[g]]) != n_vox) {
      stop(sprintf(
        "Group %d: data matrix has %d columns but mask has %d voxels",
        g, ncol(datamat_lst[[g]]), n_vox
      ))
    }
  }

  atlas_arr <- as.array(atlas)
  if (length(dim(atlas_arr)) < 3) stop("atlas must be a 3D array/volume")
  if (!identical(dim(atlas_arr)[1:3], dim(mask_arr)[1:3])) {
    stop("atlas and mask must have the same spatial dimensions")
  }

  atlas_vec <- atlas_arr[mask_logical]

  roi_cols <- lapply(roi_ids, function(id) which(atlas_vec == id))
  empty <- which(vapply(roi_cols, length, integer(1)) == 0)
  if (length(empty) > 0) {
    stop("No voxels found in mask for ROI id(s): ", paste(roi_ids[empty], collapse = ", "))
  }

  seed_by_group <- lapply(datamat_lst, function(mat) {
    vapply(
      roi_cols,
      function(cols) rowMeans(mat[, cols, drop = FALSE], na.rm = na_rm),
      numeric(nrow(mat))
    )
  })

  seed <- do.call(rbind, seed_by_group)

  if (is.null(roi_labels)) {
    roi_labels <- paste0("roi_", roi_ids)
  }
  roi_labels <- as.character(roi_labels)
  if (length(roi_labels) == length(roi_ids)) {
    colnames(seed) <- roi_labels
  }

  seed
}

