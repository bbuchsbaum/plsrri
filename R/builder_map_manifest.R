#' Builder API: Add Subjects from Map Manifest
#'
#' @description
#' Helpers for constructing PLS-ready data matrices from manifests where each
#' observation references a single 3D map (or a single selected volume from a
#' 4D image). This is the primary intake path for first-level GLM estimate maps.
#'
#' @name builder-map-manifest
NULL

.prepare_map_manifest_observations <- function(df_norm,
                                               mask,
                                               conditions = NULL,
                                               groups = NULL) {
  assert_that(is.data.frame(df_norm))
  assert_that(inherits(mask, "NeuroVol"))

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for map-manifest inputs", call. = FALSE)
  }

  if (any(!is.na(df_norm$.lag))) {
    stop("Map manifests do not support a lag column; use add_subjects_manifest() for voxel-lag inputs.", call. = FALSE)
  }

  if (is.null(conditions)) {
    conditions <- if (is.factor(df_norm$.condition)) levels(df_norm$.condition) else unique(df_norm$.condition)
  }
  conditions <- as.character(conditions)
  if (!length(conditions)) stop("No conditions found in map manifest", call. = FALSE)

  if (is.null(groups)) {
    groups <- if (is.factor(df_norm$.group)) levels(df_norm$.group) else unique(df_norm$.group)
  }
  groups <- as.character(groups)
  if (!length(groups)) groups <- "all"

  dim_cache <- new.env(parent = emptyenv())
  uniq_files <- unique(df_norm$.file)
  if (any(!file.exists(uniq_files))) {
    missing <- uniq_files[!file.exists(uniq_files)]
    stop("Map manifest contains missing files (showing up to 5): ",
         paste(utils::head(missing, 5), collapse = ", "), call. = FALSE)
  }

  mask_dims <- dim(mask)[1:3]
  if (length(mask_dims) != 3L) stop("mask must be 3D", call. = FALSE)

  file_dims <- lapply(uniq_files, .nifti_header_dims, cache = dim_cache)
  spatial_dims <- lapply(file_dims, function(d) d[1:3])
  ok_space <- vapply(spatial_dims, function(d) identical(as.integer(d), as.integer(mask_dims)), logical(1))
  if (!all(ok_space)) {
    bad <- uniq_files[!ok_space]
    stop("Some files do not match the mask dimensions (showing up to 5): ",
         paste(utils::head(bad, 5), collapse = ", "), call. = FALSE)
  }

  obs_rows <- list()
  for (i in seq_len(nrow(df_norm))) {
    file <- df_norm$.file[[i]]
    dims <- .nifti_header_dims(file, cache = dim_cache)
    vols <- df_norm$.file_vols[[i]]

    volume <- NULL
    if (length(dims) == 4L) {
      if (is.null(vols)) {
        if (as.integer(dims[4]) != 1L) {
          stop("4D files in a map manifest must specify exactly one volume via file[vol] or volume column: ",
               basename(file), call. = FALSE)
        }
        volume <- 1L
      } else {
        if (length(vols) != 1L) {
          stop("Map-manifest rows may reference exactly one 4D volume: ", basename(file), call. = FALSE)
        }
        volume <- as.integer(vols[[1]])
      }
    } else if (!is.null(vols) && length(vols) > 1L) {
      stop("3D files cannot specify multiple volumes: ", basename(file), call. = FALSE)
    }

    obs_rows[[length(obs_rows) + 1L]] <- data.frame(
      group = df_norm$.group[[i]],
      subject = df_norm$.subject[[i]],
      condition = as.character(df_norm$.condition[[i]]),
      file = file,
      volume = if (is.null(volume)) NA_integer_ else volume,
      stringsAsFactors = FALSE
    )
  }

  obs_df <- do.call(rbind, obs_rows)
  key <- paste(obs_df$group, obs_df$subject, obs_df$condition, sep = "\t")
  if (anyDuplicated(key)) {
    dup <- unique(key[duplicated(key)])
    stop("Map manifest contains duplicated (group, subject, condition) observations (showing up to 5): ",
         paste(utils::head(dup, 5), collapse = ", "), call. = FALSE)
  }

  ordered <- .order_manifest_observations(obs_df, groups = groups, conditions = conditions)

  list(
    groups = groups,
    conditions = conditions,
    num_cond = length(conditions),
    num_subj_lst = ordered$num_subj_lst,
    obs_by_group = ordered$obs_by_group
  )
}

#' Add Subjects from a Map Manifest
#'
#' @description
#' Adds subject data to a PLS specification from a manifest where each
#' observation references a single 3D map. This is intended for first-level
#' GLM estimate maps such as condition betas or contrast statistics.
#'
#' @param spec A `pls_spec` object.
#' @param manifest A data.frame or a file path (.csv/.tsv/.rds).
#' @param mask Brain mask (NeuroVol or file path). Required for image ingestion.
#' @param subject_col,condition_col,file_col Column names (or NULL to use defaults).
#' @param group_col Optional group column (default looks for "group").
#' @param volume_col Optional single-volume selector for 4D inputs.
#' @param base_dir Optional base directory for relative file paths.
#'
#' @return Updated `pls_spec` object.
#' @export
add_subjects_map_manifest <- function(spec,
                                      manifest,
                                      mask,
                                      subject_col = NULL,
                                      condition_col = NULL,
                                      file_col = NULL,
                                      group_col = NULL,
                                      volume_col = NULL,
                                      base_dir = NULL) {
  assert_that(inherits(spec, "pls_spec"))

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for map-manifest inputs", call. = FALSE)
  }

  df <- .read_manifest_file(manifest)
  if (!is.data.frame(df) || nrow(df) == 0) stop("manifest must have at least one row", call. = FALSE)

  mask_vol <- mask
  if (is.character(mask)) {
    mask_vol <- neuroim2::read_vol(mask)
  }
  if (!inherits(mask_vol, "NeuroVol")) stop("mask must be a NeuroVol or a NIfTI file path", call. = FALSE)

  df_norm <- .normalize_manifest(
    df,
    subject_col = subject_col,
    condition_col = condition_col,
    file_col = file_col,
    group_col = group_col,
    lag_col = NULL,
    volume_col = volume_col,
    base_dir = base_dir
  )

  prep <- .prepare_map_manifest_observations(df_norm, mask_vol)

  spec$datamat_lst <- prep$obs_by_group
  spec$num_subj_lst <- prep$num_subj_lst
  spec$num_cond <- as.integer(prep$num_cond)
  spec$conditions <- as.character(prep$conditions)
  spec$groups <- as.character(prep$groups)
  spec$mask <- mask_vol
  spec$feature_layout <- list(
    kind = "voxel_map",
    n_voxels = as.integer(sum(as.array(mask_vol) > 0)),
    source = "map_manifest"
  )
  spec$.map_manifest_raw <- TRUE

  spec
}
