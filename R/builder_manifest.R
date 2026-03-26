#' Builder API: Add Subjects from Manifest / GDS
#'
#' @description
#' Helpers for constructing PLS-ready data matrices from higher-level inputs.
#' In particular, this supports event-related fMRI style inputs where the 4th
#' dimension of a NIfTI encodes **lags** (time since onset), and the PLS
#' datamat columns are constructed by folding voxel × lag into a single feature
#' axis (MATLAB-compatible ordering).
#'
#' @name builder-manifest
NULL

.read_manifest_file <- function(x) {
  if (is.data.frame(x)) return(x)
  if (!is.character(x) || length(x) != 1L) {
    stop("manifest must be a data.frame or a single file path", call. = FALSE)
  }
  if (!file.exists(x)) stop("manifest file does not exist: ", x, call. = FALSE)

  ext <- tolower(tools::file_ext(x))
  if (ext %in% c("rds")) return(readRDS(x))
  if (ext %in% c("csv")) return(utils::read.csv(x, stringsAsFactors = FALSE))
  if (ext %in% c("tsv", "txt")) return(utils::read.delim(x, sep = "\t", stringsAsFactors = FALSE))
  stop("Unsupported manifest extension: .", ext, call. = FALSE)
}

.parse_int_spec <- function(x) {
  # Accept "1", "1:8", "1,3,5", "1:4,6:8" (1-based indices).
  x <- gsub("\\s+", "", as.character(x))
  if (!nzchar(x)) return(integer(0))

  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  out <- integer(0)
  for (p in parts) {
    if (!nzchar(p)) next
    if (grepl("^\\d+$", p)) {
      out <- c(out, as.integer(p))
      next
    }
    if (grepl("^\\d+:\\d+$", p)) {
      ab <- strsplit(p, ":", fixed = TRUE)[[1]]
      a <- as.integer(ab[1]); b <- as.integer(ab[2])
      if (is.na(a) || is.na(b)) stop("Invalid volume spec: ", x, call. = FALSE)
      out <- c(out, seq.int(a, b))
      next
    }
    stop("Invalid integer spec: ", p, call. = FALSE)
  }
  as.integer(out)
}

.parse_file_ref <- function(x) {
  # Parse "path/to/file.nii.gz[1:8]" -> list(path, volumes)
  x <- as.character(x)
  m <- regexec("^(.*)\\[([^]]+)\\]$", x)
  reg <- regmatches(x, m)[[1]]
  if (length(reg) == 0) {
    return(list(path = x, volumes = NULL))
  }
  path <- reg[2]
  spec <- reg[3]
  vols <- .parse_int_spec(spec)
  if (!length(vols)) stop("Empty volume selector in file reference: ", x, call. = FALSE)
  list(path = path, volumes = vols)
}

.nifti_header_dims <- function(path, cache) {
  key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (exists(key, envir = cache, inherits = FALSE)) {
    return(get(key, envir = cache, inherits = FALSE))
  }
  hdr <- neuroim2::read_header(path)
  dims <- as.integer(hdr@dims)
  assign(key, dims, envir = cache)
  dims
}

.resolve_manifest_cols <- function(df,
                                  subject_col,
                                  condition_col,
                                  file_col,
                                  group_col = NULL,
                                  lag_col = NULL,
                                  volume_col = NULL) {
  nm <- names(df)
  pick <- function(preferred, fallback) {
    if (!is.null(preferred) && preferred %in% nm) return(preferred)
    if (!is.null(fallback) && fallback %in% nm) return(fallback)
    NULL
  }
  list(
    subject = pick(subject_col, "subject"),
    condition = pick(condition_col, "condition"),
    file = pick(file_col, "file"),
    group = pick(group_col, "group"),
    lag = pick(lag_col, "lag"),
    volume = pick(volume_col, "volume")
  )
}

.normalize_manifest <- function(df,
                               subject_col,
                               condition_col,
                               file_col,
                               group_col = NULL,
                               lag_col = NULL,
                               volume_col = NULL,
                               base_dir = NULL) {
  cols <- .resolve_manifest_cols(
    df,
    subject_col = subject_col,
    condition_col = condition_col,
    file_col = file_col,
    group_col = group_col,
    lag_col = lag_col,
    volume_col = volume_col
  )

  if (is.null(cols$subject) || is.null(cols$condition) || is.null(cols$file)) {
    stop("Manifest must include subject, condition, and file columns", call. = FALSE)
  }

  out <- df
  out$.subject <- as.character(df[[cols$subject]])
  out$.condition <- df[[cols$condition]]
  out$.condition <- if (is.factor(out$.condition)) out$.condition else as.character(out$.condition)
  out$.file_raw <- as.character(df[[cols$file]])
  out$.group <- if (!is.null(cols$group)) df[[cols$group]] else "all"
  out$.group <- if (is.factor(out$.group)) out$.group else as.character(out$.group)

  out$.lag <- if (!is.null(cols$lag)) suppressWarnings(as.integer(df[[cols$lag]])) else NA_integer_

  if (!is.null(cols$volume)) {
    out$.volume_spec <- as.character(df[[cols$volume]])
  } else {
    out$.volume_spec <- NA_character_
  }

  # Resolve file path + optional bracket selector.
  parsed <- lapply(out$.file_raw, .parse_file_ref)
  out$.file <- vapply(parsed, function(x) x$path, character(1))
  out$.file_vols <- lapply(parsed, function(x) x$volumes)

  if (!all(is.na(out$.volume_spec)) && any(vapply(out$.file_vols, function(v) !is.null(v), logical(1)))) {
    stop("Specify volumes either via a 'volume' column OR via file[volspec], not both.", call. = FALSE)
  }

  if (!all(is.na(out$.volume_spec))) {
    out$.file_vols <- lapply(out$.volume_spec, function(v) {
      if (is.na(v) || !nzchar(v)) return(NULL)
      vols <- .parse_int_spec(v)
      if (!length(vols)) stop("Invalid volume spec: ", v, call. = FALSE)
      vols
    })
  }

  # Base dir for relative paths
  if (!is.null(base_dir) && nzchar(base_dir)) {
    base_dir <- normalizePath(base_dir, winslash = "/", mustWork = TRUE)
    out$.file <- vapply(out$.file, function(p) {
      p <- path.expand(p)
      if (!grepl("^(/|[A-Za-z]:)", p) && !startsWith(p, "\\\\")) {
        p <- file.path(base_dir, p)
      }
      normalizePath(p, winslash = "/", mustWork = FALSE)
    }, character(1))
  } else {
    out$.file <- vapply(out$.file, function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE), character(1))
  }

  out
}

.prepare_manifest_observations <- function(df_norm,
                                          mask,
                                          conditions = NULL,
                                          groups = NULL,
                                          require_equal_lags = TRUE) {
  assert_that(is.data.frame(df_norm))
  assert_that(inherits(mask, "NeuroVol"))

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for manifest-based inputs", call. = FALSE)
  }

  # Condition/group levels (preserve factor levels when present)
  if (is.null(conditions)) {
    conditions <- if (is.factor(df_norm$.condition)) levels(df_norm$.condition) else unique(df_norm$.condition)
  }
  conditions <- as.character(conditions)
  if (!length(conditions)) stop("No conditions found in manifest", call. = FALSE)

  if (is.null(groups)) {
    groups <- if (is.factor(df_norm$.group)) levels(df_norm$.group) else unique(df_norm$.group)
  }
  groups <- as.character(groups)
  if (!length(groups)) groups <- "all"

  # NIfTI dims + lag inference
  dim_cache <- new.env(parent = emptyenv())
  uniq_files <- unique(df_norm$.file)
  if (any(!file.exists(uniq_files))) {
    missing <- uniq_files[!file.exists(uniq_files)]
    stop("Manifest contains missing files (showing up to 5): ", paste(utils::head(missing, 5), collapse = ", "), call. = FALSE)
  }

  mask_dims <- dim(mask)[1:3]
  if (length(mask_dims) != 3L) stop("mask must be 3D", call. = FALSE)

  file_dims <- lapply(uniq_files, .nifti_header_dims, cache = dim_cache)
  spatial_dims <- lapply(file_dims, function(d) d[1:3])
  ok_space <- vapply(spatial_dims, function(d) identical(as.integer(d), as.integer(mask_dims)), logical(1))
  if (!all(ok_space)) {
    bad <- uniq_files[!ok_space]
    stop("Some files do not match the mask dimensions (showing up to 5): ", paste(utils::head(bad, 5), collapse = ", "), call. = FALSE)
  }

  # Determine manifest mode: "obs" (1 row per subject×condition) vs "lag" (multiple rows)
  key <- paste(df_norm$.group, df_norm$.subject, df_norm$.condition, sep = "\t")
  mode <- if (anyDuplicated(key)) "lag" else "obs"

  # Build per-observation sources list
  obs_rows <- list()
  n_lags_vec <- integer(0)
  lag_labels_global <- NULL

  if (mode == "obs") {
    for (i in seq_len(nrow(df_norm))) {
      f <- df_norm$.file[i]
      dims <- .nifti_header_dims(f, cache = dim_cache)
      vols <- df_norm$.file_vols[[i]]
      if (is.null(vols)) {
        if (length(dims) == 4L) vols <- seq_len(dims[4]) else vols <- 1L
      }
      if (length(dims) == 3L && length(vols) > 1L) {
        stop("3D file cannot provide multiple lags: ", f, call. = FALSE)
      }
      if (any(is.na(vols)) || any(vols < 1L)) stop("Volume indices must be positive integers", call. = FALSE)

      src <- lapply(vols, function(v) list(file = f, volume = if (length(dims) == 4L) as.integer(v) else NULL))
      obs_rows[[length(obs_rows) + 1L]] <- data.frame(
        group = df_norm$.group[i],
        subject = df_norm$.subject[i],
        condition = as.character(df_norm$.condition[i]),
        stringsAsFactors = FALSE
      )
      obs_rows[[length(obs_rows)]]$sources <- list(src)
      n_lags_vec <- c(n_lags_vec, length(src))

      # Lag labels: preserve original lag indices when selecting dim4 volumes
      this_lag_labels <- if (length(dims) == 4L) as.integer(vols - 1L) else 0L
      if (is.null(lag_labels_global)) {
        lag_labels_global <- this_lag_labels
      } else if (!identical(as.integer(lag_labels_global), as.integer(this_lag_labels))) {
        stop("Selected lag volumes are inconsistent across observations.", call. = FALSE)
      }
    }
  } else {
    # lag mode: multiple rows per observation
    split_idx <- split(seq_len(nrow(df_norm)), key)
    for (k in names(split_idx)) {
      idx <- split_idx[[k]]
      chunk <- df_norm[idx, , drop = FALSE]
      # Determine ordering of rows (lags)
      ord <- seq_len(nrow(chunk))
      if (any(!is.na(chunk$.lag))) {
        ord <- order(chunk$.lag, na.last = TRUE)
        lag_labels <- chunk$.lag[ord]
      } else {
        # Fall back to volume index ordering (from file[spec] or volume column)
        vols_i <- vapply(chunk$.file_vols, function(v) if (is.null(v)) NA_integer_ else as.integer(v)[1], integer(1))
        ord <- order(vols_i, na.last = TRUE)
        if (all(!is.na(vols_i))) {
          lag_labels <- as.integer(vols_i[ord] - 1L)
        } else {
          lag_labels <- seq_len(length(ord)) - 1L
        }
      }
      chunk <- chunk[ord, , drop = FALSE]

      src <- vector("list", nrow(chunk))
      for (j in seq_len(nrow(chunk))) {
        f <- chunk$.file[j]
        dims <- .nifti_header_dims(f, cache = dim_cache)
        vols <- chunk$.file_vols[[j]]
        if (length(dims) == 4L) {
          if (is.null(vols) || length(vols) != 1L) {
            stop("Lag-mode rows referencing 4D files must specify exactly one volume (file[vol] or volume column).", call. = FALSE)
          }
          src[[j]] <- list(file = f, volume = as.integer(vols[[1]]))
        } else {
          if (!is.null(vols) && length(vols) > 1L) {
            stop("3D file cannot provide multiple lags: ", f, call. = FALSE)
          }
          src[[j]] <- list(file = f, volume = NULL)
        }
      }

      # Global lag labels consistency
      if (is.null(lag_labels_global)) {
        lag_labels_global <- lag_labels
      } else if (!identical(as.integer(lag_labels_global), as.integer(lag_labels))) {
        # Keep it simple: require identical lag labels/order across observations
        stop("Lag labels/order are inconsistent across observations; provide a consistent 'lag' column.", call. = FALSE)
      }

      parts <- strsplit(k, "\t", fixed = TRUE)[[1]]
      obs_rows[[length(obs_rows) + 1L]] <- data.frame(
        group = parts[1],
        subject = parts[2],
        condition = parts[3],
        stringsAsFactors = FALSE
      )
      obs_rows[[length(obs_rows)]]$sources <- list(src)
      n_lags_vec <- c(n_lags_vec, length(src))
    }
  }

  if (require_equal_lags && length(unique(n_lags_vec)) != 1L) {
    stop("All observations must have the same number of lags (dim4 or selector).", call. = FALSE)
  }
  n_lags <- if (length(n_lags_vec)) as.integer(n_lags_vec[[1]]) else 1L
  if (n_lags < 1L) stop("Number of lags must be >= 1", call. = FALSE)
  lag_labels <- if (!is.null(lag_labels_global)) as.integer(lag_labels_global) else as.integer(seq_len(n_lags) - 1L)

  obs_df <- do.call(rbind, obs_rows)
  ordered <- .order_manifest_observations(obs_df, groups = groups, conditions = conditions)

  list(
    groups = groups,
    conditions = conditions,
    num_cond = length(conditions),
    num_subj_lst = ordered$num_subj_lst,
    n_lags = n_lags,
    lag_labels = lag_labels,
    obs_by_group = ordered$obs_by_group,
    mode = mode
  )
}

#' Add Subjects from a Lagged NIfTI Manifest
#'
#' @description
#' Adds subject data to a PLS specification from a "manifest" describing
#' subject × condition observations and the NIfTI files used for each.
#'
#' The canonical (and recommended) format is one row per observation
#' (subject × condition), where `file` points to a 4D NIfTI and dim4 encodes
#' lags (time since onset). Optionally, a subset of lags can be selected via
#' `file[1:8]` syntax or a separate `volume` column.
#'
#' A second supported format is "one row per lag": provide multiple rows with
#' the same (subject, condition) key, plus a `lag` column (0-based) or `volume`
#' column; each row supplies a single 3D file or a single selected volume from a
#' 4D file.
#'
#' @param spec A `pls_spec` object
#' @param manifest A data.frame or a file path (.csv/.tsv/.rds)
#' @param mask Brain mask (NeuroVol or file path). Required for NIfTI ingestion.
#' @param subject_col,condition_col,file_col Column names (or NULL to use defaults)
#' @param group_col Optional group column (default looks for "group")
#' @param lag_col Optional lag column (default looks for "lag")
#' @param volume_col Optional volume selector column (default looks for "volume")
#' @param base_dir Optional base directory for relative file paths
#'
#' @return Updated `pls_spec` object
#' @export
add_subjects_manifest <- function(spec,
                                  manifest,
                                  mask,
                                  subject_col = NULL,
                                  condition_col = NULL,
                                  file_col = NULL,
                                  group_col = NULL,
                                  lag_col = NULL,
                                  volume_col = NULL,
                                  base_dir = NULL) {
  assert_that(inherits(spec, "pls_spec"))

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for manifest-based inputs", call. = FALSE)
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
    lag_col = lag_col,
    volume_col = volume_col,
    base_dir = base_dir
  )

  prep <- .prepare_manifest_observations(df_norm, mask_vol)

  # Store raw group-wise observation specs; processing to matrices happens in run()
  spec$datamat_lst <- prep$obs_by_group
  spec$num_subj_lst <- prep$num_subj_lst
  spec$num_cond <- as.integer(prep$num_cond)
  spec$conditions <- as.character(prep$conditions)
  spec$groups <- as.character(prep$groups)
  spec$mask <- mask_vol
  spec$feature_layout <- list(
    kind = "voxel_lag",
    fold_order = "voxel_lag", # lag varies fastest within voxel (MATLAB reshape)
    n_voxels = as.integer(sum(as.array(mask_vol) > 0)),
    n_lags = as.integer(prep$n_lags),
    lag_labels = as.integer(prep$lag_labels),
    source = "manifest",
    mode = prep$mode
  )
  spec$.manifest_raw <- TRUE

  spec
}
