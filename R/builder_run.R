#' Builder API: Run Analysis
#'
#' @description
#' Executes the PLS analysis from a specification.
#'
#' @name builder-run
NULL

#' Run PLS Analysis from Specification
#'
#' @description
#' Validates the PLS specification and executes the analysis.
#' This is the final step in the builder pattern.
#'
#' @param spec A `pls_spec` object
#' @param progress Logical, show progress messages
#' @param ... Additional arguments passed to `pls_analysis()`
#'
#' @return A `pls_result` object
#' @export
#'
#' @examples
#' set.seed(42)
#' data1 <- matrix(rnorm(60 * 50), 60, 50)
#' data2 <- matrix(rnorm(54 * 50), 54, 50)
#'
#' result <- pls_spec() |>
#'   add_subjects(list(data1, data2), groups = c(20, 18)) |>
#'   add_conditions(3) |>
#'   configure(method = "task", nperm = 10) |>
#'   run(progress = FALSE)
run <- function(spec, progress = TRUE, ...) {
  UseMethod("run")
}

#' Materialize Raw PLS Inputs Without Fitting
#'
#' @keywords internal
.materialize_pls_spec <- function(spec, derive_seed_behavior = TRUE) {
  assert_that(inherits(spec, "pls_spec"))

  # Process BIDS data if needed
  if (isTRUE(spec$.bids_raw)) {
    spec <- .process_bids_data(spec)
  }

  # Process manifest-based data if needed
  if (isTRUE(spec$.manifest_raw)) {
    spec <- .process_manifest_data(spec)
  }

  # Process map-manifest data if needed
  if (isTRUE(spec$.map_manifest_raw)) {
    spec <- .process_map_manifest_data(spec)
  }

  # Process trial-level within-subject seed data if needed
  if (isTRUE(spec$.trial_raw)) {
    spec <- .process_trial_data(spec)
  }

  # Derive seed behavior matrix if requested (Seed PLS with manifest/BIDS inputs)
  if (isTRUE(derive_seed_behavior) &&
      is.null(spec$stacked_behavdata) &&
      !is.null(spec$seed_info) &&
      spec$method %in% c(3L, 4L, 5L, 6L)) {
    spec <- .process_seed_data(spec)
  }

  spec
}

#' @export
run.pls_spec <- function(spec, progress = TRUE, ...) {
  spec <- .materialize_pls_spec(spec, derive_seed_behavior = TRUE)

  # Validate specification
  .validate_spec(spec)

  # Run analysis
  result <- pls_analysis(
    datamat_lst = spec$datamat_lst,
    num_subj_lst = spec$num_subj_lst,
    num_cond = spec$num_cond,
    method = spec$method,
    num_perm = spec$num_perm,
    num_boot = spec$num_boot,
    num_split = spec$num_split,
    clim = spec$clim,
    stacked_behavdata = spec$stacked_behavdata,
    stacked_designdata = spec$stacked_designdata,
    bscan = spec$bscan,
    meancentering_type = spec$meancentering_type,
    cormode = spec$cormode,
    boot_type = spec$boot_type,
    is_struct = spec$is_struct,
    parallel_config = spec$.parallel,
    progress = progress,
    ...
  )

  # Store metadata
  result$mask <- spec$mask
  result$groups <- spec$groups
  result$site <- spec$site
  result$conditions <- spec$conditions
  result$feature_layout <- spec$feature_layout
  result$ws_seed_info <- spec$ws_seed_info

  if (!isTRUE(spec$.skip_site_diagnostics) && !is.null(spec$site)) {
    if (spec$method %in% c(3L, 5L) && !is.list(spec$num_subj_lst)) {
      result$site_diagnostics <- site_pooling_diagnostics(
        result,
        spec = spec,
        site = spec$site,
        progress = FALSE
      )
    } else {
      warning(
        "Automatic site diagnostics are currently available only for balanced behavior PLS methods.",
        call. = FALSE
      )
    }
  }

  result
}

#' Validate PLS Specification
#'
#' @keywords internal
.validate_spec <- function(spec) {
  errors <- character(0)

  # Check data matrices
  if (length(spec$datamat_lst) == 0) {
    if (!isTRUE(spec$.trial_raw)) {
      errors <- c(errors, "No data matrices. Use add_subjects() first.")
    }
  }

  # Check conditions
  if (is.null(spec$num_cond)) {
    errors <- c(errors, "Number of conditions not set. Use add_conditions() first.")
  }

  # Method-specific checks
  method <- spec$method

  if (method %in% c(3L, 4L, 5L, 6L)) {
    if (is.null(spec$stacked_behavdata)) {
      is_seed <- is.list(spec$seed_info) && !is.null(spec$seed_info$seed_source)
      if (!is_seed || !identical(method, 3L)) {
        errors <- c(errors,
          sprintf("Method %d requires behavior data. Use add_behavior() first.", method))
      } else if (is.null(spec$mask)) {
        errors <- c(errors, "Seed PLS requires spec$mask to compute seed data.")
      }
    }
  }

  if (method %in% c(2L, 5L, 6L)) {
    if (is.null(spec$stacked_designdata)) {
      errors <- c(errors,
        sprintf("Method %d requires design contrasts. Use add_design() first.", method))
    }
  }

  # Check minimum subjects for bootstrap
  if (spec$num_boot > 0) {
    if (is.list(spec$num_subj_lst)) {
      bscan <- if (is.null(spec$bscan)) seq_len(spec$num_cond) else as.integer(spec$bscan)
      selected_counts <- unlist(lapply(spec$num_subj_lst, function(x) as.integer(x)[bscan]))
      if (length(selected_counts) == 0 || min(selected_counts) < 3) {
        errors <- c(errors, "Bootstrap requires at least 3 subjects per selected condition (ssb).")
      }
    } else {
      min_subj <- min(spec$num_subj_lst)
      if (min_subj < 3) {
        errors <- c(errors,
          "Bootstrap requires at least 3 subjects per group.")
      }
    }
  }

  # Check minimum subjects for split-half
  if (spec$num_split > 0) {
    if (is.list(spec$num_subj_lst)) {
      errors <- c(errors, "Split-half validation currently requires balanced designs (numeric num_subj_lst).")
    } else {
      min_subj <- min(spec$num_subj_lst)
      if (min_subj < 4) {
        errors <- c(errors,
          "Split-half validation requires at least 4 subjects per group.")
      }
    }
  }

  # Report errors
  if (length(errors) > 0) {
    stop(paste(c("PLS specification validation failed:", errors), collapse = "\n  - "))
  }

  invisible(TRUE)
}

#' Process Trial-Level Within-Subject Seed Data
#'
#' @keywords internal
.process_trial_data <- function(spec) {
  trial_data <- spec$trial_data
  if (!is.list(trial_data) || !identical(trial_data$kind, "ws_seed")) {
    stop("Invalid trial_data specification", call. = FALSE)
  }
  if (!spec$method %in% c(1L, 2L)) {
    stop(
      "Trial-level within-subject seed preprocessing is supported only for task PLS methods.",
      call. = FALSE
    )
  }

  expected_conditions <- spec$conditions %||% NULL
  if (!is.null(expected_conditions)) {
    first_levels <- unique(as.character(trial_data$condition_lst[[1]]))
    if (length(expected_conditions) != length(first_levels)) {
      expected_conditions <- NULL
    }
  }

  prep <- .prepare_ws_seed_inputs(
    beta_lst = trial_data$beta_lst,
    seed_lst = trial_data$seed_lst,
    condition_lst = trial_data$condition_lst,
    min_trials = trial_data$min_trials,
    expected_conditions = expected_conditions,
    allow_missing_conditions = TRUE
  )
  corr_rows <- .compute_ws_seed_subject_rows(prep, fisher_z = trial_data$fisher_z)
  grouped <- .build_ws_seed_grouped_datamat(
    corr_rows = corr_rows,
    subject_groups = .resolve_trial_subject_groups(
      trial_groups = trial_data$groups %||% NULL,
      num_subj_lst = spec$num_subj_lst,
      n_subjects = prep$n_subjects,
      group_labels = spec$groups %||% NULL
    ),
    cond_labels = prep$cond_labels,
    n_voxels = prep$n_voxels,
    n_seeds = prep$n_seeds,
    seed_labels = trial_data$seed_labels %||% .resolve_ws_seed_labels(prep$n_seeds, NULL),
    layout = trial_data$layout %||% "seed_condition"
  )

  n_na <- sum(vapply(grouped$datamat_lst, function(x) sum(is.na(x)), numeric(1)))
  if (n_na > 0L) {
    total_vals <- sum(vapply(grouped$datamat_lst, length, integer(1)))
    pct_na <- round(100 * n_na / max(1L, total_vals), 1)
    cli::cli_alert_warning(
      "{n_na} NA values ({pct_na}%) were generated in within-subject seed maps; missing-data handling will be used downstream."
    )
  }

  expected_num_cond <- grouped$num_cond
  base_num_cond <- prep$n_cond

  if (!is.null(spec$num_cond) && !as.integer(spec$num_cond) %in% c(as.integer(base_num_cond), as.integer(expected_num_cond))) {
    stop(sprintf(
      "num_cond = %d but trial data implies %d or %d conditions",
      spec$num_cond, base_num_cond, expected_num_cond
    ), call. = FALSE)
  }
  supplied_num_subj <- spec$num_subj_lst
  if (identical(grouped$layout, "seed_condition") && is.list(supplied_num_subj)) {
    supplied_num_subj <- lapply(supplied_num_subj, function(x) {
      x <- as.integer(x)
      if (length(x) == prep$n_cond) rep(x, prep$n_seeds) else x
    })
  }

  .validate_trial_num_subj_spec(
    supplied = supplied_num_subj,
    derived = grouped$num_subj_lst
  )

  spec$datamat_lst <- grouped$datamat_lst
  spec$num_subj_lst <- grouped$num_subj_lst
  spec$num_cond <- grouped$num_cond

  if (is.null(spec$conditions)) {
    spec$conditions <- grouped$conditions
  } else if (length(spec$conditions) == prep$n_cond && identical(grouped$layout, "seed_condition")) {
    spec$conditions <- .expand_ws_seed_conditions(grouped$seed_labels, as.character(spec$conditions))
  } else if (length(spec$conditions) != grouped$num_cond) {
    stop(sprintf(
      "conditions has length %d but trial data implies %d or %d conditions",
      length(spec$conditions), prep$n_cond, grouped$num_cond
    ), call. = FALSE)
  }

  if (is.null(spec$stacked_designdata) && identical(spec$method, 2L)) {
    spec$stacked_designdata <- .default_nonrotated_task_design(grouped$num_cond, length(grouped$datamat_lst))
  }

  spec$groups <- grouped$groups

  spec$feature_layout <- if (identical(grouped$layout, "seed_condition")) {
    list(
      kind = "voxel_map",
      n_voxels = prep$n_voxels,
      source = "ws_seed",
      layout = grouped$layout,
      n_seeds = prep$n_seeds
    )
  } else if (prep$n_seeds == 1L) {
    list(kind = "voxel_map", n_voxels = prep$n_voxels, source = "ws_seed", layout = grouped$layout)
  } else {
    list(kind = "voxel_seed", n_voxels = prep$n_voxels, n_seeds = prep$n_seeds, source = "ws_seed", layout = grouped$layout)
  }
  spec$ws_seed_info <- list(
    n_seeds = prep$n_seeds,
    n_voxels = prep$n_voxels,
    seed_labels = grouped$seed_labels,
    layout = grouped$layout,
    fisher_z = isTRUE(trial_data$fisher_z),
    min_trials = as.integer(trial_data$min_trials),
    cond_labels = prep$cond_labels,
    expanded_conditions = grouped$conditions
  )
  spec$.trial_raw <- FALSE

  spec
}

.default_nonrotated_task_design <- function(num_cond, n_groups) {
  one_group <- diag(as.integer(num_cond))
  do.call(rbind, rep(list(one_group), as.integer(n_groups)))
}

.resolve_trial_subject_groups <- function(trial_groups,
                                          num_subj_lst,
                                          n_subjects,
                                          group_labels = NULL) {
  if (!is.null(trial_groups)) {
    if (length(trial_groups) != n_subjects) {
      stop("trial_data groups must have length equal to the number of subjects", call. = FALSE)
    }
    grp <- as.character(trial_groups)
    levels <- if (!is.null(group_labels) && length(group_labels) > 0L) {
      as.character(group_labels)
    } else {
      unique(grp)
    }
    if (!all(unique(grp) %in% levels)) {
      stop("trial_data groups contain labels not present in spec$groups", call. = FALSE)
    }
    return(as.character(factor(grp, levels = levels)))
  }

  if (length(num_subj_lst) == 0L) {
    return(rep("all", n_subjects))
  }

  if (is.list(num_subj_lst)) {
    stop(
      "SSB within-subject seed preprocessing requires subject-level group assignments via add_trial_data(..., groups = ...)",
      call. = FALSE
    )
  }

  counts <- as.integer(num_subj_lst)
  if (sum(counts) != n_subjects) {
    stop(sprintf(
      "sum(num_subj_lst) = %d but trial data contains %d subjects",
      sum(counts), n_subjects
    ), call. = FALSE)
  }

  labels <- if (!is.null(group_labels) && length(group_labels) == length(counts)) {
    as.character(group_labels)
  } else if (length(counts) == 1L) {
    "all"
  } else {
    paste0("group_", seq_along(counts))
  }

  rep(labels, counts)
}

.build_ws_seed_grouped_datamat <- function(corr_rows,
                                          subject_groups,
                                          cond_labels,
                                          n_voxels,
                                          n_seeds,
                                          seed_labels,
                                          layout = c("seed_condition", "stacked_seed_features")) {
  layout <- match.arg(layout)
  groups <- unique(subject_groups)
  datamat_lst <- vector("list", length(groups))
  names(datamat_lst) <- groups
  counts_by_group <- vector("list", length(groups))
  names(counts_by_group) <- groups

  expanded_conditions <- if (identical(layout, "seed_condition")) {
    .expand_ws_seed_conditions(seed_labels, cond_labels)
  } else {
    cond_labels
  }

  for (gi in seq_along(groups)) {
    g <- groups[[gi]]
    subj_idx <- which(subject_groups == g)
    row_blocks <- list()
    count_vec <- integer(length(expanded_conditions))

    if (identical(layout, "seed_condition")) {
      for (si in seq_len(n_seeds)) {
        col_start <- (si - 1L) * n_voxels + 1L
        col_end <- si * n_voxels

        for (ci in seq_along(cond_labels)) {
          cond_label <- cond_labels[[ci]]
          cond_rows <- lapply(subj_idx, function(subj) {
            row <- corr_rows[[subj]][[cond_label]]
            if (is.null(row)) return(NULL)
            row[col_start:col_end]
          })
          cond_rows <- Filter(Negate(is.null), cond_rows)
          out_idx <- (si - 1L) * length(cond_labels) + ci
          count_vec[[out_idx]] <- length(cond_rows)
          if (length(cond_rows) > 0L) {
            row_blocks <- c(row_blocks, cond_rows)
          }
        }
      }
    } else {
      for (ci in seq_along(cond_labels)) {
        cond_label <- cond_labels[[ci]]
        cond_rows <- lapply(subj_idx, function(si) corr_rows[[si]][[cond_label]])
        cond_rows <- Filter(Negate(is.null), cond_rows)
        count_vec[[ci]] <- length(cond_rows)
        if (length(cond_rows) > 0L) {
          row_blocks <- c(row_blocks, cond_rows)
        }
      }
    }

    datamat_lst[[gi]] <- if (length(row_blocks) == 0L) {
      matrix(numeric(0), nrow = 0L, ncol = if (identical(layout, "seed_condition")) n_voxels else n_voxels * n_seeds)
    } else {
      do.call(rbind, row_blocks)
    }
    counts_by_group[[gi]] <- as.integer(count_vec)
  }

  is_ssb <- any(vapply(counts_by_group, function(x) length(unique(x)) != 1L, logical(1)))
  num_subj_lst <- if (is_ssb) {
    counts_by_group
  } else {
    as.integer(vapply(counts_by_group, function(x) x[[1]], integer(1)))
  }

  list(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    groups = as.character(groups),
    num_cond = length(expanded_conditions),
    conditions = expanded_conditions,
    layout = layout,
    seed_labels = seed_labels
  )
}

.validate_trial_num_subj_spec <- function(supplied, derived) {
  if (length(supplied) == 0L || is.null(supplied)) {
    return(invisible(TRUE))
  }

  if (is.list(derived)) {
    if (!is.list(supplied)) {
      stop(
        "Trial data implies an SSB design; provide num_subj_lst as a list of per-condition subject counts",
        call. = FALSE
      )
    }
    supplied_norm <- lapply(supplied, as.integer)
    if (!identical(supplied_norm, derived)) {
      stop("Supplied num_subj_lst does not match the SSB counts implied by trial data", call. = FALSE)
    }
    return(invisible(TRUE))
  }

  supplied_norm <- if (is.list(supplied)) lapply(supplied, as.integer) else as.integer(supplied)
  if (is.list(supplied_norm) || !identical(supplied_norm, derived)) {
    stop("Supplied num_subj_lst does not match the counts implied by trial data", call. = FALSE)
  }

  invisible(TRUE)
}

#' Process BIDS Data
#'
#' @description
#' Converts raw BIDS data to matrices using the condition structure.
#'
#' @keywords internal
.process_bids_data <- function(spec) {
  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for BIDS processing")
  }

  cli::cli_alert_info("Processing BIDS data into matrices...")

  mask <- spec$mask
  num_cond <- spec$num_cond

  new_datamat_lst <- list()

  for (g in seq_along(spec$datamat_lst)) {
    subj_data <- spec$datamat_lst[[g]]

    if (!is.list(subj_data) || is.matrix(subj_data)) {
      # Already a matrix
      new_datamat_lst[[g]] <- subj_data
      next
    }

    n_subj <- length(subj_data)
    n_voxels <- sum(mask)

    # Build matrix: subjects x conditions x voxels
    group_mat <- matrix(0, nrow = n_subj * num_cond, ncol = n_voxels)

    row_idx <- 1
    for (subj_idx in seq_along(subj_data)) {
      img <- subj_data[[subj_idx]]

      # If 4D image, assume last dimension is conditions
      if (length(dim(img)) == 4) {
        for (c in seq_len(num_cond)) {
          vol <- img[,,,c]
          group_mat[row_idx, ] <- vol[mask > 0]
          row_idx <- row_idx + 1
        }
      } else {
        # 3D - single timepoint/condition per file
        group_mat[row_idx, ] <- as.vector(img)[mask > 0]
        row_idx <- row_idx + 1
      }
    }

    new_datamat_lst[[g]] <- group_mat

    # Update num_subj_lst if needed
    spec$num_subj_lst[g] <- nrow(group_mat) / num_cond
  }

  spec$datamat_lst <- new_datamat_lst
  spec$.bids_raw <- FALSE

  spec
}

#' Process Manifest Data
#'
#' @description
#' Converts raw manifest-based observation specs into numeric data matrices.
#' Each observation is defined by a list of NIfTI sources (lags) which are
#' masked and folded into a single feature axis in MATLAB-compatible order.
#'
#' @keywords internal
.process_manifest_data <- function(spec) {
  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for manifest processing", call. = FALSE)
  }

  if (is.null(spec$mask) || !inherits(spec$mask, "NeuroVol")) {
    stop("Manifest processing requires spec$mask to be a neuroim2::NeuroVol", call. = FALSE)
  }

  layout <- spec$feature_layout
  if (is.null(layout) || !is.list(layout) || !identical(layout$kind, "voxel_lag")) {
    stop("Manifest processing requires spec$feature_layout$kind == 'voxel_lag'", call. = FALSE)
  }

  if (length(spec$datamat_lst) == 0) return(spec)

  mask_arr <- as.array(spec$mask) > 0
  n_voxels <- as.integer(sum(mask_arr))
  n_lags <- as.integer(layout$n_lags %||% 1L)
  if (is.na(n_lags) || n_lags < 1) stop("Invalid feature_layout$n_lags", call. = FALSE)

  cli::cli_alert_info("Processing manifest inputs into matrices...")

  gz_cache <- new.env(parent = emptyenv())
  maybe_decompress <- function(path) {
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!endsWith(path, ".gz")) return(path)
    if (exists(path, envir = gz_cache, inherits = FALSE)) {
      return(get(path, envir = gz_cache, inherits = FALSE))
    }
    if (!requireNamespace("R.utils", quietly = TRUE)) {
      return(path)
    }
    out <- file.path(tempdir(), basename(sub("\\.gz$", "", path)))
    if (!file.exists(out)) {
      R.utils::gunzip(path, destname = out, overwrite = TRUE, remove = FALSE)
    }
    assign(path, out, envir = gz_cache)
    out
  }

  read_masked <- function(file, volume_index = NULL) {
    file2 <- maybe_decompress(file)
    idx <- if (is.null(volume_index)) 1L else as.integer(volume_index)
    vol <- neuroim2::read_vol(file2, index = idx)
    arr <- as.array(vol)
    as.numeric(arr[mask_arr])
  }

  new_datamat_lst <- list()
  for (g in seq_along(spec$datamat_lst)) {
    obs <- spec$datamat_lst[[g]]
    if (is.matrix(obs)) {
      new_datamat_lst[[g]] <- obs
      next
    }
    if (!is.data.frame(obs) || !"sources" %in% names(obs)) {
      stop("Manifest datamat_lst entries must be data.frames with a 'sources' column", call. = FALSE)
    }

    n_obs <- nrow(obs)
    group_mat <- matrix(NA_real_, nrow = n_obs, ncol = n_voxels * n_lags)

    for (i in seq_len(n_obs)) {
      src <- obs$sources[[i]]
      if (!is.list(src) || length(src) != n_lags) {
        stop("Observation ", i, " has ", length(src), " lag sources; expected ", n_lags, call. = FALSE)
      }

      tmp <- matrix(NA_real_, nrow = n_lags, ncol = n_voxels)
      for (li in seq_len(n_lags)) {
        s <- src[[li]]
        tmp[li, ] <- read_masked(s$file, s$volume %||% NULL)
      }

      # MATLAB-compatible folding: lag varies fastest within voxel
      group_mat[i, ] <- as.vector(tmp)
    }

    new_datamat_lst[[g]] <- group_mat
  }

  if (!is.null(names(spec$datamat_lst))) {
    names(new_datamat_lst) <- names(spec$datamat_lst)
  }

  spec$datamat_lst <- new_datamat_lst
  spec$.manifest_raw <- FALSE

  spec
}

#' Process Map Manifest Data
#'
#' @description
#' Converts raw map-manifest observation specs into numeric data matrices.
#' Each observation references a single 3D map (or a single selected volume
#' from a 4D image) which is vectorized inside the analysis mask.
#'
#' @keywords internal
.process_map_manifest_data <- function(spec) {
  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for map-manifest processing", call. = FALSE)
  }

  if (is.null(spec$mask) || !inherits(spec$mask, "NeuroVol")) {
    stop("Map-manifest processing requires spec$mask to be a neuroim2::NeuroVol", call. = FALSE)
  }

  layout <- spec$feature_layout
  if (is.null(layout) || !is.list(layout) || !identical(layout$kind, "voxel_map")) {
    stop("Map-manifest processing requires spec$feature_layout$kind == 'voxel_map'", call. = FALSE)
  }

  cli::cli_alert_info("Processing map-manifest inputs into matrices...")

  mask_arr <- as.array(spec$mask) > 0
  n_voxels <- as.integer(sum(mask_arr))

  out <- list()
  for (g in seq_along(spec$datamat_lst)) {
    gd <- spec$datamat_lst[[g]]
    if (!is.data.frame(gd) || !all(c("file", "volume") %in% names(gd))) {
      stop("Map-manifest group entries must be data frames with file and volume columns", call. = FALSE)
    }

    group_mat <- matrix(NA_real_, nrow = nrow(gd), ncol = n_voxels)
    for (i in seq_len(nrow(gd))) {
      file <- gd$file[[i]]
      vol_idx <- suppressWarnings(as.integer(gd$volume[[i]]))
      if (is.na(vol_idx)) vol_idx <- NULL

      img <- if (is.null(vol_idx)) {
        neuroim2::read_vol(file)
      } else {
        neuroim2::read_vol(file, index = vol_idx)
      }

      arr <- as.array(img)
      if (length(dim(arr)) != 3L) {
        stop("Map-manifest observations must resolve to 3D volumes: ", file, call. = FALSE)
      }
      group_mat[i, ] <- as.numeric(arr[mask_arr])
    }

    out[[g]] <- group_mat
  }

  spec$datamat_lst <- out
  spec$.map_manifest_raw <- FALSE
  spec
}

#' Process Seed PLS Behavior Data
#'
#' @description
#' For Seed PLS, behavior data is derived from one or more seed regions.
#' This helper computes the seed matrix from the already-processed datamat
#' using the seed definition stored in `spec$seed_info`.
#'
#' @keywords internal
.process_seed_data <- function(spec) {
  if (is.null(spec$seed_info) || !is.list(spec$seed_info)) return(spec)
  if (!is.null(spec$stacked_behavdata)) return(spec)

  if (is.null(spec$mask)) {
    stop("Seed PLS requires spec$mask", call. = FALSE)
  }
  if (!is.list(spec$datamat_lst) || length(spec$datamat_lst) == 0 || !all(vapply(spec$datamat_lst, is.matrix, logical(1)))) {
    stop("Seed PLS requires numeric data matrices (spec$datamat_lst).", call. = FALSE)
  }

  seed_source <- as.character(spec$seed_info$seed_source)[1]
  if (is.na(seed_source) || !nzchar(seed_source)) {
    stop("Invalid seed_info$seed_source", call. = FALSE)
  }

  labels <- NULL
  roi_ids <- NULL
  roi_labels <- NULL
  lags <- NULL

  if (!is.null(spec$seed_info$lags)) {
    lags <- suppressWarnings(as.integer(spec$seed_info$lags))
    lags <- lags[!is.na(lags)]
    if (length(lags) == 0) lags <- NULL
  }

  if (identical(seed_source, "atlas")) {
    labels <- spec$seed_info$atlas_vol %||% spec$seed_info$labels
    roi_ids <- spec$seed_info$roi_ids
    roi_labels <- spec$seed_info$roi_labels %||% spec$seed_info$roi_names
  } else if (identical(seed_source, "mask")) {
    labels <- spec$seed_info$seed_mask %||% spec$seed_info$labels
    roi_ids <- 1L
    roi_labels <- spec$seed_info$seed_name %||% "seed"
  } else if (seed_source %in% c("custom_atlas", "custom")) {
    labels <- spec$seed_info$seed_atlas %||% spec$seed_info$labels
    roi_ids <- spec$seed_info$roi_ids
    roi_labels <- spec$seed_info$roi_labels
  } else {
    stop("Unknown seed_source: ", seed_source, call. = FALSE)
  }

  if (is.null(labels)) stop("Seed PLS requires a label volume/mask in spec$seed_info", call. = FALSE)

  seed_data <- pls_seed_data_from_labels(
    datamat_lst = spec$datamat_lst,
    mask = spec$mask,
    labels = labels,
    roi_ids = roi_ids,
    roi_labels = roi_labels,
    lags = lags,
    feature_layout = spec$feature_layout,
    na_rm = TRUE,
    drop_zero_var = TRUE
  )

  spec$stacked_behavdata <- seed_data
  spec
}

#' Quick Run for Simple Analyses
#'
#' @description
#' Convenience function for running simple task PLS without full builder API.
#'
#' @param datamat_lst List of data matrices
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param nperm Number of permutations
#' @param nboot Number of bootstrap samples
#' @param ... Additional arguments to pls_analysis()
#'
#' @return A `pls_result` object
#' @export
quick_pls <- function(datamat_lst, num_subj_lst, num_cond,
                       nperm = 1000, nboot = 500, ...) {
  pls_analysis(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    method = 1L,
    num_perm = nperm,
    num_boot = nboot,
    ...
  )
}

#' Seed PLS (Functional Connectivity)
#'
#' @description
#' Convenience function for seed-based PLS analysis, commonly used for
#' functional connectivity. This is Behavior PLS (method 3) where the
#' "behavior" data is the time series from a seed region.
#'
#' The result shows which voxels correlate/covary with the seed region,
#' potentially differently across conditions.
#'
#' @param datamat_lst List of data matrices (one per group). Each matrix has
#'   rows = subjects x conditions, columns = voxels/features.
#' @param seed_data Matrix of seed region data. Rows must match stacked datamat
#'   (subjects x conditions across all groups), columns are seed regions/voxels.
#' @param num_subj_lst Integer vector with number of subjects per group.
#' @param num_cond Number of conditions.
#' @param cormode Correlation mode:
#'   \describe{
#'     \item{0}{Pearson correlation (default)}
#'     \item{2}{Covariance}
#'     \item{4}{Cosine angle}
#'     \item{6}{Dot product}
#'   }
#' @param nperm Number of permutations (default 1000).
#' @param nboot Number of bootstrap samples (default 500).
#' @param ... Additional arguments passed to `pls_analysis()`.
#'
#' @return A `pls_result` object with class `pls_behavior`.
#'
#' @details
#' Seed PLS identifies brain patterns that maximally correlate with activity
#' in a seed region. Unlike standard functional connectivity, it can reveal
#' condition-specific connectivity patterns.
#'
#' To run seed PLS:
#' 1
#' . Extract time series from your seed region(s)
#' 2. Organize as a matrix: rows = observations (matching datamat), columns = seed(s)
#' 3. Pass to this function
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract seed time series (e.g., from PCC)
#' seed_ts <- extract_roi(bold_data, pcc_mask)
#'
#' # Run seed PLS
#' result <- seed_pls(
#'   datamat_lst = list(brain_data),
#'   seed_data = seed_ts,
#'   num_subj_lst = 25,
#'   num_cond = 3
#' )
#'
#' # View connectivity patterns
#' plot_brain(result, lv = 1, what = "bsr", threshold = 3)
#' }
seed_pls <- function(datamat_lst, seed_data, num_subj_lst, num_cond,
                      cormode = 0L, nperm = 1000, nboot = 500, ...) {

  # Validate seed_data dimensions
  total_rows <- sum(sapply(datamat_lst, nrow))
  if (nrow(seed_data) != total_rows) {
    stop(sprintf(
      "seed_data has %d rows but datamat_lst has %d total rows",
      nrow(seed_data), total_rows
    ))
  }

  pls_analysis(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    method = 3L,  # Behavior PLS
    stacked_behavdata = seed_data,
    cormode = cormode,
    num_perm = nperm,
    num_boot = nboot,
    ...
  )
}

#' Behavioral Correlates PLS
#'
#' @description
#' Convenience function for behavior PLS analysis, identifying brain patterns
#' that correlate with behavioral measures (reaction time, accuracy, etc.).
#'
#' @param datamat_lst List of data matrices (one per group).
#' @param behav_data Matrix of behavioral measures. Rows must match stacked
#'   datamat, columns are different behavioral measures.
#' @param num_subj_lst Integer vector with number of subjects per group.
#' @param num_cond Number of conditions.
#' @param cormode Correlation mode (0=Pearson, 2=covariance, 4=cosine, 6=dot).
#' @param nperm Number of permutations (default 1000).
#' @param nboot Number of bootstrap samples (default 500).
#' @param ... Additional arguments passed to `pls_analysis()`.
#'
#' @return A `pls_result` object with class `pls_behavior`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Behavioral measures: RT and accuracy
#' behav <- cbind(rt = rt_data, accuracy = acc_data)
#'
#' result <- behav_pls(
#'   datamat_lst = list(brain_data),
#'   behav_data = behav,
#'   num_subj_lst = 30,
#'   num_cond = 4
#' )
#' }
behav_pls <- function(datamat_lst, behav_data, num_subj_lst, num_cond,
                       cormode = 0L, nperm = 1000, nboot = 500,
                       site = NULL, ...) {

  total_rows <- sum(sapply(datamat_lst, nrow))
  if (nrow(behav_data) != total_rows) {
    stop(sprintf(
      "behav_data has %d rows but datamat_lst has %d total rows",
      nrow(behav_data), total_rows
    ))
  }

  result <- pls_analysis(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    method = 3L,
    stacked_behavdata = behav_data,
    cormode = cormode,
    num_perm = nperm,
    num_boot = nboot,
    ...
  )

  result$site <- site
  if (!is.null(site)) {
    if (!is.list(num_subj_lst)) {
      spec <- pls_spec()
      spec$datamat_lst <- datamat_lst
      spec$stacked_behavdata <- behav_data
      spec$num_subj_lst <- num_subj_lst
      spec$num_cond <- num_cond
      spec$method <- 3L
      spec$cormode <- cormode
      spec$site <- site
      result$site_diagnostics <- site_pooling_diagnostics(
        result,
        spec = spec,
        site = site,
        progress = FALSE
      )
    } else {
      warning(
        "Automatic site diagnostics are currently available only for balanced behavior PLS methods.",
        call. = FALSE
      )
    }
  }

  result
}


# --- Within-Subject Seed PLS builder support ---

#' Add Trial-Level Data for Within-Subject Seed PLS
#'
#' @description
#' Attaches trial-level beta matrices, seed data, and condition mappings
#' to a PLS specification for use with \code{method = "ws_seed"}.
#'
#' @param spec A \code{pls_spec} object.
#' @param beta_lst A list of matrices (one per subject), each with
#'   rows = trials and columns = voxels.
#' @param seed_lst A list of numeric vectors or matrices (one per subject)
#'   with the seed region's trial-level betas.
#' @param condition_lst A list of integer vectors or factors (one per subject)
#'   mapping trials to conditions.
#' @param fisher_z Logical; apply Fisher r-to-z transform (default \code{TRUE}).
#' @param min_trials Minimum trials per condition for valid correlation
#'   (default 3).
#'
#' @return Updated \code{pls_spec} object.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- pls_spec() |>
#'   add_trial_data(beta_lst, seed_lst, cond_lst) |>
#'   configure(method = "ws_seed", nperm = 500, nboot = 500) |>
#'   run()
#' }
add_trial_data <- function(spec, beta_lst, seed_lst, condition_lst,
                           groups = NULL,
                           seed_labels = NULL,
                           layout = c("seed_condition", "stacked_seed_features"),
                           fisher_z = TRUE, min_trials = 3L) {
  assert_that(inherits(spec, "pls_spec"))
  layout <- match.arg(layout)

  if (length(spec$datamat_lst) > 0 || isTRUE(spec$.bids_raw) || isTRUE(spec$.manifest_raw) || isTRUE(spec$.map_manifest_raw)) {
    stop("add_trial_data() cannot be combined with other subject-data inputs in the same pls_spec")
  }

  prep <- .prepare_ws_seed_inputs(
    beta_lst = beta_lst,
    seed_lst = seed_lst,
    condition_lst = condition_lst,
    min_trials = min_trials,
    expected_conditions = spec$conditions %||% NULL,
    allow_missing_conditions = TRUE
  )

  if (!is.null(groups) && length(groups) != prep$n_subjects) {
    stop("groups must have length equal to the number of subjects in trial_data")
  }

  seed_labels <- .resolve_ws_seed_labels(prep$n_seeds, seed_labels)

  if (length(spec$num_subj_lst) > 0 && !is.list(spec$num_subj_lst) &&
      is.null(groups) &&
      sum(as.integer(spec$num_subj_lst)) != prep$n_subjects) {
    stop(sprintf(
      "sum(num_subj_lst) = %d but trial data contains %d subjects",
      sum(as.integer(spec$num_subj_lst)), prep$n_subjects
    ))
  }
  expanded_cond_labels <- .expand_ws_seed_conditions(seed_labels, prep$cond_labels)
  if (!is.null(spec$num_cond)) {
    valid_num_cond <- if (identical(layout, "seed_condition")) {
      c(as.integer(prep$n_cond), as.integer(length(expanded_cond_labels)))
    } else {
      as.integer(prep$n_cond)
    }
    if (!as.integer(spec$num_cond) %in% valid_num_cond) {
      stop(sprintf(
        "num_cond = %d but trial data implies %s conditions",
        spec$num_cond,
        paste(valid_num_cond, collapse = " or ")
      ))
    }
  }
  if (!is.null(spec$conditions)) {
    valid_condition_lengths <- if (identical(layout, "seed_condition")) {
      c(prep$n_cond, length(expanded_cond_labels))
    } else {
      prep$n_cond
    }
    if (!length(spec$conditions) %in% valid_condition_lengths) {
      stop(sprintf(
        "conditions has length %d but trial data implies %s conditions",
        length(spec$conditions),
        paste(valid_condition_lengths, collapse = " or ")
      ))
    }
  }

  spec$trial_data <- list(
    kind = "ws_seed",
    beta_lst = prep$beta_lst,
    seed_lst = prep$seed_lst,
    condition_lst = prep$condition_lst,
    groups = if (is.null(groups)) NULL else as.character(groups),
    seed_labels = seed_labels,
    layout = layout,
    fisher_z = fisher_z,
    min_trials = prep$min_trials
  )
  spec$.trial_raw <- TRUE
  spec$ws_seed_info <- list(
    n_seeds = prep$n_seeds,
    n_voxels = prep$n_voxels,
    seed_labels = seed_labels,
    layout = layout,
    fisher_z = isTRUE(fisher_z),
    min_trials = prep$min_trials,
    cond_labels = prep$cond_labels
  )

  if (length(spec$num_subj_lst) == 0 && is.null(groups)) {
    spec$num_subj_lst <- prep$n_subjects
  }
  if (is.null(spec$num_cond) || spec$num_cond == 0L) {
    spec$num_cond <- if (identical(layout, "seed_condition")) prep$n_cond * prep$n_seeds else prep$n_cond
  }
  if (is.null(spec$conditions)) {
    spec$conditions <- if (identical(layout, "seed_condition")) {
      expanded_cond_labels
    } else {
      prep$cond_labels
    }
  }

  spec
}
