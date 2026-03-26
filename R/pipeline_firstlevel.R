#' Pipeline First-Level Stages
#'
#' @description
#' Planning and execution helpers for first-level fMRI estimation in `plscli`.
#'
#' @name pipeline-firstlevel
NULL

.pipeline_firstlevel_label_metadata <- function(label, output_spec) {
  pattern <- output_spec$basis_pattern %||% NULL
  if (is.null(pattern) || !nzchar(as.character(pattern)[1])) {
    return(list(
      condition = NA_character_,
      basis_label = NA_character_,
      basis_index = NA_integer_
    ))
  }

  pieces <- regexec(pattern, label, perl = TRUE)
  match <- regmatches(label, pieces)[[1]]
  cond_group <- as.integer(output_spec$condition_group %||% 1L)
  basis_group <- as.integer(output_spec$basis_group %||% 2L)
  if (length(match) <= max(cond_group, basis_group)) {
    return(list(
      condition = NA_character_,
      basis_label = NA_character_,
      basis_index = NA_integer_
    ))
  }

  basis_label <- as.character(match[[basis_group + 1L]])
  basis_order <- .pipeline_as_chr(output_spec$basis_order)
  basis_index <- if (length(basis_order)) match(basis_label, basis_order) else NA_integer_
  if (is.na(basis_index) && grepl("^[0-9]+$", basis_label)) {
    basis_index <- as.integer(basis_label) + 1L
  }

  list(
    condition = as.character(match[[cond_group + 1L]]),
    basis_label = basis_label,
    basis_index = as.integer(basis_index)
  )
}

.pipeline_build_baseline_model <- function(spec, dataset) {
  baseline <- .pipeline_nested(spec, c("design", "baseline"), NULL)
  if (is.null(baseline)) return(NULL)
  .pipeline_require("fmridesign")
  args <- baseline
  args$sframe <- dataset$sampling_frame
  do.call(fmridesign::baseline_model, args)
}

.pipeline_firstlevel_event_table <- function(rows, work_row) {
  event_tables <- vector("list", nrow(rows))
  for (i in seq_len(nrow(rows))) {
    event_file <- rows$events_file[[i]]
    if (is.na(event_file) || !file.exists(event_file)) {
      stop("Missing events file for subject ", work_row$subject, " task ", work_row$task, " run ", rows$run[[i]], call. = FALSE)
    }
    event_tables[[i]] <- .pipeline_read_events(event_file, run_id = i)
  }
  do.call(rbind, event_tables)
}

.pipeline_firstlevel_dataset <- function(rows, spec, work_row) {
  scan_files <- rows$scan_file
  mask_file <- unique(stats::na.omit(rows$mask_file))
  if (!length(mask_file)) {
    stop("No mask file resolved for work unit ", work_row$work_id, call. = FALSE)
  }
  mask_file <- mask_file[[1]]

  tr <- unique(stats::na.omit(rows$tr))
  if (length(tr) == 0 || !is.finite(tr[[1]]) || tr[[1]] <= 0) {
    tr <- .pipeline_nested(spec, c("dataset", "tr"), NULL)
  } else {
    tr <- tr[[1]]
  }
  if (is.null(tr) || !is.finite(as.numeric(tr)) || as.numeric(tr) <= 0) {
    stop("TR could not be resolved from headers or dataset.tr", call. = FALSE)
  }

  event_table <- .pipeline_firstlevel_event_table(rows, work_row)
  dataset <- fmridataset::fmri_dataset(
    scans = scan_files,
    mask = mask_file,
    TR = as.numeric(tr),
    run_length = as.integer(rows$n_volumes),
    event_table = event_table,
    preload = FALSE
  )

  list(dataset = dataset, mask_file = mask_file, tr = as.numeric(tr), event_table = event_table)
}

.pipeline_firstlevel_fit <- function(dataset, spec) {
  baseline_model <- .pipeline_build_baseline_model(spec, dataset)
  fmrireg::fmri_lm(
    stats::as.formula(spec$design$formula),
    block = stats::as.formula(spec$design$block),
    baseline_model = baseline_model,
    dataset = dataset,
    strategy = spec$first_level$strategy,
    nchunks = spec$first_level$nchunks,
    progress = spec$first_level$progress
  )
}

.pipeline_firstlevel_output_file <- function(work_row, output_type, label, statistic) {
  file.path(
    work_row$work_dir,
    sprintf(
      "sub-%s_task-%s_type-%s_label-%s_stat-%s.nii.gz",
      .pipeline_safe_name(work_row$subject),
      .pipeline_safe_name(work_row$task),
      .pipeline_safe_name(output_type),
      .pipeline_safe_name(label),
      .pipeline_safe_name(statistic)
    )
  )
}

.pipeline_firstlevel_manifest_rows <- function(fit, spec, work_row, mask_file) {
  output_type <- spec$first_level$output$type
  labels <- .pipeline_as_chr(spec$first_level$output$labels)
  available <- fmrireg::coef_names(fit, type = output_type)
  if (length(labels)) {
    available <- intersect(available, labels)
  }
  if (!length(available)) {
    stop("No first-level labels matched the requested output selection", call. = FALSE)
  }

  rows <- list()
  for (label in available) {
    meta <- .pipeline_firstlevel_label_metadata(label, spec$first_level$output)
    for (statistic in spec$first_level$output$statistics) {
      img <- fmrireg::coef_image(fit, coef = label, statistic = statistic, type = output_type)
      out_file <- .pipeline_firstlevel_output_file(work_row, output_type, label, statistic)
      RNifti::writeNifti(as.array(img), file = out_file)

      rows[[length(rows) + 1L]] <- .new_firstlevel_manifest_row(
        work_id = work_row$work_id,
        subject = work_row$subject,
        group = work_row$group,
        task = work_row$task,
        type = output_type,
        label = label,
        statistic = statistic,
        file = out_file,
        mask_file = mask_file,
        condition = meta$condition,
        basis_label = meta$basis_label,
        basis_index = meta$basis_index
      )
    }
  }

  out <- do.call(rbind, rows)
  .validate_firstlevel_manifest(out)
  out
}

.pipeline_build_firstlevel_plan <- function(discovery, firstlevel_dir) {
  .validate_discovery_manifest(discovery)
  keys <- unique(discovery[c("subject", "group", "task")])

  rows <- lapply(seq_len(nrow(keys)), function(i) {
    work_id <- sprintf("w%04d", i)
    work_dir <- file.path(firstlevel_dir, work_id)
    .new_firstlevel_plan_row(
      subject = keys$subject[[i]],
      group = keys$group[[i]],
      task = keys$task[[i]],
      work_id = work_id,
      scan_count = sum(discovery$subject == keys$subject[[i]] & discovery$task == keys$task[[i]]),
      work_dir = work_dir,
      fit_file = file.path(work_dir, "fit.rds"),
      manifest_file = file.path(work_dir, "maps.tsv")
    )
  })

  out <- do.call(rbind, rows)
  .validate_firstlevel_plan(out)
  out
}

.pipeline_select_firstlevel_rows <- function(discovery, work_row) {
  rows <- discovery[discovery$subject == work_row$subject & discovery$task == work_row$task, , drop = FALSE]
  rows <- rows[order(rows$run), , drop = FALSE]
  if (nrow(rows) == 0) {
    stop("No discovery rows found for work unit ", work_row$work_id, call. = FALSE)
  }
  rows
}

.pipeline_run_firstlevel_work <- function(spec, work_row, discovery) {
  .pipeline_require(c("fmridataset", "fmrireg", "neuroim2", "RNifti"))

  rows <- .pipeline_select_firstlevel_rows(discovery, work_row)
  dataset_info <- .pipeline_firstlevel_dataset(rows, spec, work_row)
  fit <- .pipeline_firstlevel_fit(dataset_info$dataset, spec)

  dir.create(work_row$work_dir, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(spec$first_level$save_fit)) {
    saveRDS(fit, work_row$fit_file)
  }

  manifest <- .pipeline_firstlevel_manifest_rows(fit, spec, work_row, dataset_info$mask_file)
  root <- .pipeline_output_root(spec)
  .pipeline_write_tsv(manifest, work_row$manifest_file,
                      root = root, path_cols = c("file", "mask_file"))
  manifest
}

#' Plan First-Level Work Units
#'
#' @param spec Path to a YAML file or a pipeline specification list.
#'
#' @return The work-plan data.frame.
#' @export
pipeline_firstlevel_plan <- function(spec) {
  spec <- validate_pipeline_spec(spec)
  paths <- .pipeline_artifact_paths(spec)
  if (!file.exists(paths$discovery_tsv)) {
    pipeline_discover(spec)
  }

  discovery <- .pipeline_read_tsv(paths$discovery_tsv)
  plan <- .pipeline_build_firstlevel_plan(discovery, paths$firstlevel_dir)
  .pipeline_write_tsv(plan, paths$firstlevel_plan_tsv,
                      root = paths$root, path_cols = c("work_dir", "fit_file", "manifest_file"))
  plan
}

#' Run First-Level Estimation
#'
#' @param spec Path to a YAML file or a pipeline specification list.
#' @param work_id Optional work-unit identifier from `pipeline_firstlevel_plan()`.
#'
#' @return A data.frame of first-level map outputs.
#' @export
pipeline_firstlevel_run <- function(spec, work_id = NULL) {
  spec <- validate_pipeline_spec(spec)
  paths <- .pipeline_artifact_paths(spec)
  if (!file.exists(paths$firstlevel_plan_tsv)) {
    pipeline_firstlevel_plan(spec)
  }

  plan <- .pipeline_read_tsv(paths$firstlevel_plan_tsv,
                            root = paths$root, path_cols = c("work_dir", "fit_file", "manifest_file"))
  .validate_firstlevel_plan(plan)
  discovery <- .pipeline_read_tsv(paths$discovery_tsv)
  .validate_discovery_manifest(discovery)

  if (!is.null(work_id)) {
    plan <- plan[plan$work_id %in% as.character(work_id), , drop = FALSE]
    if (nrow(plan) == 0) stop("Requested work_id not found in first-level plan", call. = FALSE)
  }

  out <- lapply(seq_len(nrow(plan)), function(i) {
    .pipeline_run_firstlevel_work(spec, plan[i, , drop = FALSE], discovery)
  })
  do.call(rbind, out)
}
