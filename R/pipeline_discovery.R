#' Pipeline Discovery Stages
#'
#' @description
#' Validation, BIDS discovery, and pipeline status reporting for `plscli`.
#'
#' @name pipeline-discovery
NULL

#' Validate a Pipeline and Write Validation Output
#'
#' @param spec Path to a YAML file or a pipeline specification list.
#'
#' @return A data frame describing the validated configuration.
#' @export
pipeline_validate <- function(spec) {
  spec <- validate_pipeline_spec(spec)
  paths <- .pipeline_artifact_paths(spec)

  report <- data.frame(
    field = c("bids_dir", "tasks", "first_level_output", "pls_method", "execution_mode", "output_root"),
    value = c(
      spec$dataset$bids_dir,
      paste(spec$dataset$task, collapse = ","),
      spec$first_level$output$type,
      spec$pls$method,
      spec$execution$mode,
      paths$root
    ),
    stringsAsFactors = FALSE
  )

  .pipeline_write_tsv(report, paths$validation_tsv)
  report
}

#' Discover BIDS Inputs for `plscli`
#'
#' @param spec Path to a YAML file or a pipeline specification list.
#'
#' @return The discovery manifest as a data.frame.
#' @export
pipeline_discover <- function(spec) {
  spec <- validate_pipeline_spec(spec)
  .pipeline_require(c("bidser", "neuroim2"))
  paths <- .pipeline_artifact_paths(spec)

  bids <- bidser::bids_project(spec$dataset$bids_dir, fmriprep = TRUE)
  participants <- .pipeline_normalize_subject(bidser::participants(bids))
  selected_subjects <- .pipeline_as_chr(.pipeline_nested(spec, c("dataset", "subjects"), participants))
  if (length(selected_subjects)) {
    participants <- participants[participants %in% .pipeline_normalize_subject(selected_subjects)]
  }
  if (!length(participants)) stop("No matching participants found in BIDS dataset", call. = FALSE)

  part_df <- bids$part_df
  group_col <- .pipeline_nested(spec, c("dataset", "group_column"), NULL)
  group_lookup <- stats::setNames(rep("all", length(participants)), participants)
  if (!is.null(group_col) && is.data.frame(part_df) && group_col %in% names(part_df)) {
    subject_ids <- .pipeline_normalize_subject(part_df$participant_id)
    group_lookup <- stats::setNames(as.character(part_df[[group_col]]), subject_ids)
  }

  rows <- list()
  for (task in spec$dataset$task) {
    for (subject in participants) {
      scans <- bidser::preproc_scans(
        bids,
        subid = paste0("^", subject, "$"),
        task = task,
        space = spec$dataset$space %||% NULL,
        full_path = TRUE
      )
      if (is.null(scans) || length(scans) == 0) next
      scans <- .pipeline_order_runs(scans)

      event_regex <- paste0("^sub-", subject, ".*_task-", task, ".*_events\\.(tsv|csv)$")
      event_files <- bidser::search_files(
        bids,
        regex = event_regex,
        full_path = TRUE
      )
      event_files <- .pipeline_order_runs(event_files)

      mask_files <- bidser::mask_files(
        bids,
        subid = paste0("^", subject, "$"),
        space = spec$dataset$space %||% NULL,
        full_path = TRUE
      )
      mask_file <- if (length(mask_files)) mask_files[[1]] else .pipeline_nested(spec, c("dataset", "mask"), NA_character_)

      if (length(event_files) && length(event_files) != length(scans)) {
        warning(sprintf(
          "Subject %s task %s has %d scan(s) but %d event file(s)",
          subject, task, length(scans), length(event_files)
        ))
      }

      for (i in seq_along(scans)) {
        info <- .pipeline_nifti_info(scans[[i]])
        run_label <- .pipeline_extract_entity(scans[[i]], "run")
        if (is.na(run_label) && i <= length(event_files)) {
          run_label <- .pipeline_extract_entity(event_files[[i]], "run")
        }
        rows[[length(rows) + 1L]] <- data.frame(
          subject = subject,
          group = unname(group_lookup[[subject]] %||% "all"),
          task = task,
          run = if (is.na(run_label)) as.character(i) else as.character(run_label),
          scan_file = scans[[i]],
          events_file = if (i <= length(event_files)) event_files[[i]] else NA_character_,
          mask_file = mask_file,
          n_volumes = as.integer(info$n_volumes),
          tr = as.numeric(info$tr),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (!length(rows)) {
    stop("Discovery found no matching preprocessed scans", call. = FALSE)
  }

  out <- do.call(rbind, rows)
  .validate_discovery_manifest(out)
  .pipeline_write_tsv(out, paths$discovery_tsv)
  out
}

#' Summarize Pipeline Stage Outputs
#'
#' @param spec Path to a YAML file or a pipeline specification list.
#'
#' @return A data.frame summarizing stage status.
#' @export
pipeline_summarize <- function(spec) {
  spec <- read_pipeline_spec(spec)
  paths <- .pipeline_artifact_paths(spec)

  summary <- data.frame(
    stage = c("validate", "discover", "firstlevel_plan", "firstlevel_run", "pls_plan", "pls_run"),
    status = c(
      if (file.exists(paths$validation_tsv)) "done" else "missing",
      if (file.exists(paths$discovery_tsv)) "done" else "missing",
      if (file.exists(paths$firstlevel_plan_tsv)) "done" else "missing",
      if (dir.exists(paths$firstlevel_dir) && length(list.files(paths$firstlevel_dir, pattern = "maps\\.tsv$", recursive = TRUE))) "done" else "missing",
      if (file.exists(paths$pls_plan_rds) && file.exists(paths$pls_manifest_tsv)) "done" else "missing",
      if (file.exists(paths$pls_result_rds)) "done" else "missing"
    ),
    file = c(
      paths$validation_tsv,
      paths$discovery_tsv,
      paths$firstlevel_plan_tsv,
      paths$firstlevel_dir,
      paths$pls_manifest_tsv,
      paths$pls_result_rds
    ),
    stringsAsFactors = FALSE
  )

  .pipeline_write_tsv(summary, paths$pipeline_summary_tsv)
  summary
}
