#' Pipeline Planning Helpers
#'
#' @description
#' Pure planning helpers that transform first-level manifests into PLS-ready
#' observation manifests without performing file I/O or model execution.
#'
#' @name pipeline-planning
NULL

.pipeline_select_firstlevel_outputs <- function(map_rows, pls_input_spec) {
  .validate_firstlevel_manifest(map_rows)

  out <- map_rows[
    map_rows$type == pls_input_spec$type &
      map_rows$statistic == pls_input_spec$statistic,
    ,
    drop = FALSE
  ]
  if (nrow(out) == 0) {
    stop("No first-level outputs matched pls.input.type/statistic", call. = FALSE)
  }

  labels <- .pipeline_as_chr(pls_input_spec$labels)
  if (length(labels)) {
    out <- out[out$label %in% labels, , drop = FALSE]
  }
  if (nrow(out) == 0) {
    stop("No first-level labels matched pls.input.labels", call. = FALSE)
  }

  label_map <- pls_input_spec$label_map %||% list()
  default_condition <- vapply(out$label, function(lbl) {
    mapped <- label_map[[lbl]]
    if (is.null(mapped) || !nzchar(as.character(mapped)[1])) lbl else as.character(mapped)[1]
  }, character(1))
  if (!("condition" %in% names(out))) {
    out$condition <- default_condition
  } else {
    missing_condition <- is.na(out$condition) | !nzchar(out$condition)
    out$condition[missing_condition] <- default_condition[missing_condition]
  }

  out
}

.pipeline_build_pls_manifest <- function(map_rows, pls_input_spec) {
  selected <- .pipeline_select_firstlevel_outputs(map_rows, pls_input_spec)
  manifest_info <- .pipeline_parse_basis_manifest(selected, pls_input_spec)
  .validate_pls_manifest(manifest_info$manifest, kind = manifest_info$kind)
  manifest_info
}
