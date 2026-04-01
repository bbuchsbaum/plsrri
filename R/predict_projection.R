#' Held-Out Score Projection Helpers
#'
#' @name predict-projection
#' @keywords internal
NULL

#' Project Held-Out Observations into a Fitted Score Space
#'
#' @param x A fitted `pls_result` or `mva_result`.
#' @param newdata Held-out `pls_spec` data to project.
#' @param type Score space to project into: `"feature"`/`"brain"`,
#'   `"design"`/`"behavior"`, or `"both"`.
#' @param progress Logical, show progress.
#' @param ... Reserved for method-specific extensions.
#'
#' @return A score matrix, or a named list with `feature` and `design`
#'   matrices when `type = "both"`.
#' @export
#' @examples
#' set.seed(42)
#' datamat <- matrix(rnorm(24 * 10), 24, 10)
#' spec <- pls_spec()
#' spec$datamat_lst <- list(datamat)
#' spec$num_subj_lst <- 12L
#' spec$num_cond <- 2L
#' result <- run(spec, progress = FALSE)
#' proj <- project_scores(result, spec, type = "brain")
#' @export
project_scores <- function(x, newdata, type = "feature", progress = FALSE, ...) {
  UseMethod("project_scores")
}

.normalize_projection_type <- function(type) {
  type <- tolower(as.character(type)[1] %||% "feature")
  switch(
    type,
    feature = "feature",
    brain = "feature",
    design = "design",
    behavior = "design",
    both = "both",
    stop("type must be one of: feature, brain, design, behavior, both", call. = FALSE)
  )
}

#' @export
project_scores.mva_result <- function(x, newdata, type = "feature", progress = FALSE, ...) {
  type <- .normalize_projection_type(type)
  method_obj <- get_method(x$method)
  method_obj$project_scores(
    result = x,
    spec = newdata,
    type = type,
    progress = progress
  )
}

#' @export
project_scores.pls_result <- function(x, newdata, type = "feature", progress = FALSE, ...) {
  project_scores(
    pls_result_to_mva_result(x),
    newdata = newdata,
    type = type,
    progress = progress
  )
}

.pls_projection_context <- function(result) {
  stopifnot(inherits(result, "mva_result"))

  ex <- result$decomposition$extra %||% list()
  fit_spec <- if (inherits(result$spec, "pls_spec")) result$spec else NULL
  other_input <- ex$other_input %||% list()

  list(
    num_cond = ex$num_cond %||% fit_spec$num_cond %||% NULL,
    num_subj_lst = ex$num_subj_lst %||% fit_spec$num_subj_lst %||% NULL,
    groups = ex$groups %||% fit_spec$groups %||% NULL,
    cormode = other_input$cormode %||% fit_spec$cormode %||% 0L
  )
}

.validate_pls_projection_spec <- function(result, spec, method_int, type, ctx) {
  stopifnot(inherits(result, "mva_result"))
  assert_that(inherits(spec, "pls_spec"))

  if (method_int %in% c(4L, 6L)) {
    stop(
      "Held-out score projection is not yet implemented for multiblock PLS methods.",
      call. = FALSE
    )
  }

  if (length(spec$datamat_lst) == 0L) {
    stop("Held-out projection requires data matrices in newdata.", call. = FALSE)
  }
  if (is.null(spec$num_cond)) {
    stop("Held-out projection requires newdata$num_cond.", call. = FALSE)
  }
  if (is.null(ctx$num_cond)) {
    stop("Fitted result is missing num_cond metadata for held-out projection.", call. = FALSE)
  }
  if (!identical(as.integer(spec$num_cond), as.integer(ctx$num_cond))) {
    stop(
      sprintf(
        "Held-out projection requires matching num_cond (got %d, expected %d).",
        as.integer(spec$num_cond),
        as.integer(ctx$num_cond)
      ),
      call. = FALSE
    )
  }

  fit_num_groups <- length(ctx$num_subj_lst %||% integer(0))
  new_num_groups <- length(spec$num_subj_lst %||% integer(0))
  if (fit_num_groups > 0L && new_num_groups != fit_num_groups) {
    stop(
      sprintf(
        "Held-out projection currently requires the same number of groups as the fitted model (got %d, expected %d).",
        new_num_groups,
        fit_num_groups
      ),
      call. = FALSE
    )
  }

  fit_groups <- ctx$groups
  if (!is.null(fit_groups) && !is.null(spec$groups)) {
    if (!identical(as.character(spec$groups), as.character(fit_groups))) {
      stop(
        "Held-out projection currently requires held-out groups to match the fitted group order.",
        call. = FALSE
      )
    }
  }

  n_fit_features <- nrow(result$decomposition$feature_weights)
  feature_mismatch <- which(vapply(spec$datamat_lst, ncol, integer(1)) != n_fit_features)
  if (length(feature_mismatch) > 0L) {
    stop(
      sprintf(
        "Held-out datamat columns must match fitted feature count (%d). Problem groups: %s",
        n_fit_features,
        paste(feature_mismatch, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (type %in% c("design", "both") && method_int %in% c(3L, 5L) && is.null(spec$stacked_behavdata)) {
    stop(
      "Held-out design-score projection for behavior PLS requires behavior data in newdata.",
      call. = FALSE
    )
  }
}

.pls_expand_task_design_scores <- function(v, num_subj_lst, num_cond) {
  k <- as.integer(num_cond)
  num_groups <- length(num_subj_lst)
  group_sizes <- if (!is.list(num_subj_lst)) {
    as.integer(num_subj_lst) * k
  } else {
    vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1))
  }
  group_offsets <- c(0L, cumsum(group_sizes[-num_groups]))

  vsc <- matrix(0, nrow = count_observations(num_subj_lst, k), ncol = ncol(v))

  for (g in seq_len(num_groups)) {
    offset <- group_offsets[g]
    n_vec <- if (!is.list(num_subj_lst)) rep(as.integer(num_subj_lst[g]), k) else as.integer(num_subj_lst[[g]])
    v_group <- v[((g - 1L) * k + 1L):(g * k), , drop = FALSE]

    step <- 0L
    for (cond in seq_len(k)) {
      n <- n_vec[cond]
      subj_rows <- (offset + step + 1L):(offset + step + n)
      vsc[subj_rows, ] <- matrix(v_group[cond, ], nrow = n, ncol = ncol(v_group), byrow = TRUE)
      step <- step + n
    }
  }

  vsc
}

.project_pls_scores_impl <- function(result, spec, method_int, type, ctx) {
  stopifnot(inherits(result, "mva_result"))
  assert_that(inherits(spec, "pls_spec"))

  stacked_datamat <- do.call(rbind, spec$datamat_lst)
  feature_weights <- result$decomposition$feature_weights
  design_weights <- result$decomposition$design_weights

  feature_scores <- NULL
  design_scores <- NULL

  if (type %in% c("feature", "both")) {
    if (method_int == 2L) {
      feature_scores <- stacked_datamat %*% normalize_rows(feature_weights, margin = 2L)
    } else {
      feature_scores <- stacked_datamat %*% feature_weights
    }
  }

  if (type %in% c("design", "both")) {
    if (method_int %in% c(1L, 2L)) {
      design_scores <- .pls_expand_task_design_scores(
        v = design_weights,
        num_subj_lst = spec$num_subj_lst,
        num_cond = spec$num_cond
      )
    } else if (method_int %in% c(3L, 5L)) {
      proj <- pls_get_behavscores(
        stacked_datamat = stacked_datamat,
        stacked_behavdata = spec$stacked_behavdata,
        brainlv = feature_weights,
        behavlv = design_weights,
        num_cond = spec$num_cond,
        num_subj_lst = spec$num_subj_lst,
        cormode = ctx$cormode
      )
      if (is.null(feature_scores)) {
        feature_scores <- proj$usc
      }
      design_scores <- proj$vsc
    }
  }

  switch(
    type,
    feature = feature_scores,
    design = design_scores,
    both = list(feature = feature_scores, design = design_scores)
  )
}
