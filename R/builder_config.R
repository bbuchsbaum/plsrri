#' Builder API: Configure Analysis
#'
#' @description
#' Functions for configuring PLS analysis parameters.
#'
#' @name builder-config
NULL

#' Configure PLS Analysis
#'
#' @description
#' Sets analysis parameters for the PLS specification.
#'
#' @param spec A `pls_spec` object
#' @param method PLS method (character or integer):
#'   - "task" or 1: Mean-Centering Task PLS
#'   - "task_nonrotated" or 2: Non-Rotated Task PLS
#'   - "behavior" or 3: Regular Behavior PLS
#'   - "multiblock" or 4: Regular Multiblock PLS
#'   - "behavior_nonrotated" or 5: Non-Rotated Behavior PLS
#'   - "multiblock_nonrotated" or 6: Non-Rotated Multiblock PLS
#'   - "ws_seed": Alias for task PLS on trial-level within-subject seed
#'     correlation maps from [add_trial_data()]
#'   - "ws_seed_nonrotated": Alias for non-rotated task PLS on
#'     trial-level within-subject seed correlation maps
#' @param nperm Number of permutations (0 = no permutation test)
#' @param nboot Number of bootstrap samples (0 = no bootstrap)
#' @param nsplit Number of split-half iterations (0 = no split-half)
#' @param clim Confidence level for bootstrap (0-100, default 95)
#' @param meancentering Mean-centering type (0-3):
#'   - 0: Within-group centering (default)
#'   - 1: Grand condition mean centering
#'   - 2: Grand mean centering
#'   - 3: Remove all main effects
#' @param cormode Correlation mode for behavior PLS:
#'   - "pearson" or 0: Pearson correlation (default)
#'   - "covariance" or 2: Covariance
#'   - "cosine" or 4: Cosine angle
#'   - "dot" or 6: Dot product
#' @param boot_type Bootstrap type: "strat" (default) or "nonstrat"
#' @param is_struct Logical, structure PLS (don't permute conditions)
#'
#' @return Updated `pls_spec` object
#' @export
#'
#' @examples
#' set.seed(42)
#' data1 <- matrix(rnorm(60 * 50), 60, 50)
#' data2 <- matrix(rnorm(54 * 50), 54, 50)
#'
#' spec <- pls_spec() |>
#'   add_subjects(list(data1, data2), groups = c(20, 18)) |>
#'   add_conditions(3) |>
#'   configure(method = "task", nperm = 100, nboot = 50)
configure <- function(spec,
                       method = NULL,
                       nperm = NULL,
                       nboot = NULL,
                       nsplit = NULL,
                       clim = NULL,
                       meancentering = NULL,
                       cormode = NULL,
                       boot_type = NULL,
                       is_struct = NULL) {

  assert_that(inherits(spec, "pls_spec"))

  # Method
  if (!is.null(method)) {
    method_key <- if (is.character(method)) tolower(method[[1]]) else NULL
    spec$method <- .resolve_method(method)
    .validate_method_requirements(spec)
    if (!is.null(method_key) &&
        method_key %in% c("ws_seed", "ws_seed_pls", "within_subject_seed",
                          "ws_seed_nonrotated", "nonrotated_ws_seed",
                          "within_subject_seed_nonrotated") &&
        is.null(spec$trial_data)) {
      cli::cli_alert_warning(
        "Within-subject seed preprocessing requires trial-level data. Use add_trial_data() before run()."
      )
    }
  }

  # Permutation
  if (!is.null(nperm)) {
    assert_that(is.numeric(nperm), nperm >= 0)
    spec$num_perm <- as.integer(nperm)
  }

  # Bootstrap
  if (!is.null(nboot)) {
    assert_that(is.numeric(nboot), nboot >= 0)
    spec$num_boot <- as.integer(nboot)
  }

  # Split-half
  if (!is.null(nsplit)) {
    assert_that(is.numeric(nsplit), nsplit >= 0)
    spec$num_split <- as.integer(nsplit)
  }

  # Confidence level
  if (!is.null(clim)) {
    assert_that(is.numeric(clim), clim > 0, clim < 100)
    spec$clim <- clim
  }

  # Mean-centering
  if (!is.null(meancentering)) {
    spec$meancentering_type <- .resolve_meancentering(meancentering)
  }

  # Correlation mode
  if (!is.null(cormode)) {
    spec$cormode <- .resolve_cormode(cormode)
  }

  # Bootstrap type
  if (!is.null(boot_type)) {
    assert_that(boot_type %in% c("strat", "nonstrat"))
    spec$boot_type <- boot_type
  }

  # Structure PLS
  if (!is.null(is_struct)) {
    assert_that(is.flag(is_struct))
    spec$is_struct <- is_struct
  }

  spec
}

#' Resolve Method Name to Integer
#'
#' @keywords internal
.resolve_method <- function(method) {
  if (is.numeric(method)) {
    method <- as.integer(method)
    if (!method %in% 1:6) {
      stop("method must be 1-6")
    }
    return(method)
  }

  if (is.character(method)) {
    method <- tolower(method)
    method <- switch(
      method,
      "task" = 1L,
      "mean_centering" = 1L,
      "meancentering" = 1L,
      "ws_seed" = 1L,
      "ws_seed_pls" = 1L,
      "within_subject_seed" = 1L,
      "task_nonrotated" = 2L,
      "nonrotated_task" = 2L,
      "ws_seed_nonrotated" = 2L,
      "nonrotated_ws_seed" = 2L,
      "within_subject_seed_nonrotated" = 2L,
      "behavior" = 3L,
      "behaviour" = 3L,
      "multiblock" = 4L,
      "behavior_nonrotated" = 5L,
      "behaviour_nonrotated" = 5L,
      "nonrotated_behavior" = 5L,
      "multiblock_nonrotated" = 6L,
      "nonrotated_multiblock" = 6L,
      stop("Unknown method: ", method)
    )
    return(method)
  }

  stop("method must be character or integer")
}

#' Resolve Mean-Centering Type
#'
#' @keywords internal
.resolve_meancentering <- function(mc) {
  if (is.numeric(mc)) {
    mc <- as.integer(mc)
    if (!mc %in% 0:3) {
      stop("meancentering must be 0-3")
    }
    return(mc)
  }

  if (is.character(mc)) {
    mc <- tolower(mc)
    mc <- switch(
      mc,
      "within_group" = 0L,
      "within" = 0L,
      "grand_condition" = 1L,
      "condition" = 1L,
      "grand_mean" = 2L,
      "grand" = 2L,
      "all_effects" = 3L,
      "interaction" = 3L,
      stop("Unknown meancentering type: ", mc)
    )
    return(mc)
  }

  stop("meancentering must be character or integer")
}

#' Resolve Correlation Mode
#'
#' @keywords internal
.resolve_cormode <- function(cm) {
  if (is.numeric(cm)) {
    cm <- as.integer(cm)
    if (!cm %in% c(0L, 2L, 4L, 6L)) {
      stop("cormode must be 0, 2, 4, or 6")
    }
    return(cm)
  }

  if (is.character(cm)) {
    cm <- tolower(cm)
    cm <- switch(
      cm,
      "pearson" = 0L,
      "correlation" = 0L,
      "covariance" = 2L,
      "cov" = 2L,
      "cosine" = 4L,
      "dot" = 6L,
      "dot_product" = 6L,
      stop("Unknown cormode: ", cm)
    )
    return(cm)
  }

  stop("cormode must be character or integer")
}

#' Validate Method Requirements
#'
#' @keywords internal
.validate_method_requirements <- function(spec) {
  method <- spec$method

  # Behavior methods need behavior data
  if (method %in% c(3L, 4L, 5L, 6L)) {
    if (is.null(spec$stacked_behavdata)) {
      cli::cli_alert_warning(
        "Method {method} requires behavior data. Use add_behavior() before run()."
      )
    }
  }

  # Non-rotated methods need design contrasts
  if (method %in% c(2L, 5L, 6L)) {
    has_trial_default <- identical(method, 2L) && (isTRUE(spec$.trial_raw) || is.list(spec$trial_data))
    if (is.null(spec$stacked_designdata) && !has_trial_default) {
      cli::cli_alert_warning(
        "Method {method} requires design contrasts. Use add_design() before run()."
      )
    }
  }

  # Trial-level ws-seed preprocessing only supports task PLS
  if (isTRUE(spec$.trial_raw) || is.list(spec$trial_data)) {
    if (method %in% c(3L, 4L, 5L, 6L)) {
      cli::cli_alert_warning(
        "Trial-level seed data is currently supported only with task PLS methods."
      )
    }
    if (is.null(spec$trial_data)) {
      cli::cli_alert_warning(
        "Within-subject seed preprocessing requires trial-level data. Use add_trial_data() before run()."
      )
    }
  }

  invisible(NULL)
}

#' Set Parallel Processing Options
#'
#' @description
#' Configures parallel processing for permutation and bootstrap tests.
#'
#' @param spec A `pls_spec` object
#' @param workers Number of parallel workers (NULL = auto-detect)
#' @param backend Parallel backend: "future" (default) or "sequential"
#'
#' @return Updated `pls_spec` object
#' @export
set_parallel <- function(spec, workers = NULL, backend = "future") {
  assert_that(inherits(spec, "pls_spec"))
  assert_that(backend %in% c("future", "sequential"))

  if (is.null(workers)) {
    workers <- parallel::detectCores() - 1
  }

  spec$.parallel <- list(
    backend = backend,
    workers = workers
  )

  spec
}
