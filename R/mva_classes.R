#' Multivariate Analysis Data Classes
#'
#' @description
#' S3 classes for method-neutral multivariate analysis results.
#' These wrap the universal output shape that all methods produce.
#'
#' @name mva-classes
NULL

#' Create a Multivariate Decomposition Object
#'
#' @description
#' The universal output of `MvaMethod$fit()`. Every analysis method must
#' produce this shape, mapping its native output to these fields.
#'
#' @param feature_weights P x K matrix of feature loadings (brain weights)
#' @param design_weights D x K matrix of design/behavior loadings, or NULL
#' @param importance Length-K vector (singular values, eigenvalues, etc.)
#' @param scores_feature N x K matrix of feature-space scores (brain scores)
#' @param scores_design N x K matrix of design-space scores, or NULL
#' @param extra Named list of method-specific extras
#' @param method Character string identifying the method
#'
#' @return An `mva_decomposition` object
#' @export
new_mva_decomposition <- function(feature_weights,
                                   design_weights = NULL,
                                   importance,
                                   scores_feature = NULL,
                                   scores_design = NULL,
                                   extra = list(),
                                   method = NULL) {
  stopifnot(is.matrix(feature_weights) || is.numeric(feature_weights))
  stopifnot(is.numeric(importance))

  if (!is.matrix(feature_weights)) {
    feature_weights <- as.matrix(feature_weights)
  }

  structure(
    list(
      feature_weights = feature_weights,
      design_weights  = design_weights,
      importance      = importance,
      scores_feature  = scores_feature,
      scores_design   = scores_design,
      extra           = extra,
      method          = method
    ),
    class = "mva_decomposition"
  )
}

#' @export
print.mva_decomposition <- function(x, ...) {
  K <- length(x$importance)
  P <- nrow(x$feature_weights)
  cli::cli_h2("Multivariate Decomposition")
  if (!is.null(x$method)) {
    cli::cli_text("Method: {x$method}")
  }
  cli::cli_text("Components: {K}")
  cli::cli_text("Features: {P}")
  if (!is.null(x$scores_feature)) {
    cli::cli_text("Observations: {nrow(x$scores_feature)}")
  }
  if (!is.null(x$design_weights)) {
    cli::cli_text("Design variables: {nrow(x$design_weights)}")
  }
  if (length(x$extra) > 0) {
    cli::cli_text("Extra fields: {paste(names(x$extra), collapse = ', ')}")
  }
  invisible(x)
}


#' Create a Multivariate Analysis Result
#'
#' @description
#' Wraps an `mva_decomposition` with inference results (permutation,
#' bootstrap, split-half) and metadata. This is the top-level result
#' object returned by the method-neutral analysis path.
#'
#' @param decomposition An `mva_decomposition` object
#' @param perm_result Permutation test result, or NULL
#' @param boot_result Bootstrap result, or NULL
#' @param split_result Split-half result, or NULL
#' @param spec The analysis specification used
#' @param method Character string identifying the method
#' @param mask Brain mask (NeuroVol), or NULL
#'
#' @return An `mva_result` object with dual class for method dispatch
#' @export
new_mva_result <- function(decomposition,
                            perm_result = NULL,
                            boot_result = NULL,
                            split_result = NULL,
                            spec = NULL,
                            method = NULL,
                            mask = NULL) {
  stopifnot(inherits(decomposition, "mva_decomposition"))

  method_name <- method %||% decomposition$method %||% "unknown"

  structure(
    list(
      decomposition = decomposition,
      perm_result   = perm_result,
      boot_result   = boot_result,
      split_result  = split_result,
      spec          = spec,
      method        = method_name,
      mask          = mask
    ),
    class = c(paste0("mva_", method_name), "mva_result")
  )
}

#' @export
print.mva_result <- function(x, ...) {
  method_obj <- tryCatch(get_method(x$method), error = function(e) NULL)
  label <- if (!is.null(method_obj)) method_obj$label else x$method
  comp_name <- if (!is.null(method_obj)) {
    method_obj$capabilities$component_name %||% "Component"
  } else {
    "Component"
  }

  cli::cli_h1("Multivariate Analysis Result")
  cli::cli_text("Method: {label}")

  d <- x$decomposition
  K <- length(d$importance)
  cli::cli_text("{comp_name}s: {K}")
  cli::cli_text("Features: {nrow(d$feature_weights)}")

  if (!is.null(x$perm_result)) {
    cli::cli_text("Permutation test: yes")
  }
  if (!is.null(x$boot_result)) {
    cli::cli_text("Bootstrap: yes")
  }
  if (!is.null(x$split_result)) {
    cli::cli_text("Split-half: yes")
  }

  invisible(x)
}
