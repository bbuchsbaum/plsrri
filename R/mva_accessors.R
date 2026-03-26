#' Generic Accessors for Multivariate Analysis Results
#'
#' @description
#' Method-neutral accessors that work on any `mva_result` object.
#' PLS-specific accessors (salience, bsr, etc.) are retained as aliases
#' and continue to work on `pls_result` objects unchanged.
#'
#' @name mva-accessors
NULL

# --- Feature Weights (brain loadings) ---

#' Extract Feature Weights
#'
#' @description
#' Returns the feature-space loadings (brain weights) from any multivariate
#' analysis result. For PLS, these are the saliences (u matrix).
#'
#' @param x An `mva_result` or `pls_result` object
#' @param k Component index or vector (NULL = all)
#' @param as_neurovol Convert to NeuroVol if mask available
#' @param ... Additional arguments
#'
#' @return Matrix of feature weights, or NeuroVol
#' @export
feature_weights <- function(x, k = NULL, as_neurovol = FALSE, ...) {
  UseMethod("feature_weights")
}

#' @export
feature_weights.mva_result <- function(x, k = NULL, as_neurovol = FALSE, ...) {
  fw <- x$decomposition$feature_weights
  if (!is.null(k)) {
    k <- as.integer(k)
    fw <- fw[, k, drop = FALSE]
  }
  if (as_neurovol && !is.null(x$mask)) {
    fw <- .matrix_to_neurovol(fw, x$mask)
  }
  fw
}

#' @export
feature_weights.pls_result <- function(x, k = NULL, as_neurovol = FALSE, ...) {
  salience(x, lv = k, as_neurovol = as_neurovol)
}

# --- Design Weights ---

#' Extract Design/Behavior Weights
#'
#' @description
#' Returns the design-space loadings from any multivariate analysis result.
#' For PLS, these are the v matrix (design or behavior loadings).
#'
#' @param x An `mva_result` or `pls_result` object
#' @param k Component index or vector (NULL = all)
#' @param ... Additional arguments
#'
#' @return Matrix of design weights
#' @export
design_weights <- function(x, k = NULL, ...) {
  UseMethod("design_weights")
}

#' @export
design_weights.mva_result <- function(x, k = NULL, ...) {
  dw <- x$decomposition$design_weights
  if (is.null(dw)) return(NULL)
  if (!is.null(k)) {
    k <- as.integer(k)
    dw <- dw[, k, drop = FALSE]
  }
  dw
}

#' @export
design_weights.pls_result <- function(x, k = NULL, ...) {
  loadings(x, lv = k)
}

# --- Importance ---

#' Extract Component Importance
#'
#' @description
#' Returns the importance measure for each component. For PLS, these are
#' singular values. For CPCA, eigenvalues. For ICA, explained variance.
#'
#' @param x An `mva_result` or `pls_result` object
#' @param normalize Return as proportion of total (percentage)
#' @param ... Additional arguments
#'
#' @return Numeric vector of importance values
#' @export
importance <- function(x, normalize = FALSE, ...) {
  UseMethod("importance")
}

#' @export
importance.mva_result <- function(x, normalize = FALSE, ...) {
  imp <- x$decomposition$importance
  if (normalize) {
    imp <- (imp^2) / sum(imp^2) * 100
    method_obj <- tryCatch(get_method(x$method), error = function(e) NULL)
    comp_name <- if (!is.null(method_obj)) {
      method_obj$capabilities$component_name %||% "C"
    } else "C"
    names(imp) <- paste0(comp_name, seq_along(imp))
  }
  imp
}

#' @export
importance.pls_result <- function(x, normalize = FALSE, ...) {
  singular_values(x, normalize = normalize)
}

# --- Stability (Bootstrap Ratios) ---

#' Extract Stability Estimates
#'
#' @description
#' Returns bootstrap-based stability for feature weights. For PLS, these
#' are bootstrap ratios (BSR = salience / SE). Other methods may use
#' different stability metrics.
#'
#' @param x An `mva_result` or `pls_result` object
#' @param k Component index or vector (NULL = all)
#' @param threshold Optional threshold for masking
#' @param as_neurovol Convert to NeuroVol if mask available
#' @param ... Additional arguments
#'
#' @return Matrix of stability values, or NeuroVol
#' @export
stability <- function(x, k = NULL, threshold = NULL, as_neurovol = FALSE, ...) {
  UseMethod("stability")
}

#' @export
stability.mva_result <- function(x, k = NULL, threshold = NULL,
                                  as_neurovol = FALSE, ...) {
  if (is.null(x$boot_result)) {
    stop("No bootstrap result available.", call. = FALSE)
  }
  # For PLS-family results, use compare_u (bootstrap ratios)
  bsr_mat <- x$boot_result$compare_u
  if (is.null(bsr_mat)) {
    stop("No stability estimates available in bootstrap result.", call. = FALSE)
  }
  if (!is.null(k)) {
    k <- as.integer(k)
    bsr_mat <- bsr_mat[, k, drop = FALSE]
  }
  if (!is.null(threshold)) {
    bsr_mat[abs(bsr_mat) < threshold] <- 0
  }
  if (as_neurovol && !is.null(x$mask)) {
    bsr_mat <- .matrix_to_neurovol(bsr_mat, x$mask)
  }
  bsr_mat
}

#' @export
stability.pls_result <- function(x, k = NULL, threshold = NULL,
                                  as_neurovol = FALSE, ...) {
  bsr(x, lv = k, threshold = threshold, as_neurovol = as_neurovol)
}

# --- Component Count ---

#' Get Number of Components
#'
#' @description
#' Returns the number of components (latent variables, principal components,
#' independent components, etc.) in the result.
#'
#' @param x An `mva_result` or `pls_result` object
#' @param ... Additional arguments
#'
#' @return Integer
#' @export
n_components <- function(x, ...) {
  UseMethod("n_components")
}

#' @export
n_components.mva_result <- function(x, ...) {
  length(x$decomposition$importance)
}

#' @export
n_components.pls_result <- function(x, ...) {
  n_lv(x)
}

# --- Scores (generic, works on mva_result) ---

#' @export
scores.mva_result <- function(x, type = "feature", k = NULL, ...) {
  type <- match.arg(type, c("feature", "design", "brain", "behavior"))
  # Map PLS-compatible type names
  if (type %in% c("brain", "feature")) {
    sc <- x$decomposition$scores_feature
  } else {
    sc <- x$decomposition$scores_design
  }
  if (is.null(sc)) {
    stop(sprintf("No %s scores available", type), call. = FALSE)
  }
  if (!is.null(k)) {
    k <- as.integer(k)
    sc <- sc[, k, drop = FALSE]
  }
  sc
}

# --- Significance (generic, works on mva_result) ---

#' @export
significance.mva_result <- function(x, k = NULL, threshold = NULL, ...) {
  if (is.null(x$perm_result)) {
    stop("No permutation result available.", call. = FALSE)
  }
  sprob <- x$perm_result$sprob
  method_obj <- tryCatch(get_method(x$method), error = function(e) NULL)
  comp_name <- if (!is.null(method_obj)) {
    method_obj$capabilities$component_name %||% "C"
  } else "C"
  names(sprob) <- paste0(comp_name, seq_along(sprob))

  if (!is.null(k)) {
    k <- as.integer(k)
    sprob <- sprob[k]
  }
  if (is.null(threshold)) return(sprob)

  ve <- importance(x, normalize = TRUE)[if (!is.null(k)) k else seq_along(sprob)]
  data.frame(
    component = names(sprob),
    pvalue = round(sprob, 3),
    significant = sprob < threshold,
    var_explained = paste0(round(ve, 1), "%")
  )
}

# --- Confidence (generic, works on mva_result) ---

#' @export
confidence.mva_result <- function(x, what = "feature_weights", lv = NULL) {
  if (is.null(x$boot_result)) {
    stop("No bootstrap result available.", call. = FALSE)
  }
  what <- match.arg(what, c("feature_weights", "salience", "correlation", "brain_scores"))

  boot <- x$boot_result
  if (what %in% c("feature_weights", "salience")) {
    se <- boot$u_se
    fw <- x$decomposition$feature_weights
    z <- stats::qnorm(1 - (100 - (boot$clim %||% 95)) / 200)
    lower <- fw - z * se
    upper <- fw + z * se
  } else if (what == "correlation") {
    lower <- boot$llcorr
    upper <- boot$ulcorr
    if (is.null(lower)) stop("No correlation CIs available", call. = FALSE)
  } else {
    lower <- boot$llusc
    upper <- boot$ulusc
    if (is.null(lower)) stop("No brain score CIs available", call. = FALSE)
  }

  if (!is.null(lv)) {
    lv <- as.integer(lv)
    if (is.matrix(lower)) {
      lower <- lower[, lv, drop = FALSE]
      upper <- upper[, lv, drop = FALSE]
    } else {
      lower <- lower[lv]
      upper <- upper[lv]
    }
  }
  list(lower = lower, upper = upper, clim = boot$clim %||% 95)
}

# --- Feature Count (generic) ---

#' @export
n_features.mva_result <- function(x, ...) {
  nrow(x$decomposition$feature_weights)
}
