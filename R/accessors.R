#' Result Accessors for PLS
#'
#' @description
#' Functions for extracting components from PLS results.
#' These provide a clean interface for accessing saliences, bootstrap ratios,
#' scores, loadings, and other result components.
#'
#' @name pls-accessors
NULL

#' Extract Saliences (Brain Loadings)
#'
#' @description
#' Extracts the salience matrix (u) from a PLS result.
#' Optionally converts to NeuroVol if mask is available.
#'
#' @param x A `pls_result` object
#' @param lv Latent variable index or vector of indices (NULL = all)
#' @param as_neurovol Logical, convert to SparseNeuroVol if mask available
#'
#' @return Matrix of saliences, or SparseNeuroVol if as_neurovol=TRUE
#' @export
#'
#' @examples
#' set.seed(42)
#' data1 <- matrix(rnorm(60 * 50), 60, 50)
#' data2 <- matrix(rnorm(54 * 50), 54, 50)
#' result <- quick_pls(list(data1, data2), c(20, 18), 3, progress = FALSE)
#' sal <- salience(result, lv = 1)
salience <- function(x, lv = NULL, as_neurovol = FALSE) {
  UseMethod("salience")
}

#' @export
salience.pls_result <- function(x, lv = NULL, as_neurovol = FALSE) {

  u <- x$u

  # Select specific LVs
  if (!is.null(lv)) {
    lv <- as.integer(lv)
    if (any(lv > ncol(u) | lv < 1)) {
      stop("lv index out of range")
    }
    u <- u[, lv, drop = FALSE]
  }

  # Convert to NeuroVol
  if (as_neurovol && !is.null(x$mask)) {
    u <- .matrix_to_neurovol(u, x$mask)
  }

  u
}

#' Extract Bootstrap Ratios
#'
#' @description
#' Extracts the bootstrap ratio (BSR) from a PLS result.
#' BSR = salience / standard error, similar to a z-score.
#'
#' @param x A `pls_result` object
#' @param lv Latent variable index or vector of indices (NULL = all)
#' @param threshold Optional threshold for masking (e.g., |BSR| > 3)
#' @param as_neurovol Logical, convert to SparseNeuroVol if mask available
#'
#' @return Matrix of bootstrap ratios, or SparseNeuroVol
#' @export
#'
#' @examples
#' set.seed(42)
#' data1 <- matrix(rnorm(60 * 50), 60, 50)
#' data2 <- matrix(rnorm(54 * 50), 54, 50)
#' result <- quick_pls(list(data1, data2), c(20, 18), 3, nboot = 20, progress = FALSE)
#' bsr_map <- bsr(result, lv = 1, threshold = 2)
bsr <- function(x, lv = NULL, threshold = NULL, as_neurovol = FALSE) {
  UseMethod("bsr")
}

#' @export
bsr.pls_result <- function(x, lv = NULL, threshold = NULL, as_neurovol = FALSE) {

  if (is.null(x$boot_result)) {
    stop("No bootstrap result available. Run with num_boot > 0.")
  }

  compare_u <- x$boot_result$compare_u

  # Select specific LVs
  if (!is.null(lv)) {
    lv <- as.integer(lv)
    if (any(lv > ncol(compare_u) | lv < 1)) {
      stop("lv index out of range")
    }
    compare_u <- compare_u[, lv, drop = FALSE]
  }

  # Apply threshold
  if (!is.null(threshold)) {
    compare_u[abs(compare_u) < threshold] <- 0
  }

  # Convert to NeuroVol
  if (as_neurovol && !is.null(x$mask)) {
    compare_u <- .matrix_to_neurovol(compare_u, x$mask)
  }

  compare_u
}

#' Extract Scores
#'
#' @description
#' Extracts brain scores (usc) or design/behavior scores (vsc) from a PLS result.
#'
#' @param x A `pls_result` object
#' @param type "brain" (default), "design", or "behavior"
#' @param lv Latent variable index or vector of indices (NULL = all)
#'
#' @return Matrix of scores
#' @export
#'
#' @examples
#' set.seed(42)
#' data1 <- matrix(rnorm(60 * 50), 60, 50)
#' data2 <- matrix(rnorm(54 * 50), 54, 50)
#' result <- quick_pls(list(data1, data2), c(20, 18), 3, progress = FALSE)
#' brain_scores <- scores(result, type = "brain")
#' design_scores <- scores(result, type = "design")
scores <- function(x, type = "brain", lv = NULL) {
  UseMethod("scores")
}

#' @export
scores.pls_result <- function(x, type = "brain", lv = NULL) {

  type <- match.arg(type, c("brain", "design", "behavior"))

  sc <- switch(
    type,
    brain = x$usc,
    design = x$vsc,
    behavior = x$vsc
  )

  if (is.null(sc)) {
    stop(sprintf("No %s scores available", type))
  }

  # Select specific LVs
  if (!is.null(lv)) {
    lv <- as.integer(lv)
    if (any(lv > ncol(sc) | lv < 1)) {
      stop("lv index out of range")
    }
    sc <- sc[, lv, drop = FALSE]
  }

  sc
}

#' Extract Loadings
#'
#' @description
#' Extracts design or behavior loadings (v) from a PLS result.
#' For behavior PLS, loadings represent the correlation between behavior
#' measures and brain scores.
#'
#' @param x A `pls_result` object
#' @param type "design" (default) or "behavior"
#' @param lv Latent variable index or vector of indices (NULL = all)
#'
#' @return Matrix of loadings
#' @export
loadings <- function(x, type = "design", lv = NULL) {
  UseMethod("loadings")
}

#' @export
loadings.pls_result <- function(x, type = "design", lv = NULL) {

  type <- match.arg(type, c("design", "behavior"))

  if (type == "design") {
    v <- x$v
  } else {
    # For behavior PLS, lvcorrs are the loadings
    v <- x$lvcorrs
    if (is.null(v)) {
      v <- x$v
    }
  }

  if (is.null(v)) {
    stop(sprintf("No %s loadings available", type))
  }

  # Select specific LVs
  if (!is.null(lv)) {
    lv <- as.integer(lv)
    if (any(lv > ncol(v) | lv < 1)) {
      stop("lv index out of range")
    }
    v <- v[, lv, drop = FALSE]
  }

  v
}

#' Extract Singular Values
#'
#' @description
#' Extracts singular values from a PLS result.
#'
#' @param x A `pls_result` object
#' @param normalize Logical, return variance explained (default FALSE)
#'
#' @return Vector of singular values or variance explained
#' @export
singular_values <- function(x, normalize = FALSE) {
  UseMethod("singular_values")
}

#' @export
singular_values.pls_result <- function(x, normalize = FALSE) {
  s <- x$s

  if (normalize) {
    s <- (s^2) / sum(s^2) * 100
    names(s) <- paste0("LV", seq_along(s))
  }

  s
}

#' Get Significance Values (P-values)
#'
#' @description
#' Extracts permutation-based p-values for each latent variable.
#'
#' @param x A `pls_result` object
#' @param lv Latent variable index or vector of indices (NULL = all)
#' @param threshold P-value threshold for significance (default 0.05)
#'
#' @return Named vector of p-values, or data frame with significance info
#' @export
#'
#' @examples
#' set.seed(42)
#' data1 <- matrix(rnorm(60 * 50), 60, 50)
#' data2 <- matrix(rnorm(54 * 50), 54, 50)
#' result <- quick_pls(list(data1, data2), c(20, 18), 3, nperm = 20, progress = FALSE)
#' pvals <- significance(result)
#' sig_lvs <- significance(result, threshold = 0.05)
significance <- function(x, lv = NULL, threshold = NULL) {
  UseMethod("significance")
}

#' @export
significance.pls_result <- function(x, lv = NULL, threshold = NULL) {

  if (is.null(x$perm_result)) {
    stop("No permutation result available. Run with num_perm > 0.")
  }

  sprob <- x$perm_result$sprob
  names(sprob) <- paste0("LV", seq_along(sprob))

  # Select specific LVs
  if (!is.null(lv)) {
    lv <- as.integer(lv)
    sprob <- sprob[lv]
  }

  # Return just p-values or significance summary
  if (is.null(threshold)) {
    return(sprob)
  }

  # Return significance summary
  ve <- singular_values(x, normalize = TRUE)[if (!is.null(lv)) lv else seq_along(sprob)]
  data.frame(
    lv = names(sprob),
    pvalue = round(sprob, 3),
    significant = sprob < threshold,
    var_explained = paste0(round(ve, 1), "%")
  )
}

#' Get Confidence Intervals
#'
#' @description
#' Extracts bootstrap confidence intervals for saliences, correlations,
#' or brain scores.
#'
#' @param x A `pls_result` object
#' @param what What to get CIs for: "salience", "correlation", or "brain_scores"
#' @param lv Latent variable index or vector of indices (NULL = all)
#'
#' @return List with lower and upper bounds
#' @export
confidence <- function(x, what = "salience", lv = NULL) {
  UseMethod("confidence")
}

#' @export
confidence.pls_result <- function(x, what = "salience", lv = NULL) {

  if (is.null(x$boot_result)) {
    stop("No bootstrap result available. Run with num_boot > 0.")
  }

  what <- match.arg(what, c("salience", "correlation", "brain_scores"))

  boot <- x$boot_result

  if (what == "salience") {
    # Use SE to construct CI
    se <- boot$u_se
    u <- x$u

    z <- qnorm(1 - (100 - boot$clim) / 200)
    lower <- u - z * se
    upper <- u + z * se

  } else if (what == "correlation") {
    if (is.null(boot$orig_corr)) {
      stop("No correlation CIs available (behavior PLS only)")
    }
    lower <- boot$llcorr
    upper <- boot$ulcorr

  } else if (what == "brain_scores") {
    if (is.null(boot$orig_usc)) {
      stop("No brain score CIs available")
    }
    lower <- boot$llusc
    upper <- boot$ulusc
  }

  # Select specific LVs
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

  list(lower = lower, upper = upper, clim = boot$clim)
}

#' Convert Matrix to NeuroVol
#'
#' @description
#' Helper function to convert a salience/BSR matrix to NeuroVol using mask.
#'
#' @keywords internal
.matrix_to_neurovol <- function(mat, mask) {
  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    warning("neuroim2 not available, returning matrix")
    return(mat)
  }

  # Get mask dimensions and space
  mask_dims <- dim(mask)

  if (ncol(mat) == 1) {
    # Single volume
    vol <- array(0, dim = mask_dims)
    vol[mask > 0] <- mat[, 1]
    neuroim2::NeuroVol(vol, neuroim2::space(mask))
  } else {
    # Multiple volumes (one per LV)
    vol_list <- lapply(seq_len(ncol(mat)), function(i) {
      vol <- array(0, dim = mask_dims)
      vol[mask > 0] <- mat[, i]
      neuroim2::NeuroVol(vol, neuroim2::space(mask))
    })
    neuroim2::NeuroVec(vol_list)
  }
}

#' Get Number of Latent Variables
#'
#' @description
#' Returns the number of latent variables in a PLS result.
#'
#' @param x A `pls_result` object
#'
#' @return Integer
#' @export
n_lv <- function(x) {
  UseMethod("n_lv")
}

#' @export
n_lv.pls_result <- function(x) {
  length(x$s)
}

#' Get Number of Voxels/Features
#'
#' @description
#' Returns the number of voxels or features in a PLS result.
#'
#' @param x A `pls_result` object
#'
#' @return Integer
#' @export
n_features <- function(x) {
  UseMethod("n_features")
}

#' @export
n_features.pls_result <- function(x) {
  nrow(x$u)
}
