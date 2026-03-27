#' Cross-Correlation Operations
#'
#' @description
#' Compute cross-correlation between design matrix and data matrix.
#' Ported from MATLAB rri_xcor.m
#'
#' @name pls-xcor
NULL

#' Cross-Correlation of Two Matrices
#'
#' @description
#' Computes the cross-correlation (or covariance, cosine angle, or dot product)
#' between a design matrix and a data matrix. Both matrices must have the same
#' number of rows.
#'
#' @param design Design matrix (n_obs x n_design)
#' @param datamat Data matrix (n_obs x n_features)
#' @param cormode Correlation mode:
#' \describe{
#'   \item{0}{Pearson correlation (default)}
#'   \item{2}{Covariance}
#'   \item{4}{Cosine angle}
#'   \item{6}{Dot product}
#' }
#'
#' @return Cross-correlation matrix (n_design x n_features)
#' @export
#'
#' @examples
#' design <- matrix(rnorm(30), 10, 3)
#' datamat <- matrix(rnorm(100), 10, 10)
#' xcor <- pls_xcor(design, datamat, cormode = 0)
pls_xcor <- function(design, datamat, cormode = 0L) {
  assert_that(is.matrix(design))
  assert_that(is.matrix(datamat))

  n_rows <- nrow(datamat)
  n_design_rows <- nrow(design)

  if (n_rows != n_design_rows) {
    stop("design and datamat must have the same number of rows")
  }

  # MATLAB missnk_rri_xcor: if any non-finite values are present, switch to
  # the missing-data implementation for the requested cormode.
  if (any(!is.finite(design)) || any(!is.finite(datamat))) {
    return(.pls_xcor_missnk(design, datamat, cormode))
  }

  if (.plsrri_fast_paths_enabled("xcor")) {
    return(pls_xcor_cpp(design, datamat, as.integer(cormode)))
  }

  switch(
    as.character(cormode),

    # Pearson correlation (cormode = 0)
    "0" = {
      # Center and scale datamat
      avg <- colMeans(datamat)
      stdev <- apply(datamat, 2, sd)

      # Handle zero variance columns
      zero_var <- which(stdev == 0)
      if (length(zero_var) > 0) {
        datamat[, zero_var] <- 0
        avg[zero_var] <- 0
        stdev[zero_var] <- 1
      }

      # Z-score datamat
      datamat <- sweep(datamat, 2, avg, "-")
      datamat <- sweep(datamat, 2, stdev, "/")

      # Center and scale design
      davg <- colMeans(design)
      dstdev <- apply(design, 2, sd)

      # Handle zero variance columns
      zero_var_d <- which(dstdev == 0)
      if (length(zero_var_d) > 0) {
        design[, zero_var_d] <- 0
        davg[zero_var_d] <- 0
        dstdev[zero_var_d] <- 1
      }

      # Z-score design
      design <- sweep(design, 2, davg, "-")
      design <- sweep(design, 2, dstdev, "/")

      # Cross-product normalized by (n-1)
      xprod <- crossprod(design, datamat)
      xprod / (n_rows - 1)
    },

    # Covariance (cormode = 2)
    "2" = {
      # Center only (no scaling)
      avg <- colMeans(datamat)
      davg <- colMeans(design)

      datamat <- sweep(datamat, 2, avg, "-")
      design <- sweep(design, 2, davg, "-")

      xprod <- crossprod(design, datamat)
      xprod / (n_rows - 1)
    },

    # Cosine angle (cormode = 4)
    "4" = {
      # Scale but don't center
      stdev <- apply(datamat, 2, sd)
      zero_var <- which(stdev == 0)
      if (length(zero_var) > 0) {
        datamat[, zero_var] <- 0
        stdev[zero_var] <- 1
      }

      dstdev <- apply(design, 2, sd)
      zero_var_d <- which(dstdev == 0)
      if (length(zero_var_d) > 0) {
        design[, zero_var_d] <- 0
        dstdev[zero_var_d] <- 1
      }

      datamat <- sweep(datamat, 2, stdev, "/")
      design <- sweep(design, 2, dstdev, "/")

      crossprod(design, datamat)
    },

    # Dot product (cormode = 6)
    "6" = {
      crossprod(design, datamat)
    },

    stop("cormode must be 0, 2, 4, or 6")
  )
}

# --- Missing-data helpers (MATLAB missnk_*) ---

# Treat any non-finite value (NA/NaN/Inf) as missing.
# @keywords internal
.pls_miss_to_na <- function(X) {
  X[!is.finite(X)] <- NA_real_
  X
}

# Standardized sum that corrects for missingness (MATLAB misssum).
# Returns a vector of column sums for matrices.
# @keywords internal
.pls_misssum <- function(X) {
  if (is.vector(X)) {
    X <- as.numeric(X)
    miss <- !is.finite(X)
    X0 <- X
    X0[miss] <- 0
    n_real <- length(X0) - sum(miss)
    if (n_real == 0L) return(NA_real_)
    weight <- length(X0)
    return(weight * sum(X0) / n_real)
  }

  if (!is.matrix(X)) {
    stop("X must be a vector or matrix")
  }

  miss <- !is.finite(X)
  X0 <- X
  X0[miss] <- 0

  n_real <- nrow(X0) - colSums(miss)
  weight <- nrow(X0)

  out <- weight * colSums(X0) / n_real
  out[n_real == 0] <- NA_real_
  out
}

# Mean that ignores missing values (MATLAB missnk_mean along dim=1).
# @keywords internal
.pls_missmean <- function(X, margin = 1L) {
  X <- .pls_miss_to_na(X)
  if (margin == 1L) {
    return(colMeans(X, na.rm = TRUE))
  }
  if (margin == 2L) {
    return(rowMeans(X, na.rm = TRUE))
  }
  stop("margin must be 1 (columns) or 2 (rows)")
}

# Standard deviation that ignores missing values (MATLAB missstd).
# @keywords internal
.pls_misssd <- function(X) {
  X <- .pls_miss_to_na(X)
  apply(X, 2, stats::sd, na.rm = TRUE)
}

# Matrix product with missingness correction (MATLAB missmult).
# @keywords internal
.pls_missmult <- function(A, B) {
  assert_that(is.matrix(A))
  assert_that(is.matrix(B))
  if (ncol(A) != nrow(B)) {
    stop("A and B have incompatible dimensions for multiplication")
  }

  if (!(any(!is.finite(A)) || any(!is.finite(B)))) {
    return(A %*% B)
  }

  k <- ncol(A)

  A0 <- A
  B0 <- B
  A0[!is.finite(A0)] <- 0
  B0[!is.finite(B0)] <- 0

  sum_prod <- A0 %*% B0

  Af <- matrix(as.double(is.finite(A)), nrow = nrow(A), ncol = ncol(A))
  Bf <- matrix(as.double(is.finite(B)), nrow = nrow(B), ncol = ncol(B))
  n_real <- Af %*% Bf

  out <- (k * sum_prod) / n_real
  out[n_real == 0] <- NA_real_
  out
}

# Missing-data cross-correlation (MATLAB missnk_rri_xcor).
# @keywords internal
.pls_xcor_missnk <- function(design, datamat, cormode = 0L) {
  assert_that(is.matrix(design))
  assert_that(is.matrix(datamat))

  r <- nrow(datamat)
  dr <- nrow(design)
  if (r != dr) {
    stop("design and datamat must have the same number of rows")
  }

  mode <- switch(
    as.character(cormode),
    `0` = 1L,
    `2` = 3L,
    `4` = 5L,
    `6` = 7L,
    `1` = 1L,
    `3` = 3L,
    `5` = 5L,
    `7` = 7L,
    stop("cormode must be 0, 2, 4, or 6")
  )

  if (mode == 1L) {
    # Pearson correlation with missing data
    avg <- .pls_missmean(datamat, 1L)
    stdev <- .pls_misssd(datamat)

    zero_var <- which(stdev == 0)
    if (length(zero_var) > 0) {
      datamat[, zero_var] <- 0
      avg[zero_var] <- 0
      stdev[zero_var] <- 1
    }

    davg <- .pls_missmean(design, 1L)
    dstdev <- .pls_misssd(design)

    zero_var_d <- which(dstdev == 0)
    if (length(zero_var_d) > 0) {
      design[, zero_var_d] <- 0
      davg[zero_var_d] <- 0
      dstdev[zero_var_d] <- 1
    }

    datamat <- sweep(datamat, 2, avg, "-")
    datamat <- sweep(datamat, 2, stdev, "/")
    design <- sweep(design, 2, davg, "-")
    design <- sweep(design, 2, dstdev, "/")

    xprod <- .pls_missmult(t(design), datamat)
    xprod / (r - 1)
  } else if (mode == 3L) {
    # Covariance with missing data
    avg <- .pls_missmean(datamat, 1L)
    davg <- .pls_missmean(design, 1L)

    datamat <- sweep(datamat, 2, avg, "-")
    design <- sweep(design, 2, davg, "-")

    xprod <- .pls_missmult(t(design), datamat)
    xprod / (r - 1)
  } else if (mode == 5L) {
    # Cosine angle with missing data
    stdev <- .pls_misssd(datamat)
    zero_var <- which(stdev == 0)
    if (length(zero_var) > 0) {
      datamat[, zero_var] <- 0
      stdev[zero_var] <- 1
    }

    dstdev <- .pls_misssd(design)
    zero_var_d <- which(dstdev == 0)
    if (length(zero_var_d) > 0) {
      design[, zero_var_d] <- 0
      dstdev[zero_var_d] <- 1
    }

    datamat <- sweep(datamat, 2, stdev, "/")
    design <- sweep(design, 2, dstdev, "/")

    .pls_missmult(t(design), datamat)
  } else {
    # Dot product with missing data
    .pls_missmult(t(design), datamat)
  }
}
