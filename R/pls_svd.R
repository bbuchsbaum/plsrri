#' SVD Operations for PLS
#'
#' @description
#' SVD-related functions including handling of missing values.
#' Ported from MATLAB misssvd.m
#'
#' @name pls-svd
NULL

#' SVD with Missing Value Handling
#'
#' @description
#' Performs SVD on a matrix, iteratively imputing missing values (NaN/NA).
#' Based on the algorithm from misssvd.m (Claus A. Andersson).
#'
#' @param X Matrix to decompose
#' @param truncate Logical; if TRUE, truncate to rank of X (economy SVD)
#' @param conv_lim Convergence limit for missing value iteration
#' @param max_iter Maximum iterations for missing value imputation
#'
#' @return List with components:
#' \describe{
#'   \item{u}{Left singular vectors}
#'   \item{d}{Singular values (as vector)}
#'   \item{v}{Right singular vectors}
#' }
#'
#' @export
#'
#' @examples
#' X <- matrix(rnorm(100), 10, 10)
#' X[sample(100, 5)] <- NA
#' result <- misssvd(X)
misssvd <- function(X, truncate = TRUE, conv_lim = 1e-12, max_iter = 100) {
  assert_that(is.matrix(X))

  conv_lim_miss <- 100 * conv_lim
  dims <- dim(X)

  # Check for missing values
  missing_exists <- any(!is.finite(X))

  if (!missing_exists) {
    # No missing values - standard SVD
    if (truncate) {
      result <- svd(X, nu = min(dims), nv = min(dims))
    } else {
      result <- svd(X)
    }
    return(list(u = result$u, d = result$d, v = result$v))
  }

  # Handle missing data through iterative imputation
  miss_idx <- which(is.na(X) | !is.finite(X))

  # Get row and column indices of missing values
  miss_rc <- arrayInd(miss_idx, dims)
  miss_rows <- miss_rc[, 1]
  miss_cols <- miss_rc[, 2]

  # Initial imputation using row and column means
  row_means <- rowMeans(X, na.rm = TRUE) / 3
  col_means <- colMeans(X, na.rm = TRUE) / 3

  for (i in seq_along(miss_idx)) {
    r <- miss_rows[i]
    c <- miss_cols[i]
    X[r, c] <- row_means[r] + col_means[c]
  }

  # Handle any remaining NaN (when entire row or column is missing)
  mean_of_means <- mean(c(row_means, col_means), na.rm = TRUE) / 2
  still_na <- which(is.na(X))
  if (length(still_na) > 0) {
    X[still_na] <- mean_of_means
  }

  # Initial SVD
  if (truncate) {
    result <- svd(X, nu = min(dims), nv = min(dims))
  } else {
    result <- svd(X)
  }

  # Reconstruct and impute
  Xm <- result$u %*% diag(result$d, nrow = length(result$d)) %*% t(result$v)
  X[miss_idx] <- Xm[miss_idx]

  # Track convergence
  ssmis_old <- sum(Xm[miss_idx]^2)
  sstot_old <- sum(X^2)
  ssreal_old <- sstot_old - ssmis_old

  # Iterate until convergence
  for (iter in seq_len(max_iter)) {
    if (truncate) {
      result <- svd(X, nu = min(dims), nv = min(dims))
    } else {
      result <- svd(X)
    }

    Xm <- result$u %*% diag(result$d, nrow = length(result$d)) %*% t(result$v)
    X[miss_idx] <- Xm[miss_idx]

    ssmis <- sum(Xm[miss_idx]^2)
    sstot <- sum(X^2)
    ssreal <- sstot - ssmis

    # Check convergence
    converged_real <- abs(ssreal - ssreal_old) < conv_lim * ssreal_old
    converged_miss <- abs(ssmis - ssmis_old) < conv_lim_miss * ssmis_old

    if (converged_real && converged_miss) {
      break
    }

    ssreal_old <- ssreal
    ssmis_old <- ssmis
  }

  list(u = result$u, d = result$d, v = result$v)
}

#' Standard PLS SVD
#'
#' @description
#' Wrapper for SVD used in PLS analysis. Uses economy SVD by default.
#'
#' @param X Matrix to decompose
#' @param handle_missing Logical; use misssvd if TRUE and missing values present
#'
#' @return List with u, d, v components
#' @export
pls_svd <- function(X, handle_missing = TRUE) {
  assert_that(is.matrix(X))

  if (handle_missing && any(!is.finite(X))) {
    return(misssvd(X, truncate = TRUE))
  }

  # Standard economy SVD
  dims <- dim(X)
  result <- svd(X, nu = min(dims), nv = min(dims))
  list(u = result$u, d = result$d, v = result$v)
}
