#' Statistical Functions for PLS
#'
#' @description
#' Statistical utility functions including percentile computation and
#' distribution-based inference.
#'
#' @name pls-statistics
NULL

#' Compute Percentile
#'
#' @description
#' Computes the Nk-th percentile of X using linear interpolation.
#' Ported from MATLAB percentile.m
#'
#' @param x Numeric vector or matrix
#' @param nk Percentile(s) to compute (0-100)
#' @param margin For matrices: 1 = column percentiles (default), 2 = row percentiles
#'
#' @return Percentile value(s)
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' pls_percentile(x, c(2.5, 97.5))
pls_percentile <- function(x, nk, margin = 1L) {
  assert_that(is.numeric(x))
  assert_that(is.numeric(nk), all(nk >= 0), all(nk <= 100))

  # Handle vectors
  if (is.vector(x)) {
    return(.percentile_vec(x, nk))
  }

  # Handle matrices
  if (is.matrix(x)) {
    if (margin == 1L) {
      return(apply(x, 2, .percentile_vec, nk = nk))
    } else {
      return(t(apply(x, 1, .percentile_vec, nk = nk)))
    }
  }

  stop("x must be a vector or matrix")
}

#' Percentile for Vector (Helper)
#'
#' @keywords internal
.percentile_vec <- function(x, nk) {
  x <- x[is.finite(x)]
  n <- length(x)

  if (n == 0) {
    return(rep(NA_real_, length(nk)))
  }

  x_sorted <- sort(x)

  # Build interpolation grid
  # x values: [0, 0.5/n, 1.5/n, ..., (n-0.5)/n, 1] * 100
  x_grid <- c(0, (seq(0.5, n - 0.5) / n) * 100, 100)
  y_grid <- c(min(x_sorted), x_sorted, max(x_sorted))

  # Interpolate
  approx(x_grid, y_grid, xout = nk, rule = 2)$y
}

#' Compute Bootstrap Confidence Intervals
#'
#' @description
#' Computes confidence intervals from bootstrap distribution.
#'
#' @param boot_distrib Matrix of bootstrap values (n_boot x n_elements)
#' @param clim Confidence level (0-100)
#'
#' @return List with lower and upper bounds
#' @export
pls_boot_ci <- function(boot_distrib, clim = 95) {
  assert_that(is.matrix(boot_distrib))
  assert_that(clim > 0, clim < 100)

  alpha <- (100 - clim) / 2
  lower <- pls_percentile(boot_distrib, alpha, margin = 2)
  upper <- pls_percentile(boot_distrib, 100 - alpha, margin = 2)

  list(lower = lower, upper = upper, clim = clim)
}

#' Distribution-Based Confidence Intervals (MATLAB rri_distrib)
#'
#' @description
#' Port of MATLAB `rri_distrib.m`. Computes percentile confidence intervals
#' and bias-corrected adjusted intervals for skewed bootstrap distributions.
#'
#' @param distrib 3D array with dimensions (r x c x (num_boot+1)). Slice 1 is
#'   the original statistic; slices 2..(num_boot+1) are bootstrap samples.
#' @param ll Lower percentile (e.g., 100 - clim)
#' @param ul Upper percentile (e.g., clim)
#' @param num_boot Number of bootstrap samples (not counting the original)
#' @param climNi Two-tailed alpha/2 as a probability (e.g., 0.025 for 95% CI)
#' @param orig Original statistic matrix (r x c)
#'
#' @return List with `ll`, `ul`, `prop`, `ll_adj`, `ul_adj`
#' @keywords internal
pls_distrib_ci <- function(distrib, ll, ul, num_boot, climNi, orig) {
  assert_that(is.array(distrib))
  assert_that(length(dim(distrib)) == 3L)
  assert_that(is.numeric(ll), length(ll) == 1, ll >= 0, ll <= 100)
  assert_that(is.numeric(ul), length(ul) == 1, ul >= 0, ul <= 100)
  assert_that(is.count(num_boot))
  assert_that(is.numeric(climNi), length(climNi) == 1, climNi > 0, climNi < 0.5)
  assert_that(is.matrix(orig))

  d <- dim(distrib)
  r <- d[1]
  c <- d[2]
  if (d[3] < (num_boot + 1L)) {
    stop("distrib third dimension must be at least num_boot + 1")
  }
  if (!identical(dim(orig), c(r, c))) {
    stop("orig must have same first two dimensions as distrib")
  }

  llmat <- matrix(NA_real_, nrow = r, ncol = c)
  ulmat <- matrix(NA_real_, nrow = r, ncol = c)
  prop <- matrix(NA_real_, nrow = r, ncol = c)
  ll_adj <- matrix(NA_real_, nrow = r, ncol = c)
  ul_adj <- matrix(NA_real_, nrow = r, ncol = c)

  for (ri in seq_len(r)) {
    for (ci in seq_len(c)) {
      bootvec <- distrib[ri, ci, 2:(num_boot + 1L)]

      ulmat[ri, ci] <- pls_percentile(bootvec, ul)
      llmat[ri, ci] <- pls_percentile(bootvec, ll)

      prop_ij <- sum(bootvec <= orig[ri, ci], na.rm = TRUE) / num_boot
      prop[ri, ci] <- prop_ij

      if (prop_ij == 0 || prop_ij == 1 || !is.finite(prop_ij)) {
        next
      }

      ni <- stats::qnorm(prop_ij)
      uli <- (2 * ni) + stats::qnorm(1 - climNi)
      lli <- (2 * ni) + stats::qnorm(climNi)

      ncdf_lli <- stats::pnorm(lli) * 100
      ncdf_uli <- stats::pnorm(uli) * 100

      ll_adj[ri, ci] <- pls_percentile(bootvec, ncdf_lli)
      ul_adj[ri, ci] <- pls_percentile(bootvec, ncdf_uli)
    }
  }

  list(ll = llmat, ul = ulmat, prop = prop, ll_adj = ll_adj, ul_adj = ul_adj)
}

#' Compute P-values from Distribution
#'
#' @description
#' Computes p-values by comparing observed values to a null distribution.
#'
#' @param observed Observed values (vector)
#' @param null_distrib Null distribution matrix (n_perm x n_elements)
#' @param tail "two" (default), "upper", or "lower"
#'
#' @return Vector of p-values
#' @export
pls_pvalue <- function(observed, null_distrib, tail = "two") {
  assert_that(is.numeric(observed))
  assert_that(is.matrix(null_distrib))
  assert_that(tail %in% c("two", "upper", "lower"))

  n_perm <- nrow(null_distrib)
  n_elem <- length(observed)

  if (ncol(null_distrib) != n_elem) {
    stop("null_distrib columns must match length of observed")
  }

  if (tail == "upper") {
    # P(null >= observed)
    p <- colSums(null_distrib >= matrix(observed, nrow = n_perm,
                                        ncol = n_elem, byrow = TRUE)) / n_perm
  } else if (tail == "lower") {
    # P(null <= observed)
    p <- colSums(null_distrib <= matrix(observed, nrow = n_perm,
                                        ncol = n_elem, byrow = TRUE)) / n_perm
  } else {
    # Two-tailed: 2 * min(upper, lower)
    p_upper <- colSums(null_distrib >= matrix(observed, nrow = n_perm,
                                              ncol = n_elem, byrow = TRUE)) / n_perm
    p_lower <- colSums(null_distrib <= matrix(observed, nrow = n_perm,
                                              ncol = n_elem, byrow = TRUE)) / n_perm
    p <- 2 * pmin(p_upper, p_lower)
  }

  pmin(p, 1)
}

#' Check for Low Variability
#'
#' @description
#' Checks if behavior data has low variability which could cause
#' numerical issues (division by zero in correlation).
#'
#' @param behavdata Behavior data matrix
#' @param threshold Minimum standard deviation allowed
#'
#' @return Logical, TRUE if low variability detected
#' @export
pls_is_low_variability <- function(behavdata, threshold = 1e-10) {
  assert_that(is.matrix(behavdata))

  col_sd <- apply(behavdata, 2, sd, na.rm = TRUE)
  any(col_sd < threshold)
}
