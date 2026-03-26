#' Mean-Centering Operations for PLS
#'
#' @description
#' Functions for computing task means and applying mean-centering.
#' Ported from MATLAB rri_task_mean.m and related functions.
#'
#' @name pls-meancentering
NULL

#' Compute Task Means
#'
#' @description
#' Computes the mean for each condition from a stacked data matrix where data
#' is organized as subjects within conditions (n_subj * n_cond rows).
#'
#' @param datamat Data matrix (n_subj*n_cond x n_features)
#' @param n_subj Number of subjects
#'
#' @return Matrix of condition means (n_cond x n_features)
#' @export
#'
#' @examples
#' # 10 subjects, 3 conditions, 50 features
#' datamat <- matrix(rnorm(10 * 3 * 50), 30, 50)
#' means <- pls_task_mean(datamat, n_subj = 10)
pls_task_mean <- function(datamat, n_subj) {
  assert_that(is.matrix(datamat))
  assert_that(is.count(n_subj))

  n_rows <- nrow(datamat)
  n_cols <- ncol(datamat)
  n_cond <- n_rows / n_subj

  if (n_cond != floor(n_cond)) {
    stop("Number of rows must be divisible by n_subj")
  }

  n_cond <- as.integer(n_cond)
  meanmat <- matrix(0, nrow = n_cond, ncol = n_cols)

  for (i in seq_len(n_cond)) {
    row_start <- 1 + n_subj * (i - 1)
    row_end <- n_subj * i
    meanmat[i, ] <- colMeans(datamat[row_start:row_end, , drop = FALSE])
  }

  meanmat
}

#' Compute Task Means with Unequal Group Sizes
#'
#' @description
#' Computes condition means when subjects per condition vary (single-subject
#' or unbalanced designs). The n_subj_cond parameter specifies subjects in
#' each condition.
#'
#' @param datamat Data matrix (sum(n_subj_cond) x n_features)
#' @param n_subj_cond Vector of subjects per condition
#'
#' @return Matrix of condition means (n_cond x n_features)
#' @export
pls_task_mean_ssb <- function(datamat, n_subj_cond) {
  assert_that(is.matrix(datamat))
  assert_that(is.numeric(n_subj_cond), length(n_subj_cond) >= 1)

  n_cond <- length(n_subj_cond)
  n_cols <- ncol(datamat)
  meanmat <- matrix(0, nrow = n_cond, ncol = n_cols)

  step <- 0
  for (i in seq_len(n_cond)) {
    row_idx <- seq_len(n_subj_cond[i]) + step
    meanmat[i, ] <- colMeans(datamat[row_idx, , drop = FALSE])
    step <- step + n_subj_cond[i]
  }

  meanmat
}

#' Apply Mean-Centering to Data Matrix
#'
#' @description
#' Applies one of four mean-centering types to a data matrix organized by
#' groups and conditions.
#'
#' @param datamat_lst List of data matrices (one per group)
#' @param num_subj_lst Subjects per group. For balanced designs, a numeric
#'   vector (or scalar) giving the number of subjects in each group. For SSB
#'   designs, a list of integer vectors, one per group, each of length
#'   `num_cond` giving subjects per condition.
#' @param num_cond Number of conditions
#' @param meancentering_type Mean-centering type (0-3):
#' \describe{
#'   \item{0}{Remove group condition means from condition means within each group.
#'           Highlights condition effects modulated by group membership.}
#'   \item{1}{Remove grand condition means from each group condition mean.
#'           Highlights group differences, removes overall condition differences.}
#'   \item{2}{Remove grand mean over all subjects and conditions.
#'           Shows full spectrum of condition and group effects.}
#'   \item{3}{Remove all main effects (condition and group means).
#'           Pure group by condition interaction.}
#' }
#'
#' @return List with:
#' \describe{
#'   \item{centered}{List of centered data matrices by group}
#'   \item{grand_mean}{Grand mean (type 1-3) or NULL (type 0)}
#'   \item{group_mean}{Group means (type 3) or NULL}
#'   \item{cond_mean}{Condition means by group (type 3) or NULL}
#' }
#'
#' @export
pls_meancentering <- function(datamat_lst, num_subj_lst, num_cond,
                               meancentering_type = 0L) {
  assert_that(is.list(datamat_lst))
  assert_that(is.count(num_cond))
  assert_that(meancentering_type %in% 0:3)

  num_groups <- length(datamat_lst)
  assert_that(length(num_subj_lst) == num_groups)

  is_ssb <- is.list(num_subj_lst)
  if (!is_ssb) {
    assert_that(is.numeric(num_subj_lst))
    num_subj_lst <- as.integer(num_subj_lst)
  } else {
    assert_that(all(vapply(num_subj_lst, is.numeric, logical(1))))
    num_subj_lst <- lapply(num_subj_lst, as.integer)
    for (g in seq_len(num_groups)) {
      if (length(num_subj_lst[[g]]) != num_cond) {
        stop(sprintf(
          "For ssb designs, each num_subj_lst[[%d]] must have length num_cond (%d)",
          g, num_cond
        ))
      }
      if (sum(num_subj_lst[[g]]) != nrow(datamat_lst[[g]])) {
        stop(sprintf(
          "Group %d datamat has %d rows but expected %d (sum(num_subj_lst[[%d]]))",
          g, nrow(datamat_lst[[g]]), sum(num_subj_lst[[g]]), g
        ))
      }
    }
  }

  n_features <- ncol(datamat_lst[[1]])
  if (length(unique(vapply(datamat_lst, ncol, integer(1)))) != 1L) {
    stop("All group data matrices must have the same number of columns (features)")
  }

  # Pre-compute means based on centering type
  if (meancentering_type == 1L) {
    # Grand condition mean across all groups
    grand_mean <- matrix(0, nrow = num_cond, ncol = n_features)
    for (g in seq_len(num_groups)) {
      datamat <- datamat_lst[[g]]
      n <- if (!is_ssb) num_subj_lst[g] else num_subj_lst[[g]]
      grand_mean <- grand_mean + if (!is_ssb) pls_task_mean(datamat, n) else pls_task_mean_ssb(datamat, n)
    }
    grand_mean <- grand_mean / num_groups

  } else if (meancentering_type == 2L) {
    # Grand mean over all observations
    all_data <- do.call(rbind, datamat_lst)
    grand_mean <- colMeans(all_data)

  } else if (meancentering_type == 3L) {
    # Compute condition x group means
    cond_group_mean <- array(0, dim = c(num_cond, num_groups, n_features))
    for (g in seq_len(num_groups)) {
      datamat <- datamat_lst[[g]]
      n <- if (!is_ssb) num_subj_lst[g] else num_subj_lst[[g]]
      cond_group_mean[, g, ] <- if (!is_ssb) pls_task_mean(datamat, n) else pls_task_mean_ssb(datamat, n)
    }

    # Mean across groups for each condition
    cond_mean <- apply(cond_group_mean, c(1, 3), mean)  # num_cond x n_features

    # Mean across conditions for each group
    group_mean <- apply(cond_group_mean, c(2, 3), mean)  # num_groups x n_features

    # Grand mean
    grand_mean <- colMeans(group_mean)

  } else {
    grand_mean <- NULL
  }

  # Apply centering to each group
  centered <- vector("list", num_groups)

  for (g in seq_len(num_groups)) {
    datamat <- datamat_lst[[g]]
    n <- if (!is_ssb) num_subj_lst[g] else num_subj_lst[[g]]
    task_mean <- if (!is_ssb) pls_task_mean(datamat, n) else pls_task_mean_ssb(datamat, n)

    if (meancentering_type == 0L) {
      # Remove within-group mean from task means
      group_data_mean <- colMeans(datamat)
      centered[[g]] <- task_mean - matrix(group_data_mean, nrow = num_cond,
                                           ncol = n_features, byrow = TRUE)

    } else if (meancentering_type == 1L) {
      # Remove grand condition mean
      centered[[g]] <- task_mean - grand_mean

    } else if (meancentering_type == 2L) {
      # Remove grand mean
      centered[[g]] <- task_mean - matrix(grand_mean, nrow = num_cond,
                                           ncol = n_features, byrow = TRUE)

    } else if (meancentering_type == 3L) {
      # Remove condition effect, group effect, add back grand mean
      centered[[g]] <- task_mean -
        cond_mean -
        matrix(group_mean[g, ], nrow = num_cond, ncol = n_features, byrow = TRUE) +
        matrix(grand_mean, nrow = num_cond, ncol = n_features, byrow = TRUE)
    }
  }

  result <- list(centered = centered)

  if (meancentering_type >= 1L) {
    result$grand_mean <- grand_mean
  }

  if (meancentering_type == 3L) {
    result$group_mean <- group_mean
    result$cond_mean <- cond_mean
  }

  result
}

#' Normalize Rows to Unit Length
#'
#' @description
#' Normalizes each row of a matrix to have unit L2 norm.
#' Used in multiblock PLS to balance task and behavior blocks.
#'
#' @param X Matrix to normalize
#' @param margin 1 for rows (default), 2 for columns
#'
#' @return Normalized matrix
#' @export
normalize_rows <- function(X, margin = 1L) {
  assert_that(is.matrix(X))

  if (margin == 1L) {
    norms <- sqrt(rowSums(X^2))
    norms[norms == 0] <- 1  # Avoid division by zero
    sweep(X, 1, norms, "/")
  } else {
    norms <- sqrt(colSums(X^2))
    norms[norms == 0] <- 1
    sweep(X, 2, norms, "/")
  }
}
