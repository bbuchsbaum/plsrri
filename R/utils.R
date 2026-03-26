#' Utility Functions for plsrri
#'
#' @description
#' Internal utility functions used throughout the package.
#'
#' @name pls-utils
#' @keywords internal
NULL

# Silence R CMD check notes about ggplot aesthetics
utils::globalVariables(c(
  "score", "se", "x", "y", "value", "label", "loading", "lv",
  "significant", "pvalue", "condition", "group", "subject"
))

#' Stack Data Matrices
#'
#' @description
#' Stacks a list of data matrices vertically (by rows).
#'
#' @param datamat_lst List of data matrices
#'
#' @return Stacked matrix
#' @keywords internal
stack_datamats <- function(datamat_lst) {
  do.call(rbind, datamat_lst)
}

#' Check Data Matrix Validity
#'
#' @description
#' Validates a data matrix for PLS analysis.
#'
#' @param x Matrix to check
#' @param name Name for error messages
#'
#' @return TRUE invisibly if valid
#' @keywords internal
check_datamat <- function(x, name = "data matrix") {
  if (!is.matrix(x)) {
    stop(sprintf("%s must be a matrix", name))
  }

  if (any(!is.finite(x))) {
    n_na <- sum(!is.finite(x))
    warning(sprintf("%s contains %d non-finite values", name, n_na))
  }

  invisible(TRUE)
}

#' Safe Division
#'
#' @description
#' Division that returns 0 for 0/0 cases.
#'
#' @param num Numerator
#' @param denom Denominator
#'
#' @return num/denom with 0/0 -> 0
#' @keywords internal
safe_div <- function(num, denom) {
  result <- num / denom
  result[!is.finite(result)] <- 0
  result
}

#' Compute Explained Variance
#'
#' @description
#' Computes percentage of variance explained by each singular value.
#'
#' @param s Vector of singular values
#' @param cumulative Logical, return cumulative variance
#'
#' @return Vector of variance explained
#' @export
variance_explained <- function(s, cumulative = FALSE) {
  var_exp <- (s^2) / sum(s^2) * 100

  if (cumulative) {
    cumsum(var_exp)
  } else {
    var_exp
  }
}

#' Count Observations Implied by Group/Condition Structure
#'
#' @param num_subj_lst Subjects per group. May be a numeric vector for balanced
#'   designs or a list of per-condition subject-count vectors for ssb designs.
#' @param num_cond Number of conditions.
#'
#' @return Integer count of total observations/rows implied by the design.
#' @keywords internal
count_observations <- function(num_subj_lst, num_cond) {
  if (is.list(num_subj_lst)) {
    sum(vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1)))
  } else {
    sum(as.integer(num_subj_lst)) * as.integer(num_cond)
  }
}

#' Whether internal exact fast paths are enabled
#'
#' @return Logical scalar.
#' @keywords internal
.plsrri_fast_paths_enabled <- function() {
  isTRUE(getOption("plsrri.fast_paths", TRUE))
}

#' Create Index Vector for Subject-Condition Structure
#'
#' @description
#' Creates row indices for the subject-in-condition-in-group data structure.
#'
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param group Group index
#' @param condition Condition index (NULL = all)
#'
#' @return Integer vector of row indices
#' @keywords internal
make_row_indices <- function(num_subj_lst, num_cond, group = NULL, condition = NULL) {
  num_groups <- length(num_subj_lst)

  # Unequal-per-condition (ssb) support: num_subj_lst is a list of integer
  # vectors (one per group) with length == num_cond.
  if (is.list(num_subj_lst)) {
    if (is.null(group) && is.null(condition)) {
      total_rows <- sum(vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1)))
      return(seq_len(total_rows))
    }

    groups <- if (!is.null(group)) as.integer(group) else seq_len(num_groups)
    conditions <- if (!is.null(condition)) as.integer(condition) else seq_len(num_cond)

    group_sizes <- vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1))
    group_offsets <- c(0L, cumsum(group_sizes[-num_groups]))

    indices <- integer(0)

    for (g in groups) {
      n_vec <- as.integer(num_subj_lst[[g]])
      if (length(n_vec) != num_cond) {
        stop("For ssb designs, each num_subj_lst[[g]] must have length num_cond")
      }

      cum <- c(0L, cumsum(n_vec))
      offset <- group_offsets[g]

      for (c in conditions) {
        start <- offset + cum[c] + 1L
        end <- offset + cum[c + 1L]
        indices <- c(indices, seq.int(start, end))
      }
    }

    return(indices)
  }

  # All indices
  if (is.null(group) && is.null(condition)) {
    return(seq_len(sum(num_subj_lst) * num_cond))
  }

  # Compute offsets
  group_offsets <- c(0, cumsum(num_subj_lst[-num_groups]) * num_cond)

  if (!is.null(group)) {
    groups <- group
  } else {
    groups <- seq_len(num_groups)
  }

  if (!is.null(condition)) {
    conditions <- condition
  } else {
    conditions <- seq_len(num_cond)
  }

  indices <- integer(0)
  for (g in groups) {
    n <- num_subj_lst[g]
    offset <- group_offsets[g]

    for (c in conditions) {
      start <- offset + (c - 1) * n + 1
      end <- offset + c * n
      indices <- c(indices, seq(start, end))
    }
  }

  indices
}

#' Format P-value
#'
#' @description
#' Formats p-values for display.
#'
#' @param p P-value(s)
#' @param digits Number of digits
#'
#' @return Formatted string
#' @keywords internal
format_pvalue <- function(p, digits = 4) {
  ifelse(p < 10^(-digits),
         sprintf("< %s", format(10^(-digits), scientific = FALSE)),
         sprintf("%.*f", digits, p))
}

#' Check Package Availability
#'
#' @description
#' Checks if a package is available and gives informative error if not.
#'
#' @param pkg Package name
#' @param purpose What the package is needed for
#'
#' @return TRUE invisibly if available
#' @keywords internal
require_package <- function(pkg, purpose = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- sprintf("Package '%s' is required", pkg)
    if (!is.null(purpose)) {
      msg <- paste(msg, "for", purpose)
    }
    msg <- paste0(msg, ". Install it with: install.packages('", pkg, "')")
    stop(msg)
  }
  invisible(TRUE)
}

#' Procrustes Rotation Matrix (MATLAB rri_bootprocrust)
#'
#' @description
#' Computes the orthogonal rotation matrix that aligns `bootlv` to `origlv`.
#' Ported from MATLAB `rri_bootprocrust.m`.
#'
#' @param origlv Original LV matrix
#' @param bootlv Bootstrap/permuted LV matrix
#'
#' @return Rotation matrix
#' @keywords internal
pls_bootprocrust <- function(origlv, bootlv) {
  assert_that(is.matrix(origlv))
  assert_that(is.matrix(bootlv))
  assert_that(ncol(origlv) == ncol(bootlv))

  temp <- crossprod(origlv, bootlv)  # origlv' * bootlv
  sv <- svd(temp)
  sv$v %*% t(sv$u)
}

#' Permute Only Selected Conditions (MATLAB rri_randperm_notall)
#'
#' @description
#' Creates a permutation order that shuffles only rows corresponding to
#' conditions in `bscan` (across all groups), leaving other rows unchanged.
#'
#' @param num_subj_lst Subjects per group
#' @param num_cond Number of conditions
#' @param bscan Conditions to permute
#'
#' @return Integer vector of length `sum(num_subj_lst)*num_cond`
#' @keywords internal
pls_randperm_notall <- function(num_subj_lst, num_cond, bscan) {
  total_rows <- if (is.list(num_subj_lst)) {
    sum(vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1)))
  } else {
    sum(num_subj_lst) * num_cond
  }
  reorder <- seq_len(total_rows)
  mask_idx <- make_row_indices(num_subj_lst, num_cond, condition = bscan)

  # Shuffle only masked indices
  reorder[mask_idx] <- sample(mask_idx, size = length(mask_idx), replace = FALSE)
  reorder
}

#' Print Progress Message
#'
#' @description
#' Prints a progress message if progress reporting is enabled.
#'
#' @param msg Message to print
#' @param progress Logical, whether to print
#'
#' @keywords internal
progress_msg <- function(msg, progress = TRUE) {
  if (progress) {
    cli::cli_alert_info(msg)
  }
  invisible(NULL)
}

#' Set Seed with Reproducibility
#'
#' @description
#' Sets random seed ensuring R and RNG stream are synchronized.
#'
#' @param seed Integer seed
#'
#' @keywords internal
set_pls_seed <- function(seed) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  invisible(NULL)
}
