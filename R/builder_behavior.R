#' Builder API: Add Behavior/Design Data
#'
#' @description
#' Functions for adding behavioral measures and design contrasts
#' to a PLS specification.
#'
#' @name builder-behavior
NULL

#' Add Behavior Data to PLS Specification
#'
#' @description
#' Adds behavioral measures for behavior PLS or multiblock PLS.
#' The behavior data should have one row per observation (subject x condition).
#'
#' @param spec A `pls_spec` object
#' @param data Behavior data as:
#'   - A matrix (observations x measures)
#'   - A data frame with measure columns
#' @param measures Optional character vector of measure names
#' @param block_conditions For multiblock PLS: which conditions to use
#'   in the behavior block (default all)
#'
#' @return Updated `pls_spec` object
#' @export
#'
#' @examples
#' # Create brain data: 30 subjects x 3 conditions = 90 rows, 50 features
#' set.seed(42)
#' brain_data <- matrix(rnorm(90 * 50), 90, 50)
#'
#' # Behavior data: 90 rows, 4 measures
#' behav_data <- matrix(rnorm(90 * 4), 90, 4)
#' colnames(behav_data) <- c("accuracy", "rt", "confidence", "effort")
#'
#' spec <- pls_spec() |>
#'   add_subjects(list(brain_data), groups = 30) |>
#'   add_conditions(3) |>
#'   add_behavior(behav_data)
add_behavior <- function(spec, data, measures = NULL, block_conditions = NULL) {
  assert_that(inherits(spec, "pls_spec"))

  # Convert data frame to matrix
  if (is.data.frame(data)) {
    if (is.null(measures)) {
      measures <- names(data)
    }
    data <- as.matrix(data)
  }

  assert_that(is.matrix(data))

  # Validate dimensions
  total_rows <- count_observations(spec$num_subj_lst, spec$num_cond)
  if (nrow(data) != total_rows) {
    stop(sprintf(
      "Behavior data has %d rows, expected %d (sum(subjects) x conditions)",
      nrow(data), total_rows
    ))
  }

  # Check for zero variance columns
  col_var <- apply(data, 2, var, na.rm = TRUE)
  if (any(col_var == 0)) {
    zero_cols <- which(col_var == 0)
    warning(sprintf(
      "Behavior columns with zero variance detected: %s. This may cause issues.",
      paste(zero_cols, collapse = ", ")
    ))
  }

  # Store measure names
  if (is.null(measures)) {
    if (!is.null(colnames(data))) {
      measures <- colnames(data)
    } else {
      measures <- paste0("measure_", seq_len(ncol(data)))
    }
  }
  colnames(data) <- measures

  spec$stacked_behavdata <- data

  # Block conditions for multiblock
  if (!is.null(block_conditions)) {
    if (is.character(block_conditions)) {
      spec$bscan <- match(block_conditions, spec$conditions)
    } else {
      spec$bscan <- as.integer(block_conditions)
    }
  }

  spec
}

#' Add Design Contrasts to PLS Specification
#'
#' @description
#' Adds design contrasts for non-rotated PLS methods.
#' Design contrasts specify hypotheses about condition/group differences.
#'
#' @param spec A `pls_spec` object
#' @param contrasts Contrast matrix where:
#'   - For method 2 (non-rotated task): rows = groups x conditions
#'   - For method 5 (non-rotated behavior): rows = groups x conditions x measures
#'   - For method 6 (non-rotated multiblock): rows = task rows + behavior rows
#'   - Columns = number of contrasts/LVs to extract
#' @param labels Optional character vector of contrast labels
#'
#' @return Updated `pls_spec` object
#' @export
#'
#' @examples
#' # Create brain data for 2 groups
#' set.seed(42)
#' d1 <- matrix(rnorm(60 * 50), 60, 50)  # 20 subjects x 3 conditions
#' d2 <- matrix(rnorm(54 * 50), 54, 50)  # 18 subjects x 3 conditions
#'
#' # 2 groups x 3 conditions = 6 rows, testing 2 contrasts
#' contrasts <- matrix(c(
#'   1, 0,   # g1c1
#'   0, 1,   # g1c2
#'  -1,-1,   # g1c3
#'   1, 0,   # g2c1
#'   0, 1,   # g2c2
#'  -1,-1    # g2c3
#' ), ncol = 2, byrow = TRUE)
#'
#' spec <- pls_spec() |>
#'   add_subjects(list(d1, d2), groups = c(20, 18)) |>
#'   add_conditions(3) |>
#'   add_design(contrasts, labels = c("task_vs_baseline", "task1_vs_task2"))
add_design <- function(spec, contrasts, labels = NULL) {
  assert_that(inherits(spec, "pls_spec"))

  if (is.data.frame(contrasts)) {
    if (is.null(labels)) {
      labels <- names(contrasts)
    }
    contrasts <- as.matrix(contrasts)
  }

  assert_that(is.matrix(contrasts))

  # Check dimensions based on method (we may not know method yet)
  num_groups <- length(spec$datamat_lst)
  num_cond <- spec$num_cond

  # Expected rows for method 2
  expected_task <- num_groups * num_cond

  # Expected rows for method 5 (need behavior data)
  if (!is.null(spec$stacked_behavdata)) {
    n_behav <- ncol(spec$stacked_behavdata)
    expected_behav <- num_groups * num_cond * n_behav
    expected_multiblock <- expected_task + num_groups * length(spec$bscan %||% seq_len(num_cond)) * n_behav
  } else {
    expected_behav <- NULL
    expected_multiblock <- NULL
  }

  # Validate (warn if mismatch, actual validation happens at run time)
  n_rows <- nrow(contrasts)
  valid_sizes <- c(expected_task, expected_behav, expected_multiblock)
  valid_sizes <- valid_sizes[!is.null(valid_sizes)]

  if (!n_rows %in% valid_sizes) {
    warning(sprintf(
      "Contrast matrix has %d rows. Expected sizes: %s",
      n_rows, paste(valid_sizes, collapse = ", ")
    ))
  }

  # Check contrast properties
  .check_contrasts(contrasts)

  # Store labels
  if (is.null(labels)) {
    labels <- paste0("contrast_", seq_len(ncol(contrasts)))
  }
  colnames(contrasts) <- labels

  spec$stacked_designdata <- contrasts

  spec
}

#' Check Contrast Matrix Properties
#'
#' @keywords internal
.check_contrasts <- function(contrasts) {
  # Check rank
  if (qr(contrasts)$rank < ncol(contrasts)) {
    warning("Contrast matrix is rank deficient")
  }

  # Check orthogonality
  target_diag <- diag(
    x = colSums(contrasts^2),
    nrow = ncol(contrasts),
    ncol = ncol(contrasts)
  )
  orth_check <- abs(crossprod(contrasts) - target_diag)
  if (max(orth_check) > 1e-4) {
    cli::cli_alert_info(
      "Contrasts are not orthogonal. LV effects may overlap."
    )
  }
}

#' Helper: NULL-coalescing operator
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Generate Common Contrasts
#'
#' @description
#' Generates common contrast matrices for PLS analysis.
#'
#' @param num_groups Number of groups
#' @param num_cond Number of conditions
#' @param type Type of contrast:
#'   - "helmert": Helmert contrasts
#'   - "deviation": Deviation from mean
#'   - "treatment": Treatment vs baseline
#'   - "polynomial": Polynomial trends
#'
#' @return Contrast matrix
#' @export
generate_contrasts <- function(num_groups, num_cond, type = "helmert") {
  n_rows <- num_groups * num_cond

  # Generate contrasts for conditions
  if (type == "helmert") {
    cond_contrasts <- contr.helmert(num_cond)
  } else if (type == "treatment") {
    cond_contrasts <- contr.treatment(num_cond)
  } else if (type == "polynomial") {
    cond_contrasts <- contr.poly(num_cond)
  } else if (type == "deviation") {
    cond_contrasts <- contr.sum(num_cond)
  } else {
    stop("Unknown contrast type: ", type)
  }

  # Replicate for each group
  contrasts <- matrix(0, nrow = n_rows, ncol = ncol(cond_contrasts))
  for (g in seq_len(num_groups)) {
    row_start <- (g - 1) * num_cond + 1
    row_end <- g * num_cond
    contrasts[row_start:row_end, ] <- cond_contrasts
  }

  contrasts
}
