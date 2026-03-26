#' Builder API: Add Conditions
#'
#' @description
#' Functions for specifying experimental conditions in a PLS analysis.
#'
#' @name builder-conditions
NULL

#' Add Conditions to PLS Specification
#'
#' @description
#' Specifies the condition structure for the PLS analysis.
#' Conditions define how the data matrix rows are organized.
#'
#' @param spec A `pls_spec` object
#' @param conditions Either:
#'   - An integer specifying the number of conditions
#'   - A character vector of condition labels
#'   - A data frame with condition information (for BIDS events)
#' @param labels Optional character vector of condition labels
#'   (if conditions is an integer)
#'
#' @return Updated `pls_spec` object
#' @export
#'
#' @examples
#' set.seed(42)
#' data1 <- matrix(rnorm(60 * 50), 60, 50)  # 20 subjects x 3 conditions
#' data2 <- matrix(rnorm(54 * 50), 54, 50)  # 18 subjects x 3 conditions
#'
#' spec <- pls_spec() |>
#'   add_subjects(list(data1, data2), groups = c(20, 18)) |>
#'   add_conditions(3, labels = c("baseline", "task1", "task2"))
add_conditions <- function(spec, conditions, labels = NULL) {
  assert_that(inherits(spec, "pls_spec"))

  if (is.numeric(conditions) && length(conditions) == 1) {
    num_cond <- as.integer(conditions)
    if (is.null(labels)) {
      cond_labels <- paste0("condition_", seq_len(num_cond))
    } else {
      assert_that(length(labels) == num_cond)
      cond_labels <- labels
    }
  } else if (is.character(conditions)) {
    cond_labels <- conditions
    num_cond <- length(cond_labels)
  } else if (is.data.frame(conditions)) {
    # Events-style data frame
    spec <- .add_conditions_events(spec, conditions)
    return(spec)
  } else {
    stop("conditions must be an integer, character vector, or data frame")
  }

  spec$num_cond <- num_cond
  spec$conditions <- cond_labels

  # Validate data matrix dimensions
  if (length(spec$datamat_lst) > 0) {
    .validate_dimensions(spec)
  }

  spec
}

#' Add Conditions from Events Data Frame
#'
#' @description
#' Parses BIDS-style events to determine condition structure.
#'
#' @keywords internal
.add_conditions_events <- function(spec, events) {
  # Expect columns: trial_type or condition, onset, duration
  if ("trial_type" %in% names(events)) {
    cond_col <- "trial_type"
  } else if ("condition" %in% names(events)) {
    cond_col <- "condition"
  } else {
    stop("events data frame must have 'trial_type' or 'condition' column")
  }

  cond_labels <- unique(events[[cond_col]])
  num_cond <- length(cond_labels)

  spec$num_cond <- num_cond
  spec$conditions <- cond_labels
  spec$events <- events

  spec
}

#' Validate Data Matrix Dimensions
#'
#' @keywords internal
.validate_dimensions <- function(spec) {
  if (is.null(spec$num_cond)) {
    return(invisible(NULL))
  }

  num_cond <- spec$num_cond

  for (g in seq_along(spec$datamat_lst)) {
    mat <- spec$datamat_lst[[g]]

    # Skip if raw BIDS data
    if (is.list(mat) && !is.matrix(mat)) {
      next
    }

    n_rows <- nrow(mat)
    if (is.list(spec$num_subj_lst)) {
      n_vec <- as.integer(spec$num_subj_lst[[g]])
      if (length(n_vec) != num_cond) {
        stop(sprintf(
          "Group %d: num_subj_lst[[%d]] must have length %d (num_cond)",
          g, g, num_cond
        ))
      }

      expected_rows <- sum(n_vec)
      if (n_rows != expected_rows) {
        stop(sprintf(
          "Group %d: data matrix has %d rows, expected %d (sum(num_subj_lst[[%d]]))",
          g, n_rows, expected_rows, g
        ))
      }
    } else {
      n_subj <- spec$num_subj_lst[g]
      expected_rows <- n_subj * num_cond

      if (n_rows != expected_rows) {
        # Try to infer if num_subj_lst needs adjustment
        if (n_rows %% num_cond == 0) {
          new_n_subj <- n_rows / num_cond
          cli::cli_alert_warning(
            "Group {g}: adjusting num_subj from {n_subj} to {new_n_subj} based on data dimensions"
          )
          spec$num_subj_lst[g] <- new_n_subj
        } else {
          stop(sprintf(
            "Group %d: data matrix has %d rows, expected %d (subjects=%d x conditions=%d)",
            g, n_rows, expected_rows, n_subj, num_cond
          ))
        }
      }
    }
  }

  invisible(NULL)
}

#' Get Condition Indices
#'
#' @description
#' Returns row indices for a specific condition within a group.
#'
#' @param spec A `pls_spec` object
#' @param group Group index or label
#' @param condition Condition index or label
#'
#' @return Integer vector of row indices
#' @export
get_condition_rows <- function(spec, group = 1, condition = 1) {
  assert_that(inherits(spec, "pls_spec"))

  # Resolve group
  if (is.character(group)) {
    g <- match(group, spec$groups)
    if (is.na(g)) stop("Unknown group: ", group)
  } else {
    g <- as.integer(group)
  }

  # Resolve condition
  if (is.character(condition)) {
    c <- match(condition, spec$conditions)
    if (is.na(c)) stop("Unknown condition: ", condition)
  } else {
    c <- as.integer(condition)
  }

  make_row_indices(spec$num_subj_lst, spec$num_cond, group = g, condition = c)
}
