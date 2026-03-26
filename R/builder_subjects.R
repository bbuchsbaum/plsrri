#' Builder API: Add Subjects
#'
#' @description
#' Functions for loading subject data into a PLS specification.
#' Supports both manual data matrices and BIDS directory loading via bidser.
#'
#' @name builder-subjects
NULL

#' Add Subjects to PLS Specification
#'
#' @description
#' Adds subject data to a PLS specification. Can load from BIDS directories
#' (if bidser is available) or accept pre-loaded data matrices.
#'
#' @param spec A `pls_spec` object
#' @param data Either:
#'   - A character path to a BIDS directory
#'   - A list of data matrices (one per group)
#'   - A single data matrix (one group)
#' @param groups Character vector of group labels (for BIDS) or
#'   integer vector of subjects per group (for matrices)
#' @param task Task name (for BIDS)
#' @param space Space name for fMRIPrep derivatives (default "MNI152NLin2009cAsym")
#' @param mask_method How to handle brain mask:
#'   - "intersection": use intersection of all subject masks
#'   - "union": use union of all subject masks
#'   - "first": use first subject's mask
#'   - "provided": use mask from pls_spec
#' @param ... Additional arguments passed to bidser functions
#'
#' @return Updated `pls_spec` object
#' @export
#'
#' @examples
#' # From pre-loaded matrices
#' set.seed(42)
#' group1_data <- matrix(rnorm(60 * 50), 60, 50)
#' group2_data <- matrix(rnorm(54 * 50), 54, 50)
#'
#' spec <- pls_spec() |>
#'   add_subjects(list(group1_data, group2_data), groups = c(20, 18))
#'
#' # From BIDS (requires bidser)
#' \dontrun{
#' spec <- pls_spec() |>
#'   add_subjects("/path/to/bids", groups = c("control", "patient"), task = "rest")
#' }
add_subjects <- function(spec,
                          data,
                          groups = NULL,
                          task = NULL,
                          space = "MNI152NLin2009cAsym",
                          mask_method = "intersection",
                          ...) {

  assert_that(inherits(spec, "pls_spec"))

  # Determine data source type
  if (is.character(data) && length(data) == 1 && dir.exists(data)) {
    # BIDS directory
    spec <- .add_subjects_bids(spec, data, groups, task, space,
                               mask_method, ...)
  } else if (is.list(data) && !is.data.frame(data)) {
    # List of matrices
    spec <- .add_subjects_matrices(spec, data, groups)
  } else if (is.matrix(data)) {
    # Single matrix
    spec <- .add_subjects_matrices(spec, list(data), groups)
  } else {
    stop("data must be a BIDS directory path, list of matrices, or single matrix")
  }

  spec
}

#' Add Subjects from Matrices
#'
#' @keywords internal
.add_subjects_matrices <- function(spec, datamat_lst, groups) {

  # Validate
  assert_that(is.list(datamat_lst))
  assert_that(all(sapply(datamat_lst, is.matrix)))

  # Check all matrices have same number of columns
  n_cols <- sapply(datamat_lst, ncol)
  if (length(unique(n_cols)) > 1) {
    stop("All data matrices must have the same number of columns (features)")
  }

  # Determine num_subj_lst
  if (is.null(groups)) {
    # Assume single group per matrix
    num_subj_lst <- sapply(datamat_lst, nrow)
  } else if (is.numeric(groups)) {
    num_subj_lst <- as.integer(groups)
    if (length(num_subj_lst) != length(datamat_lst)) {
      stop("groups must have same length as datamat_lst")
    }
  } else if (is.list(groups) && !is.data.frame(groups)) {
    if (length(groups) != length(datamat_lst)) {
      stop("groups must have same length as datamat_lst")
    }
    if (!all(vapply(groups, is.numeric, logical(1)))) {
      stop("groups must be a list of numeric vectors for ssb designs")
    }

    num_subj_lst <- lapply(groups, as.integer)

    for (g in seq_along(datamat_lst)) {
      expected_rows <- sum(num_subj_lst[[g]])
      if (nrow(datamat_lst[[g]]) != expected_rows) {
        stop(sprintf(
          "Group %d: data matrix has %d rows but expected %d (sum(groups[[%d]]))",
          g, nrow(datamat_lst[[g]]), expected_rows, g
        ))
      }
    }
  } else if (is.character(groups)) {
    spec$groups <- groups
    # Try to infer from matrix rows
    num_subj_lst <- sapply(datamat_lst, function(m) {
      # Rows should be num_subj * num_cond
      # We don't know num_cond yet, so store total rows
      nrow(m)
    })
  } else {
    stop("groups must be NULL, integer vector, list (ssb), or character vector")
  }

  spec$datamat_lst <- datamat_lst
  spec$num_subj_lst <- num_subj_lst

  spec
}

#' Add Subjects from BIDS Directory
#'
#' @description
#' Loads subject data from a BIDS directory using bidser.
#' Supports fMRIPrep preprocessed derivatives.
#'
#' @keywords internal
.add_subjects_bids <- function(spec, bids_dir, groups, task, space,
                                mask_method, ...) {

 # Check dependencies
  if (!requireNamespace("bidser", quietly = TRUE)) {
    stop("Package 'bidser' is required for BIDS loading. ",
         "Install from: https://github.com/bbuchsbaum/bidser")
  }

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for BIDS loading.")
  }

  if (is.null(task)) {
    stop("task must be specified for BIDS loading")
  }

  # Load BIDS project with fMRIPrep derivatives
  cli::cli_alert_info("Loading BIDS project from {.path {bids_dir}}")
  bids <- bidser::bids_project(bids_dir, fmriprep = TRUE)

  # Get all participant IDs (returns character vector, not data.frame)
  all_participants <- bidser::participants(bids)

  if (length(all_participants) == 0) {
    stop("No participants found in BIDS project")
  }

  # Organize subjects by group
  if (is.null(groups) || identical(groups, "all")) {
    # Single group with all subjects
    group_list <- list(all = all_participants)
    groups <- "all"
  } else if (is.character(groups)) {
    # Groups specified - need to find group column in participants.tsv
    # Access the participants data frame directly
    part_df <- bids$part_df

    # Find column matching these group values
    group_col <- NULL
    for (col in names(part_df)) {
      if (col != "participant_id" && all(groups %in% unique(part_df[[col]]))) {
        group_col <- col
        break
      }
    }

    if (is.null(group_col)) {
      stop("Could not find group column in participants.tsv matching: ",
           paste(groups, collapse = ", "),
           "\nAvailable columns: ", paste(names(part_df), collapse = ", "))
    }

    # Split subjects by group
    group_list <- lapply(groups, function(g) {
      ids <- part_df$participant_id[part_df[[group_col]] == g]
      # Remove sub- prefix if present for consistency
      gsub("^sub-", "", ids)
    })
    names(group_list) <- groups

    cli::cli_alert_info("Grouping by column '{group_col}'")
  } else {
    stop("groups must be NULL, 'all', or a character vector of group labels")
  }

  # Load preprocessed scans for each group
  datamat_lst <- list()
  num_subj_lst <- integer(0)
  all_masks <- list()

  for (g_name in names(group_list)) {
    subjects <- group_list[[g_name]]
    n_subj <- length(subjects)

    cli::cli_alert_info("Loading {n_subj} subjects from group '{g_name}'")

    subj_data <- list()

    for (subj in subjects) {
      # Get preprocessed functional scans using bidser
      func_files <- bidser::preproc_scans(
        bids,
        subid = paste0("^", subj, "$"),  # Exact match
        task = task,
        space = space,
        full_path = TRUE
      )

      if (is.null(func_files) || length(func_files) == 0) {
        cli::cli_alert_warning("No preprocessed scans for subject {subj}, task {task}")
        next
      }

      # Take first match (in case of multiple runs, etc.)
      func_file <- func_files[1]

      # Load image using neuroim2
      img <- neuroim2::read_vol(func_file)
      subj_data[[subj]] <- img

      # Get brain mask
      mask_files <- bidser::mask_files(
        bids,
        subid = paste0("^", subj, "$"),
        space = space,
        full_path = TRUE
      )

      if (!is.null(mask_files) && length(mask_files) > 0) {
        mask <- neuroim2::read_vol(mask_files[1])
        all_masks[[length(all_masks) + 1]] <- mask > 0
      }
    }

    if (length(subj_data) == 0) {
      stop("No data loaded for group ", g_name)
    }

    datamat_lst[[g_name]] <- subj_data
    num_subj_lst <- c(num_subj_lst, length(subj_data))
  }

  # Compute combined mask
  if (length(all_masks) > 0 && is.null(spec$mask)) {
    cli::cli_alert_info("Computing {mask_method} mask from {length(all_masks)} subjects")

    combined_mask <- switch(mask_method,
      "intersection" = Reduce(`&`, all_masks),
      "union" = Reduce(`|`, all_masks),
      "first" = all_masks[[1]],
      all_masks[[1]]
    )

    # Convert to NeuroVol
    spec$mask <- neuroim2::NeuroVol(
      as.numeric(combined_mask),
      neuroim2::space(all_masks[[1]])
    )

    n_voxels <- sum(combined_mask)
    cli::cli_alert_success("Mask contains {format(n_voxels, big.mark=',')} voxels")
  }

  spec$datamat_lst <- datamat_lst
  spec$num_subj_lst <- num_subj_lst
  spec$groups <- names(group_list)
  spec$.bids <- bids
  spec$.bids_raw <- TRUE  # Flag that data needs processing into matrices

  cli::cli_alert_success("Loaded {sum(num_subj_lst)} subjects in {length(group_list)} group(s)")

  spec
}

#' Add Group Labels
#'
#' @description
#' Adds or updates group labels for the PLS specification.
#'
#' @param spec A `pls_spec` object
#' @param labels Character vector of group labels
#'
#' @return Updated `pls_spec` object
#' @export
add_group_labels <- function(spec, labels) {
  assert_that(inherits(spec, "pls_spec"))
  assert_that(is.character(labels))

  if (length(labels) != length(spec$datamat_lst)) {
    stop("Number of labels must match number of groups")
  }

  spec$groups <- labels
  spec
}
