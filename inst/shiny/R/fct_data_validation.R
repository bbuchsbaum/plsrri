# Data Validation Functions
# Pure functions for setup validation, extracted from mod_setup.R
# These functions run without Shiny context for unit testability

#' Validate Setup Configuration
#'
#' Validates all setup parameters for a PLS analysis.
#'
#' @param data_source Character. One of "manual", "bids", or "load".
#' @param data_loaded Logical. Whether data has been loaded.
#' @param bids_dir BIDS directory selection. Integer if not selected, otherwise parsed path.
#' @param groups List of lists with name and n_subj fields.
#' @param num_conditions Integer. Number of conditions.
#' @param num_boot Integer. Number of bootstrap samples.
#' @return Character vector of error messages. Empty if valid.
#' @keywords internal
validate_setup_config <- function(data_source, data_loaded, bids_dir, groups, num_conditions, num_boot) {
  errors <- character(0)

  data_source <- as.character(data_source)[1]
  data_loaded <- isTRUE(data_loaded)
  num_conditions <- suppressWarnings(as.integer(num_conditions)[1])
  num_boot <- suppressWarnings(as.integer(num_boot)[1])

  # Validate data source
  data_errors <- validate_data_source(data_source, data_loaded, bids_dir)
  errors <- c(errors, data_errors)

  # Validate groups
  group_errors <- validate_groups(groups)
  errors <- c(errors, group_errors)

  # Validate conditions
  if (is.na(num_conditions) || num_conditions < 1) {
    errors <- c(errors, "At least one condition required")
  }

  # Validate bootstrap requirements
  if (!is.na(num_boot) && num_boot > 0) {
    if (length(groups) > 0 && any(sapply(groups, function(g) g$n_subj < 3))) {
      errors <- c(errors, "Bootstrap requires at least 3 subjects per group")
    }
  }

  errors
}

#' Validate Groups
#'
#' Validates the groups list structure and contents.
#'
#' @param groups List of lists with name and n_subj fields.
#' @return Character vector of error messages. Empty if valid.
#' @keywords internal
validate_groups <- function(groups) {
  errors <- character(0)

  if (is.null(groups)) groups <- list()

  if (length(groups) == 0) {
    errors <- c(errors, "At least one group required")
    return(errors)
  }

  # Check all n_subj values
  subj_counts <- sapply(groups, function(g) suppressWarnings(as.integer(g$n_subj)[1]))

  if (any(is.na(subj_counts) | subj_counts < 1)) {
    errors <- c(errors, "All groups must have at least 1 subject")
  }

  errors
}

#' Validate Data Source
#'
#' Validates data source selection and associated requirements.
#'
#' @param data_source Character. One of "manual", "bids", or "load".
#' @param data_loaded Logical. Whether data has been loaded.
#' @param bids_dir BIDS directory selection. Integer if not selected.
#' @return Character vector of error messages. Empty if valid.
#' @keywords internal
validate_data_source <- function(data_source, data_loaded, bids_dir) {
  errors <- character(0)

  data_source <- as.character(data_source)[1]

  if (identical(data_source, "manual") && !isTRUE(data_loaded)) {
    errors <- c(errors, "No data matrices loaded")
  }

  if (identical(data_source, "bids")) {
    # Integer means no directory selected (shinyFiles convention)
    if (is.null(bids_dir) || (is.atomic(bids_dir) && length(bids_dir) == 0) || is.integer(bids_dir)) {
      errors <- c(errors, "No BIDS directory selected")
    }
  }

  errors
}

#' Map Method String to Integer
#'
#' Converts method name to integer code used by PLS analysis.
#'
#' @param method Character. One of "task", "behavior", "multiblock".
#' @return Integer. 1L for task, 3L for behavior, 4L for multiblock, 1L default.
#' @keywords internal
map_method_to_int <- function(method) {
  switch(
    method,
    task = 1L,
    behavior = 3L,
    multiblock = 4L,
    1L  # default to task
  )
}

#' Parse Uploaded File
#'
#' Parses an uploaded data file and returns a matrix.
#'
#' @param file_path Character. Path to the uploaded file.
#' @param file_name Character. Original name of the file (used to determine type).
#' @return Matrix if successful, NULL otherwise.
#' @keywords internal
parse_uploaded_file <- function(file_path, file_name) {
  mat <- NULL

  tryCatch({
    if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
      mat <- as.matrix(read.csv(file_path, row.names = 1))
    } else if (grepl("\\.rds$", file_name, ignore.case = TRUE)) {
      mat <- readRDS(file_path)
    } else if (grepl("\\.rda$", file_name, ignore.case = TRUE)) {
      env <- new.env()
      load(file_path, envir = env)
      mat <- get(ls(env)[1], envir = env)
    }

    # Return matrix only if valid
    if (is.matrix(mat)) {
      mat
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
}
