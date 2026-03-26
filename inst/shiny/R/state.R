# PLS Shiny State Management
# R6 class for centralized application state

#' PLS Application State
#'
#' @description
#' R6 class managing centralized state for the PLS Shiny GUI.
#' Provides reactive-friendly state management with validation.
#'
#' @keywords internal
AppState <- R6::R6Class(
  "AppState",
  public = list(
    #' @field step Current workflow step (1=Setup, 2=Analyze, 3=Explore)
    step = 1L,

    #' @field max_step Maximum step reached (prevents skipping ahead)
    max_step = 1L,

    #' @field spec PLS specification object (pls_spec)
    spec = NULL,

    #' @field result PLS result object (pls_result)
    result = NULL,

    #' @field analysis_status Analysis status ("ready", "running", "complete", "error")
    analysis_status = "ready",

    #' @field analysis_progress Progress value (0-100)
    analysis_progress = 0,

    #' @field analysis_stage Current analysis stage name
    analysis_stage = "",

    #' @field selected_lv Currently selected latent variable
    selected_lv = 1L,

    #' @field bsr_threshold Bootstrap ratio threshold for display
    bsr_threshold = 3.0,

    #' @field p_threshold P-value threshold for significance
    p_threshold = 0.05,

    #' @field view_mode Brain view mode ("montage", "ortho")
    view_mode = "montage",

    #' @field data_source Data source type ("bids", "manual", "load")
    data_source = "manual",

    #' @field method PLS method ("task", "behavior", "multiblock")
    method = "task",

    #' @field error_message Last error message
    error_message = NULL,

    #' @field analysis_source Source of analysis data ("direct" or "attach")
    analysis_source = "direct",

    #' @field prepared_analysis Pre-built analysis outputs when in attach mode (list or NULL)
    prepared_analysis = NULL,

    #' @field analyze_mode Controls what Analyze does ("pls_only")
    analyze_mode = "pls_only",

    #' @description
    #' Initialize application state
    initialize = function() {
      self$spec <- NULL
      self$result <- NULL
      self$analysis_source <- "direct"
      self$prepared_analysis <- NULL
      self$analyze_mode <- "pls_only"
      self$reset()
    },

    #' @description
    #' Reset state to initial values
    reset = function() {
      self$step <- 1L
      self$max_step <- 1L
      self$spec <- NULL
      self$result <- NULL
      self$analysis_status <- "ready"
      self$analysis_progress <- 0
      self$analysis_stage <- ""
      self$selected_lv <- 1L
      self$bsr_threshold <- 3.0
      self$p_threshold <- 0.05
      self$view_mode <- "montage"
      self$error_message <- NULL
      self$analysis_source <- "direct"
      self$prepared_analysis <- NULL
      self$analyze_mode <- "pls_only"
      invisible(self)
    },

    #' @description
    #' Advance to next workflow step
    advance_step = function() {
      if (self$step < 3) {
        self$step <- self$step + 1L
        self$max_step <- max(self$max_step, self$step)
      }
      invisible(self)
    },

    #' @description
    #' Go back to previous step
    go_back = function() {
      if (self$step > 1) {
        self$step <- self$step - 1L
      }
      invisible(self)
    },

    #' @description
    #' Set step directly (only if within allowed range)
    #' @param step Target step number
    set_step = function(step) {
      step <- as.integer(step)
      if (step >= 1 && step <= self$max_step) {
        self$step <- step
      }
      invisible(self)
    },

    #' @description
    #' Check if a step is accessible
    #' @param step Step number to check
    can_access_step = function(step) {
      step <= self$max_step
    },

    #' @description
    #' Update spec from setup inputs
    #' @param spec_data List of specification data
    update_spec = function(spec_data) {
      # Create new spec or update existing
      if (is.null(self$spec)) {
        self$spec <- plsrri::pls_spec()
      }

      # Update fields from spec_data
      for (name in names(spec_data)) {
        if (name %in% names(self$spec)) {
          self$spec[[name]] <- spec_data[[name]]
        }
      }

      invisible(self)
    },

    #' @description
    #' Store analysis result
    #' @param result PLS result object
    set_result = function(result) {
      self$result <- result
      self$analysis_status <- "complete"
      self$max_step <- 3L
      invisible(self)
    },

    #' @description
    #' Set error state
    #' @param message Error message
    set_error = function(message) {
      self$error_message <- message
      self$analysis_status <- "error"
      invisible(self)
    },

    #' @description
    #' Get summary of current state for display
    get_summary = function() {
      list(
        step = self$step,
        step_name = c("Setup", "Analyze", "Explore")[self$step],
        has_spec = !is.null(self$spec),
        has_result = !is.null(self$result),
        n_groups = if (!is.null(self$spec)) length(self$spec$datamat_lst) else 0,
        n_conditions = if (!is.null(self$spec)) self$spec$num_cond else 0,
        n_voxels = if (!is.null(self$spec) && length(self$spec$datamat_lst) > 0)
          ncol(self$spec$datamat_lst[[1]]) else 0,
        method = self$method,
        status = self$analysis_status,
        analysis_source = self$analysis_source,
        analyze_mode = self$analyze_mode
      )
    },

    #' @description
    #' Validate current spec is ready for analysis
    validate_for_analysis = function() {
      errors <- character(0)

      if (!is.null(self$prepared_analysis) && self$analyze_mode %in% c("end_to_end", "firstlevel_only")) {
        return(list(valid = TRUE, errors = character(0)))
      }

      if (is.null(self$spec)) {
        errors <- c(errors, "No specification created")
        return(list(valid = FALSE, errors = errors))
      }

      if (length(self$spec$datamat_lst) == 0) {
        errors <- c(errors, "No data loaded")
      }

      if (is.null(self$spec$num_cond) || self$spec$num_cond < 1) {
        errors <- c(errors, "Number of conditions not set")
      }

      if (length(self$spec$num_subj_lst) == 0) {
        errors <- c(errors, "Number of subjects not specified")
      }

      list(valid = length(errors) == 0, errors = errors)
    },

    #' @description
    #' Get number of significant LVs
    get_n_significant = function() {
      if (is.null(self$result) || is.null(self$result$perm_result)) {
        return(NA_integer_)
      }
      sum(self$result$perm_result$sprob < self$p_threshold)
    },

    #' @description
    #' Get total number of LVs
    get_n_lv = function() {
      if (is.null(self$result)) {
        return(NA_integer_)
      }
      length(self$result$s)
    }
  )
)

# Field names synced between AppState and reactiveValues.
# Adding a new field only requires: (1) add it to AppState, (2) add it here.
.APP_STATE_FIELDS <- c(
  "step", "max_step", "spec", "result",
  "analysis_status", "analysis_progress", "analysis_stage",
  "selected_lv", "bsr_threshold", "p_threshold",
  "view_mode", "data_source", "method", "error_message",
  "analysis_source", "prepared_analysis", "analyze_mode"
)

#' Create Reactive State Wrapper
#'
#' @description
#' Wraps AppState in reactive values for Shiny integration.
#' Uses \code{.APP_STATE_FIELDS} to programmatically sync fields,
#' so adding a new field only requires updating the AppState class
#' and the field list above.
#'
#' @param session Shiny session object
#' @return List of reactive values and update functions
#' @keywords internal
create_reactive_state <- function(session = NULL) {
  state <- AppState$new()

  # Build initial reactiveValues from current state fields
  init <- lapply(stats::setNames(.APP_STATE_FIELDS, .APP_STATE_FIELDS),
                 function(f) state[[f]])
  rv <- do.call(shiny::reactiveValues, init)

  list(
    rv = rv,
    state = state,

    sync_to_rv = function() {
      for (f in .APP_STATE_FIELDS) {
        rv[[f]] <- state[[f]]
      }
    },

    reset = function() {
      state$reset()
      for (f in .APP_STATE_FIELDS) {
        rv[[f]] <- state[[f]]
      }
    }
  )
}
