#' Multivariate Analysis Method Protocol
#'
#' @description
#' R6 base class defining the protocol that all multivariate analysis methods
#' must implement. Methods declare their capabilities, implement `fit()`, and
#' optionally provide resampling hooks and UI metadata.
#'
#' @details
#' This is the core abstraction for method-neutral multivariate neuroimaging
#' analysis. PLS, CPCA, CCA, ICA, and other methods all implement this
#' protocol. The framework uses capabilities to adapt the builder API,
#' Shiny UI, and result accessors to each method automatically.
#'
#' @name mva-method
NULL

#' MvaMethod R6 Class
#'
#' @description
#' Base class for multivariate analysis methods. Subclass this to implement
#' new methods (e.g., CPCA, CCA, ICA). See \code{PlsMethod} for an example.
#'
#' @importFrom R6 R6Class
#' @export
MvaMethod <- R6::R6Class("MvaMethod",
  public = list(
    #' @field name Short identifier (e.g., "pls_task", "cpca")
    name = NULL,

    #' @field label Human-readable label (e.g., "Task PLS", "Constrained PCA")
    label = NULL,

    #' @field family Method family grouping variants (e.g., "pls", "cpca")
    family = NULL,

    #' @field capabilities Named list declaring what this method needs and produces
    capabilities = NULL,

    #' @description
    #' Create a new MvaMethod instance
    #' @param name Short identifier
    #' @param label Human-readable label
    #' @param family Method family
    #' @param capabilities Named list of capability flags
    initialize = function(name, label, family, capabilities = list()) {
      self$name <- name
      self$label <- label
      self$family <- family
      self$capabilities <- utils::modifyList(private$default_capabilities(), capabilities)
    },

    #' @description
    #' Fit the method to data. Must return an `mva_decomposition`.
    #' @param spec An `mva_spec` or `pls_spec` object
    #' @param progress Logical, show progress
    #' @return An `mva_decomposition` object
    fit = function(spec, progress = TRUE) {
      stop("MvaMethod$fit() must be overridden by subclass: ", self$name,
           call. = FALSE)
    },

    #' @description
    #' Fit on permuted data for significance testing.
    #' Default: permute rows of data matrices, then call fit().
    #' @param spec An `mva_spec` or `pls_spec` object
    #' @param perm_order Integer vector of permuted row indices
    #' @return An `mva_decomposition` object
    permutation_fit = function(spec, perm_order) {
      # Default: permute data rows and refit
      perm_spec <- spec
      perm_spec$datamat_lst <- lapply(spec$datamat_lst, function(m) {
        m[perm_order[seq_len(nrow(m))], , drop = FALSE]
      })
      self$fit(perm_spec, progress = FALSE)
    },

    #' @description
    #' Fit on bootstrapped data for stability estimation.
    #' Default: resample rows, then call fit().
    #' @param spec An `mva_spec` or `pls_spec` object
    #' @param boot_order Integer vector of bootstrap row indices
    #' @return An `mva_decomposition` object
    bootstrap_fit = function(spec, boot_order) {
      boot_spec <- spec
      boot_spec$datamat_lst <- lapply(spec$datamat_lst, function(m) {
        m[boot_order[seq_len(nrow(m))], , drop = FALSE]
      })
      self$fit(boot_spec, progress = FALSE)
    },

    #' @description
    #' Project held-out observations into the fitted score space.
    #' Subclasses should use only training-fit artifacts stored on `result`.
    #' @param result A fitted `mva_result`
    #' @param spec Held-out `mva_spec` or `pls_spec`
    #' @param type Score space to project into
    #' @param progress Logical, show progress
    #' @return Matrix of held-out scores, or a named list when `type = "both"`
    project_scores = function(result, spec, type = "feature", progress = FALSE) {
      stop("Held-out score projection is not implemented for method: ", self$name,
           call. = FALSE)
    },

    #' @description
    #' Align bootstrap decomposition to reference (e.g., Procrustes).
    #' Default: sign-flip columns to maximize agreement.
    #' @param boot_decomp An `mva_decomposition` from bootstrap
    #' @param ref_decomp The reference `mva_decomposition`
    #' @return Aligned `mva_decomposition`
    align_to_reference = function(boot_decomp, ref_decomp) {
      # Default: sign-flip each component to match reference
      K <- length(boot_decomp$importance)
      for (k in seq_len(K)) {
        if (sum(boot_decomp$feature_weights[, k] * ref_decomp$feature_weights[, k]) < 0) {
          boot_decomp$feature_weights[, k] <- -boot_decomp$feature_weights[, k]
          if (!is.null(boot_decomp$design_weights)) {
            boot_decomp$design_weights[, k] <- -boot_decomp$design_weights[, k]
          }
        }
      }
      boot_decomp
    },

    #' @description
    #' Validate that a spec has everything this method needs.
    #' @param spec An `mva_spec` or `pls_spec` object
    #' @return TRUE invisibly, or stops with error
    validate_spec = function(spec) {
      errors <- character(0)

      if (length(spec$datamat_lst) == 0) {
        errors <- c(errors, "No data matrices loaded.")
      }

      caps <- self$capabilities
      if (isTRUE(caps$needs_behavior) && is.null(spec$stacked_behavdata %||% spec$behavdata)) {
        errors <- c(errors, paste0(self$label, " requires behavior data."))
      }
      if (isTRUE(caps$needs_design) && is.null(spec$stacked_designdata %||% spec$designdata)) {
        errors <- c(errors, paste0(self$label, " requires design contrasts."))
      }

      if (length(errors) > 0) {
        stop(paste(c(paste0(self$label, " validation failed:"), errors),
                   collapse = "\n  - "), call. = FALSE)
      }
      invisible(TRUE)
    },

    #' @description
    #' Apply method-specific configuration to a spec.
    #' Override to handle method-specific parameters.
    #' @param spec An `mva_spec` or `pls_spec` object
    #' @param ... Method-specific parameters
    #' @return Modified spec
    configure = function(spec, ...) {
      dots <- list(...)
      if (length(dots) > 0) {
        spec$method_config <- utils::modifyList(spec$method_config %||% list(), dots)
      }
      spec
    },

    #' @description
    #' UI metadata for the Shiny app. Override to customize.
    #' @return Named list with analysis_stages, explore_panels, inspector_metrics
    ui_config = function() {
      list(
        analysis_stages = c("Validating", "Computing", "Decomposing"),
        explore_panels = c("feature_weights", "scores", "importance"),
        inspector_metrics = c("importance", "significance"),
        component_name = self$capabilities$component_name %||% "Component",
        importance_name = self$capabilities$importance_name %||% "Importance"
      )
    }
  ),

  private = list(
    default_capabilities = function() {
      list(
        needs_behavior     = FALSE,
        needs_design       = FALSE,
        needs_conditions   = TRUE,
        has_feature_weights = TRUE,
        has_design_weights = TRUE,
        has_scores         = TRUE,
        component_name     = "Component",
        importance_name    = "Importance"
      )
    }
  )
)

# --- Method Registry ---

#' @keywords internal
.mva_registry <- new.env(parent = emptyenv())
.mva_registry$methods <- list()
.mva_registry$aliases <- list()

#' Register a Multivariate Analysis Method
#'
#' @param name Canonical method name
#' @param method An `MvaMethod` R6 instance
#' @param aliases Character vector of alternative names
#' @export
register_method <- function(name, method, aliases = character(0)) {
  stopifnot(inherits(method, "MvaMethod"))
  .mva_registry$methods[[name]] <- method
  for (a in aliases) {
    .mva_registry$aliases[[a]] <- name
  }
  invisible(method)
}

#' Get a Registered Method by Name
#'
#' @param name Method name or alias
#' @return An `MvaMethod` R6 instance
#' @export
get_method <- function(name) {
  if (inherits(name, "MvaMethod")) return(name)
  name <- as.character(name)

  # Direct lookup

  m <- .mva_registry$methods[[name]]
  if (!is.null(m)) return(m)

  # Alias lookup
  canonical <- .mva_registry$aliases[[name]]
  if (!is.null(canonical)) {
    m <- .mva_registry$methods[[canonical]]
    if (!is.null(m)) return(m)
  }

  stop("Unknown analysis method: '", name,
       "'. Available: ", paste(names(.mva_registry$methods), collapse = ", "),
       call. = FALSE)
}

#' List All Registered Methods
#'
#' @param family Optional family filter (e.g., "pls")
#' @return Named list of method name -> label pairs
#' @export
list_methods <- function(family = NULL) {
  methods <- .mva_registry$methods
  if (!is.null(family)) {
    methods <- Filter(function(m) identical(m$family, family), methods)
  }
  vapply(methods, function(m) m$label, character(1))
}
