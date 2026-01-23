# Brain Renderer Abstraction
# R6 classes for pluggable brain visualization backends

#' BrainRenderer (Abstract)
#'
#' @description
#' Abstract base class for brain rendering backends.
#' Subclasses must implement the render() method.
#'
#' @field render Method to render brain visualization (must be overridden)
#'
#' @keywords internal
BrainRenderer <- R6::R6Class(
  "BrainRenderer",
  public = list(
    #' @description
    #' Render brain visualization
    #' @param result A pls_result object
    #' @param lv Latent variable to render
    #' @param what What to render: "bsr" or "salience"
    #' @param threshold BSR threshold for masking
    #' @param view View mode: "montage" or "ortho"
    #' @param ... Additional arguments passed to renderer implementation
    #' @return ggplot object
    render = function(result, lv, what, threshold, view, ...) {
      stop("BrainRenderer$render() is abstract - implement in subclass", call. = FALSE)
    }
  )
)

#' Neuroim2Renderer
#'
#' @description
#' Brain renderer implementation using neuroim2/plsrri::plot_brain().
#' This is the default production renderer for brain visualization.
#'
#' @keywords internal
Neuroim2Renderer <- R6::R6Class(
  "Neuroim2Renderer",
  inherit = BrainRenderer,
  public = list(
    #' @description
    #' Render brain visualization using plsrri::plot_brain()
    #' @param result A pls_result object
    #' @param lv Latent variable to render
    #' @param what What to render: "bsr" or "salience"
    #' @param threshold BSR threshold for masking
    #' @param view View mode: "montage" or "ortho"
    #' @param ... Additional arguments (e.g., along) passed to plot_brain()
    #' @return ggplot object from plot_brain()
    render = function(result, lv, what, threshold, view, ...) {
      plsrri::plot_brain(
        result,
        lv = lv,
        what = what,
        threshold = threshold,
        view = view,
        ...
      )
    }
  )
)

#' MockBrainRenderer
#'
#' @description
#' Mock brain renderer for testing.
#' Records all render() calls and returns minimal ggplot objects.
#'
#' @field render_calls List of recorded render() invocations
#'
#' @keywords internal
MockBrainRenderer <- R6::R6Class(
  "MockBrainRenderer",
  inherit = BrainRenderer,
  public = list(
    #' @field render_calls List of recorded render() calls
    render_calls = NULL,

    #' @description
    #' Initialize mock renderer
    initialize = function() {
      self$render_calls <- list()
    },

    #' @description
    #' Record render call and return mock ggplot
    #' @param result A pls_result object
    #' @param lv Latent variable to render
    #' @param what What to render: "bsr" or "salience"
    #' @param threshold BSR threshold for masking
    #' @param view View mode: "montage" or "ortho"
    #' @param ... Additional arguments
    #' @return Mock ggplot object
    render = function(result, lv, what, threshold, view, ...) {
      # Record call details
      call_record <- list(
        result = result,
        lv = lv,
        what = what,
        threshold = threshold,
        view = view,
        extra_args = list(...)
      )
      self$render_calls <- c(self$render_calls, list(call_record))

      # Return minimal ggplot
      ggplot2::ggplot() + ggplot2::ggtitle(paste("Mock LV", lv))
    },

    #' @description
    #' Clear recorded calls
    reset_calls = function() {
      self$render_calls <- list()
      invisible(self)
    }
  )
)

#' RendererRegistry
#'
#' @description
#' Factory for managing registered brain renderers.
#' By default, registers the Neuroim2Renderer as "neuroim2".
#'
#' @field renderers List of registered renderer instances
#'
#' @keywords internal
RendererRegistry <- R6::R6Class(
  "RendererRegistry",
  public = list(
    #' @field renderers List of registered renderers
    renderers = NULL,

    #' @description
    #' Initialize registry with default renderers
    initialize = function() {
      self$renderers <- list()
      # Register default renderer
      self$register("neuroim2", Neuroim2Renderer$new())
    },

    #' @description
    #' Register a renderer
    #' @param name Character name for the renderer
    #' @param renderer BrainRenderer instance
    register = function(name, renderer) {
      stopifnot(inherits(renderer, "BrainRenderer"))
      self$renderers[[name]] <- renderer
      invisible(self)
    },

    #' @description
    #' Get a registered renderer by name
    #' @param name Character name of the renderer
    #' @return BrainRenderer instance
    get = function(name) {
      if (!name %in% names(self$renderers)) {
        stop("Renderer '", name, "' not found. Available: ",
             paste(names(self$renderers), collapse = ", "),
             call. = FALSE)
      }
      self$renderers[[name]]
    },

    #' @description
    #' List all available renderer names
    #' @return Character vector of registered renderer names
    list_available = function() {
      names(self$renderers)
    }
  )
)
