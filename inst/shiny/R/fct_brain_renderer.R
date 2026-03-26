# Brain Renderer Abstraction
# R6 classes for pluggable brain visualization backends

# In some environments (notably headless CI), `rgl` can hard-crash R on load unless
# configured to use a NULL device. `neurosurf` imports `rgl`, so any attempt to
# `requireNamespace("neurosurf")` can also crash without this guard.
ensure_rgl_use_null <- function() {
  if (Sys.getenv("RGL_USE_NULL") == "") {
    Sys.setenv(RGL_USE_NULL = "TRUE")
  }
  if (is.null(getOption("rgl.useNULL"))) {
    options(rgl.useNULL = TRUE)
  }
  invisible(NULL)
}

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
    #' Check if this renderer produces htmlwidgets (vs ggplot)
    #' @return Logical, TRUE for widget renderers, FALSE for plot renderers
    is_widget = function() {
      FALSE
    },

    #' @description
    #' Render brain visualization
    #' @param result A pls_result object
    #' @param lv Latent variable to render
    #' @param what What to render: "bsr" or "salience"
    #' @param threshold BSR threshold for masking
    #' @param view View mode: "montage" or "ortho"
    #' @param ... Additional arguments passed to renderer implementation
    #' @return ggplot object (or htmlwidget for widget renderers)
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

#' SurfwidgetRenderer
#'
#' @description
#' Brain renderer implementation using neurosurf surfwidget for cortical surface
#' visualization. Produces htmlwidget objects suitable for renderSurfwidget().
#'
#' @field surfaces Cached fsaverage surfaces (lazy-loaded)
#' @field samplers List of cached samplers keyed by mask hash
#' @field geometry Current geometry type (default "inflated")
#'
#' @keywords internal
SurfwidgetRenderer <- R6::R6Class(
  "SurfwidgetRenderer",
  inherit = BrainRenderer,
  public = list(
    #' @field surfaces Cached fsaverage surfaces
    surfaces = NULL,

    #' @field samplers List of cached samplers keyed by mask hash
    samplers = NULL,

    #' @field geometry Current geometry type
    geometry = NULL,

    #' @description
    #' Initialize surfwidget renderer
    #' @param geometry Surface geometry type (default "inflated")
    initialize = function(geometry = "inflated") {
      self$geometry <- geometry
      self$surfaces <- NULL
      self$samplers <- list()
    },

    #' @description
    #' Check if this renderer produces htmlwidgets
    #' @return TRUE (surfwidget is an htmlwidget)
    is_widget = function() {
      TRUE
    },

    #' @description
    #' Render brain visualization on cortical surface
    #' @param result A pls_result object
    #' @param lv Latent variable to render
    #' @param what What to render: "bsr" or "salience"
    #' @param threshold BSR threshold for masking
    #' @param view Ignored for surface rendering
    #' @param ... Additional arguments
    #' @return surfwidget htmlwidget object
    render = function(result, lv, what, threshold, view, ...) {
      ensure_rgl_use_null()
      if (!requireNamespace("neurosurf", quietly = TRUE)) {
        stop("neurosurf package required for surface rendering", call. = FALSE)
      }

      dots <- list(...)
      hemisphere <- dots$hemisphere %||% "lh"
      if (!hemisphere %in% c("lh", "rh")) {
        stop("hemisphere must be 'lh' or 'rh'", call. = FALSE)
      }

      # Lazy-load surfaces
      if (is.null(self$surfaces)) {
        self$surfaces <- get_fsaverage_surfaces(self$geometry)
      }

      # Get or create samplers for this result's mask
      mask_hash <- private$compute_mask_hash(result$mask)
      if (!mask_hash %in% names(self$samplers)) {
        self$samplers[[mask_hash]] <- list(
          lh = create_surface_sampler(self$surfaces, result$mask, "lh"),
          rh = create_surface_sampler(self$surfaces, result$mask, "rh")
        )
      }
      samplers <- self$samplers[[mask_hash]]

      # Map volume to surfaces
      mapped <- map_result_to_surfaces(
        result = result,
        lv = lv,
        what = what,
        surfaces = self$surfaces,
        samplers = samplers
      )

      # Create surfwidget with threshold for requested hemisphere
      neurosurf::surfwidget(
        mapped[[hemisphere]],
        thresh = c(-threshold, threshold),
        colorbar = TRUE,
        colorbar_label = if (what == "bsr") "BSR" else "Salience"
      )
    },

    #' @description
    #' Set surface geometry type (clears cached surfaces)
    #' @param geometry New geometry type
    set_geometry = function(geometry) {
      if (geometry != self$geometry) {
        self$geometry <- geometry
        self$surfaces <- NULL
        # Samplers depend on surface geometry; clear to force recompute.
        self$samplers <- list()
      }
      invisible(self)
    },

    #' @description
    #' Get or create sampler for a mask
    #' @param mask NeuroVol mask
    #' @return List with lh and rh samplers
    get_sampler_for_mask = function(mask) {
      mask_hash <- private$compute_mask_hash(mask)

      if (!mask_hash %in% names(self$samplers)) {
        # Ensure surfaces are loaded
        if (is.null(self$surfaces)) {
          self$surfaces <- get_fsaverage_surfaces(self$geometry)
        }

        self$samplers[[mask_hash]] <- list(
          lh = create_surface_sampler(self$surfaces, mask, "lh"),
          rh = create_surface_sampler(self$surfaces, mask, "rh")
        )
      }

      self$samplers[[mask_hash]]
    }
  ),

  private = list(
    #' Compute hash for mask to use as cache key
    #' @param mask NeuroVol mask
    #' @return Character hash string
    compute_mask_hash = function(mask) {
      # Use a simple hash based on mask dimensions and sum
      # This is fast and sufficient for distinguishing masks
      paste0(
        paste(dim(mask), collapse = "x"),
        "_",
        sum(mask[], na.rm = TRUE)
      )
    }
  )
)

#' MockSurfwidgetRenderer
#'
#' @description
#' Mock surfwidget renderer for testing.
#' Records all render() calls and returns mock htmlwidget-like objects.
#'
#' @field render_calls List of recorded render() invocations
#'
#' @keywords internal
MockSurfwidgetRenderer <- R6::R6Class(
  "MockSurfwidgetRenderer",
  inherit = BrainRenderer,
  public = list(
    #' @field render_calls List of recorded render() calls
    render_calls = NULL,

    #' @description
    #' Initialize mock surfwidget renderer
    initialize = function() {
      self$render_calls <- list()
    },

    #' @description
    #' Check if this renderer produces htmlwidgets
    #' @return TRUE
    is_widget = function() {
      TRUE
    },

    #' @description
    #' Record render call and return mock widget
    #' @param result A pls_result object
    #' @param lv Latent variable to render
    #' @param what What to render: "bsr" or "salience"
    #' @param threshold BSR threshold for masking
    #' @param view View mode (ignored for surface)
    #' @param ... Additional arguments
    #' @return Mock htmlwidget-like object
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

      # Return a mock htmlwidget-like object
      # Structure matches htmlwidgets output enough for testing
      mock_widget <- list(
        x = list(
          lv = lv,
          what = what,
          threshold = threshold
        ),
        sizingPolicy = list(
          defaultWidth = "100%",
          defaultHeight = "400px"
        )
      )
      class(mock_widget) <- c("surfwidget", "htmlwidget")
      mock_widget
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
    },

    #' @description
    #' Check if surfwidget rendering is available (neurosurf installed)
    #' @return Logical, TRUE if neurosurf is available
    has_surfwidget = function() {
      ensure_rgl_use_null()
      requireNamespace("neurosurf", quietly = TRUE)
    },

    #' @description
    #' Conditionally register SurfwidgetRenderer if neurosurf is available
    #' @param geometry Surface geometry type (default "inflated")
    #' @return Self for chaining, invisibly
    register_surfwidget = function(geometry = "inflated") {
      if (self$has_surfwidget()) {
        self$register("surfwidget", SurfwidgetRenderer$new(geometry = geometry))
      }
      invisible(self)
    }
  )
)
