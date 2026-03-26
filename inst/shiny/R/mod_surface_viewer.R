# Surface Viewer Module
# Interactive cortical surface visualization using surfwidget renderer backend
#
# Uses renderer abstraction from fct_brain_renderer.R for visualization.
# Default: SurfwidgetRenderer wrapping neurosurf. Accepts injected renderer for testing.
#
# Uses pure computation functions from fct_brain_viewer.R:
# - get_filter_defaults(), format_colorbar_label()

#' Surface Viewer Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
surface_viewer_ui <- function(id) {
  ns <- NS(id)

  panel_card(
    title = "Surface View",

    # Header controls
    div(
      class = "pls-surface-controls",
      style = "display: flex; gap: 16px; align-items: center; margin-bottom: 12px;",

      # Geometry selector (in tab header per CONTEXT.md)
      div(
        `data-test` = "surface-geometry",
        selectInput(
          ns("geometry"),
          "Surface:",
          choices = c(
            "Inflated" = "inflated",
            "Pial" = "pial",
            "White" = "white",
            "Smooth WM" = "smoothwm",
            "Sphere" = "sphere"
          ),
          selected = "inflated",
          width = "120px"
        )
      ),

      # Hemisphere toggles (per CONTEXT.md: hide one to see medial)
      div(
        `data-test` = "surface-hemispheres",
        checkboxGroupInput(
          ns("hemispheres"),
          NULL,
          choices = c("Left" = "lh", "Right" = "rh"),
          selected = c("lh", "rh"),
          inline = TRUE
        )
      )
    ),

    # Loading indicator (shown during surface init)
    div(
      id = ns("loading_spinner"),
      class = "pls-loading hidden",
      `data-test` = "surface-loading",
      span(class = "spinner-border spinner-border-sm"),
      " Loading surface..."
    ),

    # Dual hemisphere container (flexbox side-by-side)
    div(
      class = "pls-surface-container",
      style = "display: flex; gap: 8px;",

      # Left hemisphere
      div(
        id = ns("lh_container"),
        style = "flex: 1;",
        `data-test` = "surface-lh",
        uiOutput(ns("surface_lh_output"))
      ),

      # Right hemisphere
      div(
        id = ns("rh_container"),
        style = "flex: 1;",
        `data-test` = "surface-rh",
        uiOutput(ns("surface_rh_output"))
      )
    ),

    # Colorbar info (synchronized with volume view)
    div(
      class = "mt-2 text-center small text-muted",
      `data-test` = "surface-colorbar",
      uiOutput(ns("colorbar_info"))
    )
  )
}

#' Surface Viewer Module Server
#'
#' @param id Module namespace ID
#' @param result_rv Reactive containing pls_result
#' @param filters Reactive list of filter values from filter_bar
#' @param renderer Optional BrainRenderer instance for dependency injection (testing)
#' @return List with dispose function for cleanup
#' @keywords internal
surface_viewer_server <- function(id, result_rv, filters, renderer = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check if neurosurf is available for surface rendering
    if (exists("ensure_rgl_use_null", mode = "function")) {
      ensure_rgl_use_null()
    }
    neurosurf_available <- requireNamespace("neurosurf", quietly = TRUE)

    # Create per-session registry if no renderer injected
    registry <- if (is.null(renderer)) {
      reg <- RendererRegistry$new()
      reg$register_surfwidget()  # Conditionally registers if neurosurf available
      reg
    } else {
      NULL  # Use injected renderer directly
    }

    # Get active renderer: injected or from registry
    active_renderer <- if (!is.null(renderer)) {
      renderer
    } else if (!is.null(registry) && neurosurf_available) {
      tryCatch(
        registry$get("surfwidget"),
        error = function(e) NULL
      )
    } else {
      NULL
    }

    # Local state
    local_rv <- reactiveValues(
      geometry = "inflated",
      loading = FALSE
    )

    # Track widget IDs for cleanup
    widget_ids <- reactiveValues(lh = NULL, rh = NULL)

    # Geometry change handler - update renderer geometry
    observeEvent(input$geometry, {
      local_rv$geometry <- input$geometry

      # Update renderer geometry if supported
      if (!is.null(active_renderer) && "set_geometry" %in% names(active_renderer)) {
        active_renderer$set_geometry(input$geometry)
      }
    })

    # Hemisphere toggle handler - show/hide containers via shinyjs
    observeEvent(input$hemispheres, {
      if ("lh" %in% input$hemispheres) {
        shinyjs::show("lh_container")
      } else {
        shinyjs::hide("lh_container")
      }

      if ("rh" %in% input$hemispheres) {
        shinyjs::show("rh_container")
      } else {
        shinyjs::hide("rh_container")
      }
    }, ignoreNULL = FALSE)

    # Left hemisphere surface output
    output$surface_lh_output <- renderUI({
      if (is.null(active_renderer)) {
        # Fallback when neurosurf not available
        div(
          class = "text-muted text-center p-4",
          style = "min-height: 200px; display: flex; align-items: center; justify-content: center;",
          span("Surface visualization requires neurosurf package")
        )
      } else if (active_renderer$is_widget()) {
        # Use surfwidgetOutput for widget renderers
        if (neurosurf_available) {
          neurosurf::surfwidgetOutput(ns("surface_lh"), height = "300px")
        } else {
          div(
            class = "text-muted text-center p-4",
            span("Surface visualization requires neurosurf package")
          )
        }
      } else {
        # Fallback for non-widget renderers (e.g., mock in tests)
        plotOutput(ns("surface_lh_plot"), height = "300px")
      }
    })

    # Right hemisphere surface output
    output$surface_rh_output <- renderUI({
      if (is.null(active_renderer)) {
        div(
          class = "text-muted text-center p-4",
          style = "min-height: 200px; display: flex; align-items: center; justify-content: center;",
          span("Surface visualization requires neurosurf package")
        )
      } else if (active_renderer$is_widget()) {
        if (neurosurf_available) {
          neurosurf::surfwidgetOutput(ns("surface_rh"), height = "300px")
        } else {
          div(
            class = "text-muted text-center p-4",
            span("Surface visualization requires neurosurf package")
          )
        }
      } else {
        plotOutput(ns("surface_rh_plot"), height = "300px")
      }
    })

    # Render left hemisphere surface (when neurosurf available)
    if (neurosurf_available) {
      output$surface_lh <- neurosurf::renderSurfwidget({
        req(active_renderer)
        req(active_renderer$is_widget())

        # Depend on geometry selector and update renderer geometry
        geometry <- local_rv$geometry
        if ("set_geometry" %in% names(active_renderer)) {
          active_renderer$set_geometry(geometry)
        }

        result <- result_rv()
        req(result)
        req(result$mask)

        # Show loading
        local_rv$loading <- TRUE
        shinyjs::removeClass("loading_spinner", "hidden")

        # Get filter values with defaults
        filter_vals <- get_filter_defaults(list(
          lv = if (!is.null(filters$lv)) filters$lv() else NULL,
          bsr_threshold = if (!is.null(filters$bsr_threshold)) filters$bsr_threshold() else NULL,
          what = if (!is.null(filters$what)) filters$what() else NULL
        ))

        lv <- filter_vals$lv
        threshold <- filter_vals$bsr_threshold
        what <- filter_vals$what

        # Render via abstraction
        widget <- tryCatch({
          active_renderer$render(
            result = result,
            lv = lv,
            what = what,
            threshold = threshold,
            view = "surface",
            hemisphere = "lh"
          )
        }, error = function(e) {
          NULL
        })

        # Hide loading
        local_rv$loading <- FALSE
        shinyjs::addClass("loading_spinner", "hidden")

        # Track widget ID for cleanup
        if (!is.null(widget)) {
          widget_ids$lh <- ns("surface_lh")
        }

        widget
      })

      # Render right hemisphere surface
      output$surface_rh <- neurosurf::renderSurfwidget({
        req(active_renderer)
        req(active_renderer$is_widget())

        # Depend on geometry selector and update renderer geometry
        geometry <- local_rv$geometry
        if ("set_geometry" %in% names(active_renderer)) {
          active_renderer$set_geometry(geometry)
        }

        result <- result_rv()
        req(result)
        req(result$mask)

        # Get filter values with defaults
        filter_vals <- get_filter_defaults(list(
          lv = if (!is.null(filters$lv)) filters$lv() else NULL,
          bsr_threshold = if (!is.null(filters$bsr_threshold)) filters$bsr_threshold() else NULL,
          what = if (!is.null(filters$what)) filters$what() else NULL
        ))

        lv <- filter_vals$lv
        threshold <- filter_vals$bsr_threshold
        what <- filter_vals$what

        # Render via abstraction
        widget <- tryCatch({
          active_renderer$render(
            result = result,
            lv = lv,
            what = what,
            threshold = threshold,
            view = "surface",
            hemisphere = "rh"
          )
        }, error = function(e) {
          NULL
        })

        # Track widget ID for cleanup
        if (!is.null(widget)) {
          widget_ids$rh <- ns("surface_rh")
        }

        widget
      })
    }

    # Fallback plot output for non-widget renderers (testing with MockSurfwidgetRenderer)
    output$surface_lh_plot <- renderPlot({
      req(active_renderer)
      req(!active_renderer$is_widget())

      geometry <- local_rv$geometry
      if ("set_geometry" %in% names(active_renderer)) {
        active_renderer$set_geometry(geometry)
      }

      result <- result_rv()
      if (is.null(result) || is.null(result$mask)) {
        plot.new()
        text(0.5, 0.5, "No surface data", cex = 1.2, col = "#6B7280")
        return()
      }

      # Get filter values with defaults
      filter_vals <- get_filter_defaults(list(
        lv = if (!is.null(filters$lv)) filters$lv() else NULL,
        bsr_threshold = if (!is.null(filters$bsr_threshold)) filters$bsr_threshold() else NULL,
        what = if (!is.null(filters$what)) filters$what() else NULL
      ))

      lv <- filter_vals$lv
      threshold <- filter_vals$bsr_threshold
      what <- filter_vals$what

      tryCatch({
        p <- active_renderer$render(
          result = result,
          lv = lv,
          what = what,
          threshold = threshold,
          view = "surface",
          hemisphere = "lh"
        )
        print(p)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 0.9, col = "#EF4444")
      })
    })

    output$surface_rh_plot <- renderPlot({
      req(active_renderer)
      req(!active_renderer$is_widget())

      geometry <- local_rv$geometry
      if ("set_geometry" %in% names(active_renderer)) {
        active_renderer$set_geometry(geometry)
      }

      result <- result_rv()
      if (is.null(result) || is.null(result$mask)) {
        plot.new()
        text(0.5, 0.5, "No surface data", cex = 1.2, col = "#6B7280")
        return()
      }

      filter_vals <- get_filter_defaults(list(
        lv = if (!is.null(filters$lv)) filters$lv() else NULL,
        bsr_threshold = if (!is.null(filters$bsr_threshold)) filters$bsr_threshold() else NULL,
        what = if (!is.null(filters$what)) filters$what() else NULL
      ))

      lv <- filter_vals$lv
      threshold <- filter_vals$bsr_threshold
      what <- filter_vals$what

      tryCatch({
        p <- active_renderer$render(
          result = result,
          lv = lv,
          what = what,
          threshold = threshold,
          view = "surface",
          hemisphere = "rh"
        )
        print(p)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 0.9, col = "#EF4444")
      })
    })

    # Colorbar info using pure function
    output$colorbar_info <- renderUI({
      result <- result_rv()
      if (is.null(result)) return(NULL)

      what <- if (!is.null(filters$what)) filters$what() else "bsr"
      threshold <- if (!is.null(filters$bsr_threshold)) filters$bsr_threshold() else 3.0

      span(format_colorbar_label(what, threshold))
    })

    # WebGL resource disposal (CRITICAL for memory management)
    # Dispose WebGL resources when tab switches away
    # This is called from parent module when tab changes
    dispose_widgets <- function() {
      # Send JS message to dispose surfwidget WebGL context
      if (!is.null(widget_ids$lh)) {
        session$sendCustomMessage("dispose_surfwidget", widget_ids$lh)
      }
      if (!is.null(widget_ids$rh)) {
        session$sendCustomMessage("dispose_surfwidget", widget_ids$rh)
      }
    }

    # Register session cleanup handler
    session$onSessionEnded(function() {
      # Renderer cleanup (if it has dispose method)
      if (!is.null(active_renderer) && "dispose" %in% names(active_renderer)) {
        active_renderer$dispose()
      }
    })

    # Return dispose function for parent module to call on tab switch
    return(list(
      dispose = dispose_widgets
    ))
  })
}
