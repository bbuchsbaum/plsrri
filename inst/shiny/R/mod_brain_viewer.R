# Brain Viewer Module
# Interactive brain slice visualization wrapping plot_brain()

#' Brain Viewer Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
brain_viewer_ui <- function(id) {
  ns <- NS(id)

  panel_card(
    title = "Brain View",

    # View toggle
    div(
      class = "pls-brain-controls",
      div(
        class = "pls-brain-toggle btn-group",
        role = "group",
        actionButton(
          ns("btn_montage"),
          "Montage",
          class = "btn btn-outline-secondary btn-sm active",
          `data-test` = "brain-montage-btn"
        ),
        actionButton(
          ns("btn_ortho"),
          "Ortho",
          class = "btn btn-outline-secondary btn-sm",
          `data-test` = "brain-ortho-btn"
        )
      ),
      div(
        # Additional controls
        `data-test` = "brain-axis-select",
        selectInput(
          ns("axis"),
          label = NULL,
          choices = c(
            "Axial" = "3",
            "Coronal" = "2",
            "Sagittal" = "1"
          ),
          selected = "3",
          width = "100px"
        )
      )
    ),

    # Main plot area
    div(
      class = "brain-plot-container",
      style = "min-height: 300px;",
      `data-test` = "brain-plot",
      plotOutput(
        ns("brain_plot"),
        height = "auto",
        click = ns("plot_click"),
        hover = hoverOpts(
          id = ns("plot_hover"),
          delay = 300,
          delayType = "debounce"
        )
      )
    ),

    # Colorbar legend
    div(
      class = "mt-2 text-center small text-muted",
      `data-test` = "brain-colorbar",
      uiOutput(ns("colorbar_info"))
    )
  )
}

#' Brain Viewer Module Server
#'
#' @param id Module namespace ID
#' @param result_rv Reactive containing pls_result
#' @param filters Reactive list of filter values from filter_bar
#' @return Reactive with selected voxel info
#' @keywords internal
brain_viewer_server <- function(id, result_rv, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local state
    local_rv <- reactiveValues(
      view_mode = "montage",
      selected_coord = NULL
    )

    # View mode toggle
    observeEvent(input$btn_montage, {
      local_rv$view_mode <- "montage"
      shinyjs::addClass(id = "btn_montage", class = "active")
      shinyjs::removeClass(id = "btn_ortho", class = "active")
    })

    observeEvent(input$btn_ortho, {
      local_rv$view_mode <- "ortho"
      shinyjs::addClass(id = "btn_ortho", class = "active")
      shinyjs::removeClass(id = "btn_montage", class = "active")
    })

    # Use filters view mode if provided
    observe({
      if (!is.null(filters$view_mode)) {
        mode <- filters$view_mode()
        if (!is.null(mode)) {
          local_rv$view_mode <- mode

          if (mode == "montage") {
            shinyjs::addClass(id = "btn_montage", class = "active")
            shinyjs::removeClass(id = "btn_ortho", class = "active")
          } else {
            shinyjs::addClass(id = "btn_ortho", class = "active")
            shinyjs::removeClass(id = "btn_montage", class = "active")
          }
        }
      }
    })

    # Main brain plot
    output$brain_plot <- renderPlot({
      result <- result_rv()
      if (is.null(result)) {
        # Return empty plot with message
        plot.new()
        text(0.5, 0.5, "No results to display", cex = 1.2, col = "#6B7280")
        return()
      }

      # Check if we have a mask for brain plotting
      if (is.null(result$mask)) {
        # Fall back to a summary plot
        p <- plsrri::plot_singular_values(result)
        return(print(p))
      }

      # Get filter values
      lv <- if (!is.null(filters$lv)) filters$lv() else 1L
      if (is.null(lv)) lv <- 1L

      threshold <- if (!is.null(filters$bsr_threshold)) filters$bsr_threshold() else 3.0
      what <- if (!is.null(filters$what)) filters$what() else "bsr"
      view <- local_rv$view_mode
      along <- as.integer(input$axis)

      # Generate brain plot
      tryCatch({
        p <- plsrri::plot_brain(
          result,
          lv = lv,
          what = what,
          threshold = threshold,
          view = view,
          along = along
        )
        print(p)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Plot error:", e$message), cex = 0.9, col = "#EF4444")
      })
    }, height = function() {
      # Dynamic height based on view mode
      if (local_rv$view_mode == "ortho") {
        400
      } else {
        350
      }
    })

    # Colorbar info
    output$colorbar_info <- renderUI({
      result <- result_rv()
      if (is.null(result)) return(NULL)

      what <- if (!is.null(filters$what)) filters$what() else "bsr"
      threshold <- if (!is.null(filters$bsr_threshold)) filters$bsr_threshold() else 3.0

      if (what == "bsr") {
        span(sprintf("Bootstrap Ratio (|BSR| > %.1f)", threshold))
      } else {
        span("Salience weights")
      }
    })

    # Handle plot clicks
    observeEvent(input$plot_click, {
      click <- input$plot_click
      if (!is.null(click)) {
        local_rv$selected_coord <- list(x = click$x, y = click$y)
      }
    })

    # Return selected info
    reactive({
      list(
        coord = local_rv$selected_coord,
        view_mode = local_rv$view_mode
      )
    })
  })
}

#' Brain Viewer Mini Module
#'
#' @description
#' Compact brain viewer for inspector panel.
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
brain_mini_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "brain-mini-container",
    plotOutput(ns("brain_mini"), height = "150px")
  )
}

#' Brain Viewer Mini Server
#'
#' @param id Module namespace ID
#' @param result_rv Reactive containing pls_result
#' @param lv Reactive LV number
#' @keywords internal
brain_mini_server <- function(id, result_rv, lv) {
  moduleServer(id, function(input, output, session) {
    output$brain_mini <- renderPlot({
      result <- result_rv()
      lv_val <- lv()

      if (is.null(result) || is.null(result$mask)) {
        plot.new()
        return()
      }

      tryCatch({
        plsrri::plot_brain(
          result,
          lv = lv_val,
          what = "bsr",
          threshold = 3,
          view = "montage",
          ncol = 3
        )
      }, error = function(e) {
        plot.new()
      })
    }, bg = "transparent")
  })
}
