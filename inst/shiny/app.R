# PLS Neuroimaging Analysis - Shiny GUI
# Entry point and main application

# Load required packages
library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(R6)

# Source R modules
app_dir <- system.file("shiny", package = "plsrri")
if (app_dir == "") {
  # Development mode - source from local path
  app_dir <- "."
}

source(file.path(app_dir, "R", "theme.R"))
source(file.path(app_dir, "R", "state.R"))
source(file.path(app_dir, "R", "ui_components.R"))
source(file.path(app_dir, "R", "fct_brain_viewer.R"))  # Pure functions first
source(file.path(app_dir, "R", "fct_data_validation.R"))  # Data validation functions
source(file.path(app_dir, "R", "mod_setup.R"))
source(file.path(app_dir, "R", "mod_analyze.R"))
source(file.path(app_dir, "R", "mod_explore.R"))
source(file.path(app_dir, "R", "mod_brain_viewer.R"))
source(file.path(app_dir, "R", "mod_filter_bar.R"))
source(file.path(app_dir, "R", "mod_inspector.R"))

# ============================================================================
# UI
# ============================================================================

ui <- bslib::page_fluid(
  theme = pls_shiny_theme(),
  title = "PLS Neuroimaging Analysis",

  # Enable shinyjs
  shinyjs::useShinyjs(),

  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # Main app container
  div(
    class = "pls-app-container",

    # Header
    app_header(),

    # Stepper navigation
    stepper_ui("stepper"),

    # Main content area (switches based on step)
    div(
      class = "pls-main-content",
      uiOutput("main_panel")
    ),

    # Footer with status
    app_footer_ui("footer")
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # Initialize reactive state
  app_state <- create_reactive_state(session)
  rv <- app_state$rv

  # Function to set step
  set_step <- function(step) {
    app_state$state$set_step(step)
    app_state$sync_to_rv()
  }

  # Stepper module
  stepper_server("stepper", rv, set_step)

  # Footer module
  app_footer_server("footer", rv)

  # Main panel - render based on current step
  output$main_panel <- renderUI({
    step <- rv$step

    switch(
      step,
      # Step 1: Setup
      setup_ui("setup"),
      # Step 2: Analyze
      analyze_ui("analyze"),
      # Step 3: Explore
      explore_ui("explore")
    )
  })

  # Setup module
  setup_result <- setup_server("setup", rv)

  # Handle setup completion -> advance to analyze
  observeEvent(setup_result$continue(), {
    # Update state with spec from setup
    spec <- setup_result$spec()
    if (!is.null(spec)) {
      app_state$state$spec <- spec
      app_state$state$max_step <- 2L
      app_state$state$step <- 2L
      app_state$sync_to_rv()
    }
  })

  # Analyze module
  analyze_result <- analyze_server("analyze", rv)

  # Handle analysis completion -> advance to explore
  observeEvent(analyze_result$complete(), {
    result <- analyze_result$result()
    if (!is.null(result)) {
      app_state$state$set_result(result)
      app_state$state$step <- 3L
      app_state$sync_to_rv()
    }
  })

  # Explore module
  explore_server("explore", rv)

  # Help button handler
  observeEvent(input$btn_help, {
    showModal(modalDialog(
      title = "PLS Neuroimaging Analysis Help",
      size = "l",
      easyClose = TRUE,
      div(
        h4("Workflow"),
        p("1. ", strong("Setup:"), " Load your data and configure the analysis"),
        p("2. ", strong("Analyze:"), " Run the PLS analysis with permutation/bootstrap"),
        p("3. ", strong("Explore:"), " Visualize and export results"),
        hr(),
        h4("Quick Start"),
        tags$ul(
          tags$li("Select your data source (BIDS directory, matrices, or saved spec)"),
          tags$li("Choose the analysis method (Task PLS, Behavior PLS, etc.)"),
          tags$li("Configure resampling parameters"),
          tags$li("Click Continue to run the analysis"),
          tags$li("Explore results with interactive brain viewer")
        ),
        hr(),
        h4("Resources"),
        p(
          "Documentation: ",
          a(href = "https://github.com/bbuchsbaum/plsrri",
            "github.com/bbuchsbaum/plsrri", target = "_blank")
        )
      ),
      footer = modalButton("Close")
    ))
  })
}

# ============================================================================
# Launch Function
# ============================================================================

#' Launch PLS GUI
#'
#' @description
#' Launches the interactive Shiny GUI for PLS neuroimaging analysis.
#'
#' @param ... Additional arguments passed to `shiny::runApp()`
#'
#' @return Invisibly returns the Shiny app object
#' @export
#'
#' @examples
#' \dontrun{
#' launch_pls_gui()
#' }
launch_pls_gui <- function(...) {
  app_dir <- system.file("shiny", package = "plsrri")

  if (app_dir == "") {
    stop("Could not find Shiny app. Is plsrri installed correctly?")
  }

  shiny::runApp(app_dir, ...)
}

# Run app when sourced directly
shinyApp(ui = ui, server = server)
