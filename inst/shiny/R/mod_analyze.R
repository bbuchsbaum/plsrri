# Analyze Module
# Execution, progress tracking, completion handling

#' Analyze Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
analyze_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "pls-fade-in d-flex align-items-center justify-content-center",
    style = "flex: 1; min-height: 400px;",

    div(
      class = "pls-progress-container",

      # Progress card
      panel_card(
        class = "pls-progress-card",

        # Title and stage
        div(
          `data-test` = "analyze-stage",
          uiOutput(ns("progress_header"))
        ),

        # Progress bar
        div(
          class = "pls-progress-bar-container",
          `data-test` = "analyze-progress",
          div(
            class = "progress",
            style = "height: 8px;",
            div(
              id = ns("progress_bar"),
              class = "progress-bar",
              role = "progressbar",
              style = "width: 0%;",
              `aria-valuenow` = "0",
              `aria-valuemin` = "0",
              `aria-valuemax` = "100"
            )
          ),
          div(
            class = "text-center mt-2",
            `data-test` = "analyze-progress-percent",
            textOutput(ns("progress_percent"), inline = TRUE)
          )
        ),

        # Time stats
        div(
          class = "pls-progress-stats",
          span(`data-test` = "analyze-elapsed", textOutput(ns("elapsed_time"), inline = TRUE)),
          span(textOutput(ns("remaining_time"), inline = TRUE))
        ),

        # Action buttons
        div(
          class = "pls-progress-actions",
          actionButton(
            ns("btn_cancel"),
            "Cancel",
            class = "btn-outline-secondary",
            `data-test` = "analyze-cancel-btn"
          ),
          actionButton(
            ns("btn_back"),
            "Back to Setup",
            icon = icon("arrow-left"),
            class = "btn-outline-primary",
            `data-test` = "analyze-back-btn"
          )
        )
      ),

      # Error display (hidden by default)
      div(
        `data-test` = "analyze-error",
        uiOutput(ns("error_display"))
      ),

      # Completion display (hidden by default)
      div(
        `data-test` = "analyze-complete",
        uiOutput(ns("completion_display"))
      )
    )
  )
}

#' Analyze Module Server
#'
#' @param id Module namespace ID
#' @param state_rv Reactive values for app state
#' @return List with complete trigger and result
#' @keywords internal
analyze_server <- function(id, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local reactive values
    local_rv <- reactiveValues(
      status = "ready",  # ready, running, complete, error, cancelled
      progress = 0,
      stage = "",
      stage_num = 0,
      total_stages = 5,
      start_time = NULL,
      elapsed_seconds = 0,
      result = NULL,
      error_message = NULL,
      analysis_future = NULL
    )

    # Trigger for external communication
    complete_trigger <- reactiveVal(0)

    # Analysis stages
    stages <- c(
      "Validating",
      "Computing Covariance",
      "Running SVD",
      "Permutation Test",
      "Bootstrap"
    )

    # =========================================================================
    # Auto-start analysis when entering this step
    # =========================================================================

    observe({
      # Only run when we're on step 2 and haven't started
      if (state_rv$step == 2 && local_rv$status == "ready") {
        spec <- state_rv$spec

        if (!is.null(spec)) {
          # Start analysis
          start_analysis(spec)
        }
      }
    })

    # =========================================================================
    # Start Analysis
    # =========================================================================

    start_analysis <- function(spec) {
      local_rv$status <- "running"
      local_rv$progress <- 0
      local_rv$stage <- stages[1]
      local_rv$stage_num <- 1
      local_rv$start_time <- Sys.time()
      local_rv$error_message <- NULL

      # Update state
      state_rv$analysis_status <- "running"

      # Progress callback
      progress_callback <- function(stage, progress, message = NULL) {
        local_rv$stage <- stage
        local_rv$progress <- progress

        # Map stage to number
        stage_idx <- match(stage, stages)
        if (!is.na(stage_idx)) {
          local_rv$stage_num <- stage_idx
        }
      }

      # Run analysis (synchronous for now, could use future for async)
      tryCatch({
        # Simulate progress for validation
        progress_callback("Validating", 5)

        # Run the actual analysis
        result <- plsrri::run(
          spec,
          progress = TRUE
        )

        # Complete
        local_rv$progress <- 100
        local_rv$stage <- "Complete"
        local_rv$status <- "complete"
        local_rv$result <- result
        state_rv$analysis_status <- "complete"

        # Trigger completion after short delay for UI update
        shinyjs::delay(500, {
          complete_trigger(complete_trigger() + 1)
        })

      }, error = function(e) {
        local_rv$status <- "error"
        local_rv$error_message <- e$message
        state_rv$analysis_status <- "error"
      })
    }

    # =========================================================================
    # Progress Display
    # =========================================================================

    output$progress_header <- renderUI({
      status <- local_rv$status

      if (status == "complete") {
        div(
          h4(class = "pls-progress-title text-success", icon("check-circle"), " Analysis Complete"),
          p(class = "pls-progress-stage", "Results are ready for exploration")
        )
      } else if (status == "error") {
        div(
          h4(class = "pls-progress-title text-danger", icon("exclamation-circle"), " Analysis Failed"),
          p(class = "pls-progress-stage text-danger", local_rv$error_message)
        )
      } else if (status == "cancelled") {
        div(
          h4(class = "pls-progress-title text-warning", icon("stop-circle"), " Analysis Cancelled"),
          p(class = "pls-progress-stage", "Analysis was stopped by user")
        )
      } else {
        div(
          h4(class = "pls-progress-title", local_rv$stage),
          p(class = "pls-progress-stage",
            sprintf("Step %d of %d", local_rv$stage_num, local_rv$total_stages))
        )
      }
    })

    output$progress_percent <- renderText({
      sprintf("%d%%", as.integer(local_rv$progress))
    })

    # Update progress bar via JS
    observe({
      progress <- local_rv$progress

      shinyjs::runjs(sprintf(
        "document.getElementById('%s').style.width = '%d%%';
         document.getElementById('%s').setAttribute('aria-valuenow', '%d');",
        ns("progress_bar"), progress,
        ns("progress_bar"), progress
      ))

      # Change color based on status
      status <- local_rv$status
      bar_class <- switch(
        status,
        complete = "progress-bar bg-success",
        error = "progress-bar bg-danger",
        cancelled = "progress-bar bg-warning",
        "progress-bar bg-primary"
      )

      shinyjs::runjs(sprintf(
        "document.getElementById('%s').className = '%s';",
        ns("progress_bar"), bar_class
      ))
    })

    # =========================================================================
    # Time Tracking
    # =========================================================================

    # Timer to update elapsed time
    observe({
      invalidateLater(1000, session)

      if (local_rv$status == "running" && !is.null(local_rv$start_time)) {
        local_rv$elapsed_seconds <- as.numeric(
          difftime(Sys.time(), local_rv$start_time, units = "secs")
        )
      }
    })

    output$elapsed_time <- renderText({
      secs <- local_rv$elapsed_seconds
      if (secs == 0) return("Elapsed: --")

      mins <- floor(secs / 60)
      remaining_secs <- round(secs %% 60)

      if (mins > 0) {
        sprintf("Elapsed: %dm %ds", mins, remaining_secs)
      } else {
        sprintf("Elapsed: %ds", remaining_secs)
      }
    })

    output$remaining_time <- renderText({
      progress <- local_rv$progress
      elapsed <- local_rv$elapsed_seconds

      if (progress <= 0 || elapsed <= 0 || local_rv$status != "running") {
        return("Estimated: --")
      }

      # Estimate remaining
      total_estimated <- elapsed / (progress / 100)
      remaining <- total_estimated - elapsed

      if (remaining < 0) remaining <- 0

      mins <- floor(remaining / 60)
      secs <- round(remaining %% 60)

      if (mins > 0) {
        sprintf("Estimated: %dm %ds remaining", mins, secs)
      } else {
        sprintf("Estimated: %ds remaining", secs)
      }
    })

    # =========================================================================
    # Completion Display
    # =========================================================================

    output$completion_display <- renderUI({
      if (local_rv$status != "complete") return(NULL)

      result <- local_rv$result
      if (is.null(result)) return(NULL)

      # Summary info
      n_lv <- length(result$s)
      n_sig <- if (!is.null(result$perm_result)) {
        sum(result$perm_result$sprob < 0.05)
      } else {
        NA
      }

      var_exp <- (result$s^2 / sum(result$s^2)) * 100

      div(
        class = "mt-4",
        panel_card(
          title = "Results Summary",
          status = "complete",

          div(
            class = "row text-center",
            div(
              class = "col-4",
              h3(class = "mb-0", n_lv),
              p(class = "text-muted small mb-0", "Latent Variables")
            ),
            div(
              class = "col-4",
              h3(class = "mb-0 text-success", if (is.na(n_sig)) "--" else n_sig),
              p(class = "text-muted small mb-0", "Significant (p < 0.05)")
            ),
            div(
              class = "col-4",
              h3(class = "mb-0", sprintf("%.1f%%", var_exp[1])),
              p(class = "text-muted small mb-0", "Variance (LV1)")
            )
          ),

          div(
            class = "text-center mt-4",
            actionButton(
              ns("btn_explore"),
              "Explore Results",
              icon = icon("arrow-right"),
              class = "btn-primary btn-lg",
              `data-test` = "analyze-explore-btn"
            )
          )
        )
      )
    })

    # =========================================================================
    # Error Display
    # =========================================================================

    output$error_display <- renderUI({
      if (local_rv$status != "error") return(NULL)

      div(
        class = "mt-4",
        panel_card(
          title = "Error Details",
          status = "error",

          pre(
            class = "bg-light p-3 rounded small",
            local_rv$error_message
          ),

          div(
            class = "mt-3",
            actionButton(
              ns("btn_retry"),
              "Retry Analysis",
              icon = icon("redo"),
              class = "btn-outline-primary me-2"
            ),
            actionButton(
              ns("btn_back_error"),
              "Back to Setup",
              icon = icon("arrow-left"),
              class = "btn-outline-secondary"
            )
          )
        )
      )
    })

    # =========================================================================
    # Button Handlers
    # =========================================================================

    observeEvent(input$btn_cancel, {
      if (local_rv$status == "running") {
        local_rv$status <- "cancelled"
        state_rv$analysis_status <- "ready"
      }
    })

    observeEvent(input$btn_back, {
      # Reset and go back
      local_rv$status <- "ready"
      local_rv$progress <- 0
      state_rv$analysis_status <- "ready"
      state_rv$step <- 1L
    })

    observeEvent(input$btn_back_error, {
      local_rv$status <- "ready"
      local_rv$progress <- 0
      state_rv$analysis_status <- "ready"
      state_rv$step <- 1L
    })

    observeEvent(input$btn_retry, {
      spec <- state_rv$spec
      if (!is.null(spec)) {
        local_rv$status <- "ready"
        # Will auto-start via observer
      }
    })

    observeEvent(input$btn_explore, {
      complete_trigger(complete_trigger() + 1)
    })

    # =========================================================================
    # Return Values
    # =========================================================================

    list(
      complete = complete_trigger,
      result = reactive({ local_rv$result })
    )
  })
}
