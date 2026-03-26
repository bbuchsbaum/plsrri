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

      # Main content — switches between review card and progress/completion/error
      uiOutput(ns("main_content"))
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

    can_async <- requireNamespace("future", quietly = TRUE) &&
      requireNamespace("promises", quietly = TRUE)

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
    # Start Analysis
    # =========================================================================

    start_analysis <- function(prepared) {
      local_rv$status <- "running"
      local_rv$progress <- 0
      local_rv$stage <- prepared_analysis_review_summary(prepared)$mode_label %||% stages[1]
      local_rv$stage_num <- 1
      local_rv$start_time <- Sys.time()
      local_rv$error_message <- NULL
      local_rv$analysis_future <- NULL

      # Update state
      state_rv$analysis_status <- "running"

      if (can_async) {
        fut <- future::future({
          run_prepared_analysis(prepared)
        })
        local_rv$analysis_future <- fut
        local_rv$stage <- prepared_analysis_review_summary(prepared)$mode_label %||% "Running"

        promises::then(
          promises::as.promise(fut),
          onFulfilled = function(out) {
            if (!identical(local_rv$status, "running")) return(invisible(NULL))

            local_rv$progress <- 100
            local_rv$stage <- "Complete"
            local_rv$status <- "complete"
            local_rv$result <- out$result
            state_rv$prepared_analysis <- out$prepared_analysis
            if (!is.null(out$spec)) state_rv$spec <- out$spec
            state_rv$analysis_status <- "complete"
            local_rv$analysis_future <- NULL

            if (!is.null(out$result)) {
              shinyjs::delay(500, {
                complete_trigger(complete_trigger() + 1)
              })
            }

            invisible(NULL)
          },
          onRejected = function(e) {
            local_rv$analysis_future <- NULL
            if (identical(local_rv$status, "cancelled")) return(invisible(NULL))

            local_rv$status <- "error"
            local_rv$error_message <- conditionMessage(e)
            state_rv$analysis_status <- "error"
            invisible(NULL)
          }
        )

        return(invisible(NULL))
      }

      # Synchronous fallback when async deps aren't available
      tryCatch({
        local_rv$stage <- prepared_analysis_review_summary(prepared)$mode_label %||% "Running"
        local_rv$progress <- 0

        out <- run_prepared_analysis(prepared)

        local_rv$progress <- 100
        local_rv$stage <- "Complete"
        local_rv$status <- "complete"
        local_rv$result <- out$result
        state_rv$prepared_analysis <- out$prepared_analysis
        if (!is.null(out$spec)) state_rv$spec <- out$spec
        state_rv$analysis_status <- "complete"

        if (!is.null(out$result)) {
          shinyjs::delay(500, {
            complete_trigger(complete_trigger() + 1)
          })
        }

      }, error = function(e) {
        local_rv$status <- "error"
        local_rv$error_message <- e$message
        state_rv$analysis_status <- "error"
      })
    }

    # =========================================================================
    # Main Content — review card or progress/completion/error
    # =========================================================================

    output$main_content <- renderUI({
      status <- local_rv$status

      if (status == "ready") {
        # Review card
        tagList(
          panel_card(
            class = "pls-progress-card",

            div(
              class = "mb-3",
              h4(class = "pls-progress-title", "Analysis Review"),
              p(class = "text-muted", "Review the analysis configuration below, then click Run PLS to start.")
            ),

            uiOutput(ns("review_summary")),

            div(
              class = "pls-progress-actions mt-4",
              actionButton(
                ns("btn_run_pls"),
                "Run PLS",
                icon = icon("play"),
                class = "btn-primary btn-lg",
                `data-test` = "analyze-run-btn"
              ),
              actionButton(
                ns("btn_back_review"),
                "Back to Setup",
                icon = icon("arrow-left"),
                class = "btn-link",
                `data-test` = "analyze-back-review-btn"
              )
            )
          )
        )
      } else {
        # Progress / completion / error card
        tagList(
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
      }
    })

    # =========================================================================
    # Review Summary
    # =========================================================================

    output$review_summary <- renderUI({
      prepared <- state_rv$prepared_analysis
      if (is.null(prepared)) {
        spec <- state_rv$spec
        if (is.null(spec)) return(p(class = "text-muted", "No analysis prepared"))
        prepared <- build_prepared_analysis_from_spec(spec, source = state_rv$analysis_source %||% "direct")
      }
      summary <- prepared_analysis_review_summary(prepared)

      div(
        class = "pls-review-grid",
        div(class = "row mb-3",
          div(class = "col-6",
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Source"),
              tags$span(class = "fw-bold", summary$source_label %||% "--")
            ),
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Method"),
              tags$span(class = "fw-bold", summary$method_label %||% "--")
            )
          ),
          div(class = "col-6",
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Groups"),
              tags$span(class = "fw-bold", summary$n_groups %||% "--")
            ),
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Conditions"),
              tags$span(class = "fw-bold", if (is.na(summary$n_conditions %||% NA_integer_)) "--" else summary$n_conditions)
            )
          )
        ),
        div(class = "row mb-3",
          div(class = "col-6",
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Observations"),
              tags$span(class = "fw-bold", format(summary$n_observations %||% NA_integer_, big.mark = ","))
            )
          ),
          div(class = "col-6",
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Features"),
              tags$span(class = "fw-bold", if (is.na(summary$n_features %||% NA_integer_)) "--" else format(summary$n_features, big.mark = ","))
            )
          )
        ),
        div(class = "row",
          div(class = "col-6",
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Mode"),
              tags$span(class = "fw-bold", summary$mode_label %||% "--")
            )
          ),
          div(class = "col-6",
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Output Root"),
              tags$span(class = "fw-bold", prepared$pipeline_root %||% "In-memory")
            )
          )
        ),
        div(class = "row",
          div(class = "col-6",
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Permutations"),
              tags$span(class = "fw-bold", if (is.na(summary$nperm %||% NA_integer_)) "--" else format(summary$nperm, big.mark = ","))
            )
          ),
          div(class = "col-6",
            div(class = "mb-2",
              tags$label(class = "text-muted small d-block", "Bootstrap Samples"),
              tags$span(class = "fw-bold", if (is.na(summary$nboot %||% NA_integer_)) "--" else format(summary$nboot, big.mark = ","))
            )
          )
        )
      )
    })

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
      if (local_rv$status == "running") return("Running...")
      sprintf("%d%%", as.integer(local_rv$progress))
    })

    # Update progress bar via JS
    observe({
      status <- local_rv$status
      progress <- local_rv$progress
      bar_width <- if (status == "running") 100 else progress

      shinyjs::runjs(sprintf(
        "document.getElementById('%s').style.width = '%d%%';
         document.getElementById('%s').setAttribute('aria-valuenow', '%d');",
        ns("progress_bar"), bar_width,
        ns("progress_bar"), bar_width
      ))

      # Change color based on status
      bar_class <- switch(
        status,
        complete = "progress-bar bg-success",
        error = "progress-bar bg-danger",
        cancelled = "progress-bar bg-warning",
        running = "progress-bar bg-primary progress-bar-striped progress-bar-animated",
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
      if (is.null(result)) {
        prepared <- state_rv$prepared_analysis
        return(
          div(
            class = "mt-4",
            panel_card(
              title = "First-Level Outputs Ready",
              status = "complete",
              p("First-level preparation completed successfully."),
              p(class = "small text-muted mb-3", prepared$pipeline_root %||% ""),
              div(
                class = "text-center",
                actionButton(
                  ns("btn_back_prepared"),
                  "Back to Setup",
                  icon = icon("arrow-left"),
                  class = "btn-outline-primary"
                )
              )
            )
          )
        )
      }

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

    observeEvent(input$btn_run_pls, {
      prepared <- state_rv$prepared_analysis
      if (is.null(prepared) && !is.null(state_rv$spec)) {
        prepared <- build_prepared_analysis_from_spec(
          state_rv$spec,
          source = state_rv$analysis_source %||% "direct"
        )
      }
      if (!is.null(prepared)) {
        start_analysis(prepared)
      }
    })

    observeEvent(input$btn_back_review, {
      state_rv$step <- 1L
    })

    observeEvent(input$btn_cancel, {
      if (local_rv$status == "running") {
        if (can_async && !is.null(local_rv$analysis_future)) {
          try(future::cancel(local_rv$analysis_future), silent = TRUE)
          local_rv$analysis_future <- NULL
        }
        local_rv$status <- "cancelled"
        local_rv$stage <- "Cancelled"
        local_rv$progress <- 0
        state_rv$analysis_status <- "ready"
      }
    })

    observeEvent(input$btn_back, {
      # Reset and go back
      if (can_async && !is.null(local_rv$analysis_future)) {
        try(future::cancel(local_rv$analysis_future), silent = TRUE)
        local_rv$analysis_future <- NULL
      }
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
      prepared <- state_rv$prepared_analysis
      if (is.null(prepared) && !is.null(state_rv$spec)) {
        prepared <- build_prepared_analysis_from_spec(
          state_rv$spec,
          source = state_rv$analysis_source %||% "direct"
        )
      }
      if (!is.null(prepared)) {
        start_analysis(prepared)
      }
    })

    observeEvent(input$btn_back_prepared, {
      local_rv$status <- "ready"
      local_rv$progress <- 0
      state_rv$analysis_status <- "ready"
      state_rv$step <- 1L
    })

    observeEvent(input$btn_explore, {
      complete_trigger(complete_trigger() + 1)
    })

    session$onSessionEnded(function() {
      shiny::isolate({
        if (can_async && !is.null(local_rv$analysis_future)) {
          try(future::cancel(local_rv$analysis_future), silent = TRUE)
          local_rv$analysis_future <- NULL
        }
      })
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
