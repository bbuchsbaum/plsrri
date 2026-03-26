# Setup module: Attach First-Level Outputs mode
# Provides UI and handlers for loading a CLI pipeline output root.

#' Attach Mode Card UI
#'
#' @param ns Namespace function from the parent module.
#' @return Shiny UI element for the attach card.
#' @keywords internal
setup_ui_attach_card <- function(ns) {
  panel_card(
    title = "Pipeline Output",
    status = tags$span(`data-test` = "setup-attach-status",
                       uiOutput(ns("attach_status"), inline = TRUE)),

    p(class = "text-muted small mb-3",
      "Browse to the output root from a completed CLI pipeline run ",
      "(the folder containing ", code("firstlevel/"), " and ", code("pls/"), ")."),

    shinyFiles::shinyDirButton(
      ns("attach_root_dir"),
      label = "Browse Output Root",
      title = "Select Pipeline Output Directory",
      class = "btn btn-outline-primary btn-sm",
      `data-test` = "setup-attach-browse-btn"
    ),

    uiOutput(ns("attach_root_display")),

    # Validation summary
    uiOutput(ns("attach_validation")),

    # First-level inventory (shown after successful load)
    uiOutput(ns("attach_inventory"))
  )
}

#' Register Attach Mode Handlers
#'
#' @param input,output,session Shiny module args.
#' @param local_rv Local reactive values from the setup module.
#' @param roots File-system roots for shinyFiles.
#' @keywords internal
setup_register_attach_handlers <- function(input, output, session, local_rv, roots) {

  # Wire up directory chooser
  shinyFiles::shinyDirChoose(
    input,
    "attach_root_dir",
    roots = roots,
    session = session
  )

  # Display selected path
  output$attach_root_display <- renderUI({
    dir_info <- input$attach_root_dir
    if (setup_shinyfiles_is_unselected(dir_info)) {
      return(p(class = "text-muted small mt-2", "No directory selected"))
    }

    path <- shinyFiles::parseDirPath(roots, dir_info)
    if (length(path) == 0) return(NULL)

    p(class = "small mt-2 mb-0", icon("folder"), path[1])
  })

  # Validate and load when directory changes
  observeEvent(input$attach_root_dir, {
    dir_info <- input$attach_root_dir
    if (setup_shinyfiles_is_unselected(dir_info)) {
      local_rv$prepared_analysis <- NULL
      local_rv$prepared_analysis_error <- character(0)
      return()
    }

    path <- shinyFiles::parseDirPath(roots, dir_info)
    if (length(path) == 0) return()
    root <- as.character(path[1])

    # Call the public attach API
    info <- plsrri::pipeline_attach_summary(root)

    if (!info$valid) {
      local_rv$prepared_analysis <- NULL
      local_rv$prepared_analysis_error <- info$errors
      return()
    }

    # Try to build the analysis plan
    plan <- tryCatch(
      plsrri::pipeline_load_analysis_plan(root),
      error = function(e) {
        local_rv$prepared_analysis <- NULL
        local_rv$prepared_analysis_error <- e$message
        NULL
      }
    )
    if (is.null(plan)) return()

    local_rv$prepared_analysis <- new_prepared_analysis(
      analysis_source = "attach",
      analyze_mode = "pls_only",
      pipeline_root = root,
      analysis_plan = plan,
      pls_input = list(type = plan$input_type, statistic = plan$statistic),
      summary = summarize_attach_for_review(info$summary, spec = NULL),
      firstlevel_manifest = info$firstlevel_manifest,
      firstlevel_plan = info$firstlevel_plan
    )
    local_rv$prepared_analysis_error <- character(0)
  }, ignoreInit = TRUE)

  # Status indicator
  output$attach_status <- renderUI({
    pa <- local_rv$prepared_analysis
    if (!is.null(pa)) {
      tags$span(class = "text-success", icon("check-circle"))
    } else {
      errs <- local_rv$prepared_analysis_error
      if (length(errs) > 0) {
        tags$span(class = "text-danger", icon("exclamation-circle"))
      } else {
        NULL
      }
    }
  })

  # Validation messages
  output$attach_validation <- renderUI({
    errs <- local_rv$prepared_analysis_error
    if (length(errs) > 0) {
      div(
        class = "mt-3",
        div(
          class = "alert alert-danger py-2 small",
          icon("exclamation-triangle"),
          " Validation failed:",
          tags$ul(class = "mb-0 mt-1",
            lapply(errs, function(e) tags$li(e))
          )
        )
      )
    }
  })

  # First-level inventory table
  output$attach_inventory <- renderUI({
    pa <- local_rv$prepared_analysis
    if (is.null(pa)) return(NULL)

    s <- pa$summary

    div(
      class = "mt-3",
      `data-test` = "setup-attach-inventory",

      div(
        class = "row text-center mb-3",
        div(
          class = "col-3",
          h5(class = "mb-0 fw-bold", s$n_subjects %||% "--"),
          p(class = "text-muted small mb-0", "Subjects")
        ),
        div(
          class = "col-3",
          h5(class = "mb-0 fw-bold", s$n_groups %||% "--"),
          p(class = "text-muted small mb-0", "Groups")
        ),
        div(
          class = "col-3",
          h5(class = "mb-0 fw-bold", length(s$labels %||% character(0))),
          p(class = "text-muted small mb-0", "Labels")
        ),
        div(
          class = "col-3",
          h5(class = "mb-0 fw-bold",
             sprintf("%d/%d", s$n_completed %||% 0L, s$n_work_units %||% 0L)),
          p(class = "text-muted small mb-0", "Completed")
        )
      ),

      if ((s$n_groups %||% 0L) > 1) {
        p(class = "small text-muted mb-1",
          strong("Groups: "), paste(s$groups %||% character(0), collapse = ", "))
      },

      p(class = "small text-muted mb-1",
        strong("Labels: "), paste(s$labels %||% character(0), collapse = ", ")),

      p(class = "small text-muted mb-1",
        strong("Mode: "), s$mode_label %||% "PLS only"),

      if (isTRUE(s$has_basis %||% FALSE)) {
        p(class = "small text-muted mb-1",
          strong("Basis lags: "), s$n_lags %||% "--")
      },

      p(class = "small text-muted mb-0",
        strong("Pipeline root: "), pa$pipeline_root %||% "--")
    )
  })
}
