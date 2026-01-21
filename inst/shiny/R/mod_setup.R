# Setup Module
# Data loading, method selection, resampling configuration

#' Setup Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
setup_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "pls-fade-in",

    # Two-column layout
    div(
      class = "pls-setup-grid",

      # Left column
      div(
        # Data Source Card
        panel_card(
          title = "Data Source",
          status = tags$span(`data-test` = "setup-data-status", uiOutput(ns("data_status"), inline = TRUE)),

          div(
            `data-test` = "setup-data-source",
            radioButtons(
              ns("data_source"),
              label = NULL,
              choices = c(
                "BIDS directory" = "bids",
                "Manual matrices" = "manual",
                "Load saved spec" = "load"
              ),
              selected = "manual"
            )
          ),

          # Conditional inputs based on source
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bids'", ns("data_source")),
            ns = ns,
            div(
              class = "mt-3",
              shinyFiles::shinyDirButton(
                ns("bids_dir"),
                label = "Browse BIDS Directory",
                title = "Select BIDS Directory",
                class = "btn btn-outline-primary btn-sm",
                `data-test` = "setup-bids-btn"
              ),
              uiOutput(ns("bids_path_display"))
            )
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("data_source")),
            ns = ns,
            div(
              class = "mt-3",
              div(
                `data-test` = "setup-file-upload",
                fileInput(
                  ns("data_files"),
                  "Upload Data Matrices (CSV/RDS)",
                  accept = c(".csv", ".rds", ".rda"),
                  multiple = TRUE
                )
              ),
              uiOutput(ns("manual_data_info"))
            )
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'load'", ns("data_source")),
            ns = ns,
            div(
              class = "mt-3",
              fileInput(
                ns("spec_file"),
                "Upload Saved Specification (RDS)",
                accept = c(".rds", ".rda")
              )
            )
          )
        ),

        # Study Design Card
        panel_card(
          title = "Study Design",
          status = tags$span(`data-test` = "setup-design-status", uiOutput(ns("design_status"), inline = TRUE)),

          div(
            class = "mb-3",
            tags$label(class = "form-label", "Groups"),
            div(
              class = "d-flex align-items-center gap-2 mb-2",
              actionButton(ns("add_group"), "", icon = icon("plus"), class = "btn-sm btn-outline-primary"),
              actionButton(ns("remove_group"), "", icon = icon("minus"), class = "btn-sm btn-outline-secondary")
            ),
            uiOutput(ns("group_inputs"))
          ),

          div(
            `data-test` = "setup-num-conditions",
            numericInput(
              ns("num_conditions"),
              "Number of Conditions",
              value = 2,
              min = 1,
              max = 20,
              step = 1
            )
          ),

          uiOutput(ns("design_summary"))
        )
      ),

      # Right column
      div(
        # Analysis Method Card
        panel_card(
          title = "Analysis Method",

          div(
            `data-test` = "setup-method",
            radioButtons(
              ns("method"),
              label = NULL,
              choices = c(
                "Task PLS" = "task",
                "Behavior PLS" = "behavior",
                "Multiblock PLS" = "multiblock"
              ),
              selected = "task"
            )
          ),

          # Method description
          uiOutput(ns("method_description"))
        ),

        # Resampling Card
        panel_card(
          title = "Resampling",

          div(
            class = "row",
            div(
              class = "col-6",
              `data-test` = "setup-num-perm",
              numericInput(
                ns("num_perm"),
                "Permutations",
                value = 1000,
                min = 0,
                max = 10000,
                step = 100
              )
            ),
            div(
              class = "col-6",
              `data-test` = "setup-num-boot",
              numericInput(
                ns("num_boot"),
                "Bootstrap Samples",
                value = 500,
                min = 0,
                max = 5000,
                step = 100
              )
            )
          ),

          numericInput(
            ns("confidence"),
            "Confidence Level (%)",
            value = 95,
            min = 80,
            max = 99,
            step = 1
          ),

          # Advanced options
          tags$details(
            class = "mt-3",
            tags$summary(
              class = "pls-advanced-toggle",
              icon("chevron-right"),
              "Advanced options"
            ),
            div(
              class = "pls-advanced-content mt-2",
              selectInput(
                ns("boot_type"),
                "Bootstrap Type",
                choices = c(
                  "Stratified" = "strat",
                  "Non-stratified" = "nonstrat"
                ),
                selected = "strat"
              ),
              numericInput(
                ns("num_split"),
                "Split-half Iterations",
                value = 0,
                min = 0,
                max = 1000
              ),
              selectInput(
                ns("meancentering"),
                "Mean Centering",
                choices = c(
                  "Grand mean" = "0",
                  "Within group" = "1",
                  "Within condition" = "2",
                  "Within group & condition" = "3"
                ),
                selected = "0"
              )
            )
          )
        )
      )
    ),

    # Continue button
    div(
      class = "d-flex justify-content-end mt-4",
      div(
        `data-test` = "setup-validation",
        uiOutput(ns("validation_messages"))
      ),
      actionButton(
        ns("btn_continue"),
        "Continue",
        icon = icon("arrow-right"),
        class = "btn-primary pls-btn-primary",
        `data-test` = "setup-continue-btn"
      )
    )
  )
}

#' Setup Module Server
#'
#' @param id Module namespace ID
#' @param state_rv Reactive values for app state
#' @return List with continue trigger and spec
#' @keywords internal
setup_server <- function(id, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local reactive values
    local_rv <- reactiveValues(
      groups = list(
        list(name = "Group 1", n_subj = 20),
        list(name = "Group 2", n_subj = 18)
      ),
      data_loaded = FALSE,
      data_matrices = NULL,
      mask = NULL,
      validation_errors = character(0),
      spec = NULL
    )

    # Triggers for external communication
    continue_trigger <- reactiveVal(0)

    # =========================================================================
    # Data Source Handling
    # =========================================================================

    # BIDS directory selection
    shinyFiles::shinyDirChoose(
      input, "bids_dir",
      roots = c(home = "~", wd = "."),
      session = session
    )

    output$bids_path_display <- renderUI({
      dir_info <- input$bids_dir
      if (is.integer(dir_info)) {
        return(p(class = "text-muted small", "No directory selected"))
      }

      path <- shinyFiles::parseDirPath(c(home = "~", wd = "."), dir_info)
      if (length(path) > 0) {
        p(class = "small", icon("folder"), path)
      }
    })

    # Manual file upload
    observeEvent(input$data_files, {
      req(input$data_files)

      files <- input$data_files
      matrices <- list()

      for (i in seq_len(nrow(files))) {
        file_path <- files$datapath[i]
        file_name <- files$name[i]

        tryCatch({
          if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
            mat <- as.matrix(read.csv(file_path, row.names = 1))
          } else if (grepl("\\.rds$", file_name, ignore.case = TRUE)) {
            mat <- readRDS(file_path)
          } else if (grepl("\\.rda$", file_name, ignore.case = TRUE)) {
            env <- new.env()
            load(file_path, envir = env)
            mat <- get(ls(env)[1], envir = env)
          }

          if (is.matrix(mat)) {
            matrices[[length(matrices) + 1]] <- mat
          }
        }, error = function(e) {
          showNotification(
            paste("Error loading", file_name, ":", e$message),
            type = "error"
          )
        })
      }

      if (length(matrices) > 0) {
        local_rv$data_matrices <- matrices
        local_rv$data_loaded <- TRUE

        # Update groups to match number of matrices
        if (length(matrices) != length(local_rv$groups)) {
          local_rv$groups <- lapply(seq_along(matrices), function(i) {
            list(
              name = paste("Group", i),
              n_subj = nrow(matrices[[i]]) / max(1, input$num_conditions)
            )
          })
        }
      }
    })

    output$manual_data_info <- renderUI({
      if (!local_rv$data_loaded || is.null(local_rv$data_matrices)) {
        return(NULL)
      }

      matrices <- local_rv$data_matrices
      info <- lapply(seq_along(matrices), function(i) {
        mat <- matrices[[i]]
        tags$li(sprintf("Matrix %d: %d rows x %d columns", i, nrow(mat), ncol(mat)))
      })

      div(
        class = "small text-muted mt-2",
        icon("check-circle", class = "text-success"),
        sprintf(" %d matrices loaded:", length(matrices)),
        tags$ul(info)
      )
    })

    # Load saved spec
    observeEvent(input$spec_file, {
      req(input$spec_file)

      tryCatch({
        spec <- readRDS(input$spec_file$datapath)

        if (inherits(spec, "pls_spec")) {
          local_rv$spec <- spec
          local_rv$data_loaded <- TRUE

          # Populate UI from spec
          if (length(spec$datamat_lst) > 0) {
            local_rv$data_matrices <- spec$datamat_lst
            updateNumericInput(session, "num_conditions", value = spec$num_cond)

            local_rv$groups <- lapply(seq_along(spec$num_subj_lst), function(i) {
              list(
                name = spec$groups[i] %||% paste("Group", i),
                n_subj = spec$num_subj_lst[i]
              )
            })
          }

          # Update method
          method <- switch(
            as.character(spec$method),
            "1" = "task",
            "2" = "task",
            "3" = "behavior",
            "4" = "multiblock",
            "task"
          )
          updateRadioButtons(session, "method", selected = method)

          # Update resampling
          updateNumericInput(session, "num_perm", value = spec$num_perm)
          updateNumericInput(session, "num_boot", value = spec$num_boot)

          showNotification("Specification loaded successfully", type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Error loading spec:", e$message), type = "error")
      })
    })

    # =========================================================================
    # Group Management
    # =========================================================================

    output$group_inputs <- renderUI({
      groups <- local_rv$groups

      lapply(seq_along(groups), function(i) {
        div(
          class = "d-flex align-items-center gap-2 mb-2",
          textInput(
            ns(paste0("group_name_", i)),
            label = NULL,
            value = groups[[i]]$name,
            placeholder = paste("Group", i),
            width = "120px"
          ),
          numericInput(
            ns(paste0("group_subj_", i)),
            label = NULL,
            value = groups[[i]]$n_subj,
            min = 1,
            width = "80px"
          ),
          span(class = "text-muted small", "subjects")
        )
      })
    })

    observeEvent(input$add_group, {
      groups <- local_rv$groups
      new_idx <- length(groups) + 1
      groups[[new_idx]] <- list(name = paste("Group", new_idx), n_subj = 10)
      local_rv$groups <- groups
    })

    observeEvent(input$remove_group, {
      groups <- local_rv$groups
      if (length(groups) > 1) {
        local_rv$groups <- groups[-length(groups)]
      }
    })

    # Sync group inputs back to local_rv
    observe({
      groups <- local_rv$groups
      for (i in seq_along(groups)) {
        name_input <- input[[paste0("group_name_", i)]]
        subj_input <- input[[paste0("group_subj_", i)]]

        if (!is.null(name_input)) {
          groups[[i]]$name <- name_input
        }
        if (!is.null(subj_input)) {
          groups[[i]]$n_subj <- as.integer(subj_input)
        }
      }
      local_rv$groups <- groups
    })

    output$design_summary <- renderUI({
      groups <- local_rv$groups
      n_cond <- input$num_conditions

      total_subj <- sum(sapply(groups, function(g) g$n_subj))
      total_obs <- total_subj * n_cond

      div(
        class = "small text-muted mt-3 p-2 bg-light rounded",
        sprintf("Total: %d subjects, %d observations", total_subj, total_obs)
      )
    })

    # =========================================================================
    # Method Description
    # =========================================================================

    output$method_description <- renderUI({
      method <- input$method

      desc <- switch(
        method,
        task = "Identifies brain patterns that differ across task conditions. Best for experimental designs with distinct conditions.",
        behavior = "Finds brain patterns correlated with behavioral measures (RT, accuracy, etc.).",
        multiblock = "Combines task effects with behavioral correlations for richer analyses.",
        ""
      )

      p(class = "small text-muted mt-2", desc)
    })

    # =========================================================================
    # Status Indicators
    # =========================================================================

    output$data_status <- renderUI({
      if (local_rv$data_loaded) {
        status_dot("complete")
      } else {
        status_dot("pending")
      }
    })

    output$design_status <- renderUI({
      groups <- local_rv$groups
      n_cond <- input$num_conditions

      valid <- length(groups) > 0 &&
        all(sapply(groups, function(g) g$n_subj > 0)) &&
        n_cond > 0

      if (valid) {
        status_dot("complete")
      } else {
        status_dot("pending")
      }
    })

    # =========================================================================
    # Validation
    # =========================================================================

    validate_setup <- reactive({
      errors <- character(0)

      # Check data
      if (input$data_source == "manual" && !local_rv$data_loaded) {
        errors <- c(errors, "No data matrices loaded")
      }

      if (input$data_source == "bids") {
        dir_info <- input$bids_dir
        if (is.integer(dir_info)) {
          errors <- c(errors, "No BIDS directory selected")
        }
      }

      # Check design
      groups <- local_rv$groups
      if (length(groups) == 0) {
        errors <- c(errors, "At least one group required")
      }

      if (any(sapply(groups, function(g) g$n_subj < 1))) {
        errors <- c(errors, "All groups must have at least 1 subject")
      }

      n_cond <- input$num_conditions
      if (is.na(n_cond) || n_cond < 1) {
        errors <- c(errors, "At least one condition required")
      }

      # Check bootstrap requirements
      if (input$num_boot > 0 && any(sapply(groups, function(g) g$n_subj < 3))) {
        errors <- c(errors, "Bootstrap requires at least 3 subjects per group")
      }

      errors
    })

    output$validation_messages <- renderUI({
      errors <- validate_setup()

      if (length(errors) == 0) return(NULL)

      div(
        class = "me-3",
        lapply(errors, function(err) {
          validation_message(err, "error")
        })
      )
    })

    # Enable/disable continue button
    observe({
      errors <- validate_setup()
      if (length(errors) == 0) {
        shinyjs::enable("btn_continue")
      } else {
        shinyjs::disable("btn_continue")
      }
    })

    # =========================================================================
    # Continue Handler
    # =========================================================================

    observeEvent(input$btn_continue, {
      errors <- validate_setup()

      if (length(errors) > 0) {
        showNotification("Please fix validation errors", type = "error")
        return()
      }

      # Build spec
      groups <- local_rv$groups
      n_cond <- input$num_conditions

      # Determine method integer
      method_int <- switch(
        input$method,
        task = 1L,
        behavior = 3L,
        multiblock = 4L,
        1L
      )

      # Create spec
      spec <- plsrri::pls_spec()

      # Add data
      if (!is.null(local_rv$data_matrices)) {
        spec$datamat_lst <- local_rv$data_matrices
      }

      spec$num_subj_lst <- sapply(groups, function(g) as.integer(g$n_subj))
      spec$num_cond <- as.integer(n_cond)
      spec$method <- method_int
      spec$num_perm <- as.integer(input$num_perm)
      spec$num_boot <- as.integer(input$num_boot)
      spec$clim <- as.integer(input$confidence)
      spec$boot_type <- input$boot_type
      spec$num_split <- as.integer(input$num_split)
      spec$meancentering_type <- as.integer(input$meancentering)
      spec$groups <- sapply(groups, function(g) g$name)
      spec$mask <- local_rv$mask

      local_rv$spec <- spec

      # Trigger continue
      continue_trigger(continue_trigger() + 1)
    })

    # =========================================================================
    # Return Values
    # =========================================================================

    list(
      continue = continue_trigger,
      spec = reactive({ local_rv$spec })
    )
  })
}
