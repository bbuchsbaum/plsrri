# PLS Shiny UI Components
# Reusable UI components: stepper, cards, status dots, filter bar

#' Workflow Stepper Component
#'
#' @description
#' Horizontal workflow stepper showing Setup -> Analyze -> Explore.
#'
#' @param id Namespace ID
#' @return Shiny UI element
#' @keywords internal
stepper_ui <- function(id) {
 ns <- shiny::NS(id)

 shiny::div(
   class = "pls-stepper",
   shiny::div(
     class = "pls-stepper-container",
     # Step 1: Setup
     shiny::actionLink(
       inputId = ns("step_1"),
       class = "pls-step",
       shiny::div(
         class = "pls-step-content",
         shiny::span(class = "pls-step-dot", id = ns("dot_1")),
         shiny::span(class = "pls-step-label", "Setup")
       )
     ),
     # Connector 1-2
     shiny::div(class = "pls-step-connector"),
     # Step 2: Analyze
     shiny::actionLink(
       inputId = ns("step_2"),
       class = "pls-step",
       shiny::div(
         class = "pls-step-content",
         shiny::span(class = "pls-step-dot", id = ns("dot_2")),
         shiny::span(class = "pls-step-label", "Analyze")
       )
     ),
     # Connector 2-3
     shiny::div(class = "pls-step-connector"),
     # Step 3: Explore
     shiny::actionLink(
       inputId = ns("step_3"),
       class = "pls-step",
       shiny::div(
         class = "pls-step-content",
         shiny::span(class = "pls-step-dot", id = ns("dot_3")),
         shiny::span(class = "pls-step-label", "Explore")
       )
     )
   )
 )
}

#' Stepper Server Logic
#'
#' @param id Module ID
#' @param state_rv Reactive values for app state
#' @param set_step Function to set current step
#' @return Module server
#' @keywords internal
stepper_server <- function(id, state_rv, set_step) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update stepper appearance based on state
    shiny::observe({
      current <- state_rv$step
      max_step <- state_rv$max_step

      for (i in 1:3) {
        # Determine dot state
        dot_class <- if (i < current) {
          "pls-step-dot complete"
        } else if (i == current) {
          "pls-step-dot active"
        } else if (i <= max_step) {
          "pls-step-dot accessible"
        } else {
          "pls-step-dot pending"
        }

        # Update via JavaScript
        shiny::updateActionLink(
          session,
          paste0("step_", i),
          label = NULL
        )

        shinyjs::runjs(sprintf(
          "document.getElementById('%s').className = '%s';",
          ns(paste0("dot_", i)),
          dot_class
        ))

        # Enable/disable step links
        step_id <- ns(paste0("step_", i))
        if (i <= max_step) {
          shinyjs::enable(paste0("step_", i))
          shinyjs::runjs(sprintf(
            "document.getElementById('%s').classList.remove('disabled');",
            step_id
          ))
        } else {
          shinyjs::disable(paste0("step_", i))
          shinyjs::runjs(sprintf(
            "document.getElementById('%s').classList.add('disabled');",
            step_id
          ))
        }
      }
    })

    # Handle step clicks
    shiny::observeEvent(input$step_1, {
      if (state_rv$max_step >= 1) set_step(1)
    })

    shiny::observeEvent(input$step_2, {
      if (state_rv$max_step >= 2) set_step(2)
    })

    shiny::observeEvent(input$step_3, {
      if (state_rv$max_step >= 3) set_step(3)
    })
  })
}

#' Status Dot Component
#'
#' @description
#' Small status indicator dot (8px).
#'
#' @param status Status: "complete", "active", "error", "pending"
#' @param size Size in pixels (default 8)
#' @return Shiny UI element
#' @keywords internal
status_dot <- function(status = "pending", size = 8) {
  status <- as.character(status)[1]
  if (is.na(status) || !nzchar(status)) status <- "pending"

  color <- switch(
    status,
    complete = "var(--pls-dot-complete)",
    active = "var(--pls-dot-active)",
    error = "var(--pls-dot-error)",
    pending = "var(--pls-dot-pending)",
    "var(--pls-dot-pending)"
  )

  shiny::span(
    class = paste("pls-status-dot", status),
    style = sprintf(
      "width: %dpx; height: %dpx; background-color: %s;",
      size, size, color
    )
  )
}

#' Panel Card Component
#'
#' @description
#' Card wrapper with optional header and status.
#'
#' @param ... Card contents
#' @param title Card title
#' @param status Optional status indicator
#' @param class Additional CSS classes
#' @return Shiny UI element
#' @keywords internal
panel_card <- function(..., title = NULL, status = NULL, class = "") {
  status_node <- NULL
  if (!is.null(status)) {
    if (is.character(status) && length(status) == 1 && !is.na(status) && nzchar(status)) {
      status_node <- status_dot(status)
    } else {
      status_node <- status
    }
  }

  header <- if (!is.null(title)) {
    shiny::div(
      class = "card-header pls-card-header",
      status_node,
      shiny::span(class = "pls-card-title", title)
    )
  }

  bslib::card(
    class = paste("pls-card", class),
    if (!is.null(header)) header,
    bslib::card_body(...)
  )
}

#' Section Header Component
#'
#' @description
#' Section header with optional description.
#'
#' @param title Section title
#' @param description Optional description text
#' @return Shiny UI element
#' @keywords internal
section_header <- function(title, description = NULL) {
  shiny::div(
    class = "pls-section-header",
    shiny::h4(class = "pls-section-title", title),
    if (!is.null(description)) {
      shiny::p(class = "pls-section-desc text-muted", description)
    }
  )
}

#' App Header Component
#'
#' @description
#' Main application header with title and help button.
#'
#' @param title Application title
#' @return Shiny UI element
#' @keywords internal
app_header <- function(title = "PLS Neuroimaging Analysis") {
  shiny::div(
    class = "pls-header",
    shiny::div(
      class = "pls-header-content",
      shiny::div(
        class = "pls-header-title",
        shiny::h1(title)
      ),
      shiny::div(
        class = "pls-header-actions",
        shiny::actionButton(
          "btn_help",
          label = NULL,
          icon = shiny::icon("question-circle"),
          class = "btn-light btn-sm"
        )
      )
    )
  )
}

#' App Footer Component
#'
#' @description
#' Status footer showing summary info.
#'
#' @param id Namespace ID
#' @return Shiny UI element
#' @keywords internal
app_footer_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "pls-footer",
    shiny::div(
      class = "pls-footer-content",
      shiny::span(
        class = "pls-footer-status",
        shiny::uiOutput(ns("status_indicator"), inline = TRUE),
        shiny::textOutput(ns("status_text"), inline = TRUE)
      ),
      shiny::span(class = "pls-footer-separator", "|"),
      shiny::span(
        class = "pls-footer-info",
        shiny::textOutput(ns("info_voxels"), inline = TRUE)
      ),
      shiny::span(class = "pls-footer-separator", "|"),
      shiny::span(
        class = "pls-footer-info",
        shiny::textOutput(ns("info_groups"), inline = TRUE)
      ),
      shiny::span(class = "pls-footer-separator", "|"),
      shiny::span(
        class = "pls-footer-info",
        shiny::textOutput(ns("info_conditions"), inline = TRUE)
      )
    )
  )
}

#' App Footer Server
#'
#' @param id Module ID
#' @param state_rv Reactive values for app state
#' @return Module server
#' @keywords internal
app_footer_server <- function(id, state_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    output$status_indicator <- shiny::renderUI({
      status <- state_rv$analysis_status
      dot_status <- switch(
        status,
        ready = "pending",
        running = "active",
        complete = "complete",
        error = "error",
        "pending"
      )
      status_dot(dot_status)
    })

    output$status_text <- shiny::renderText({
      status <- state_rv$analysis_status
      switch(
        status,
        ready = "Ready",
        running = "Running",
        complete = "Complete",
        error = "Error",
        "Ready"
      )
    })

    output$info_voxels <- shiny::renderText({
      spec <- state_rv$spec
      if (!is.null(spec) && length(spec$datamat_lst) > 0) {
        n_vox <- ncol(spec$datamat_lst[[1]])
        sprintf("Voxels: %s", format(n_vox, big.mark = ","))
      } else {
        "Voxels: --"
      }
    })

    output$info_groups <- shiny::renderText({
      spec <- state_rv$spec
      if (!is.null(spec) && length(spec$datamat_lst) > 0) {
        sprintf("Groups: %d", length(spec$datamat_lst))
      } else {
        "Groups: --"
      }
    })

    output$info_conditions <- shiny::renderText({
      spec <- state_rv$spec
      if (!is.null(spec) && !is.null(spec$num_cond)) {
        sprintf("Conditions: %d", spec$num_cond)
      } else {
        "Conditions: --"
      }
    })
  })
}

#' Collapsible Advanced Options
#'
#' @description
#' Expandable section for advanced options with disclosure triangle.
#'
#' @param id Unique ID for the collapse
#' @param title Section title
#' @param ... Contents
#' @param open Whether to start open
#' @return Shiny UI element
#' @keywords internal
advanced_options <- function(id, title = "Advanced options", ..., open = FALSE) {
  shiny::div(
    class = "pls-advanced",
    shiny::tags$details(
      id = id,
      open = if (isTRUE(open)) "open" else NULL,
      shiny::tags$summary(
        class = "pls-advanced-toggle",
        shiny::icon("chevron-right", class = "pls-advanced-chevron"),
        title
      ),
      shiny::div(
        class = "pls-advanced-content",
        ...
      )
    )
  )
}

#' Help Popover Icon
#'
#' @description
#' Small inline help button that opens a Bootstrap popover (bslib).
#'
#' @param title Popover title (string)
#' @param ... Popover contents (HTML tags or character)
#' @param placement Popover placement
#' @param icon Bootstrap icon name (bsicons)
#' @return Shiny UI element
#' @keywords internal
help_popover <- function(title, ..., placement = "right", icon = "question-circle") {
  title <- as.character(title)[1]
  if (is.na(title) || !nzchar(title)) title <- "Help"

  trigger <- shiny::tags$button(
    type = "button",
    class = "btn btn-link btn-sm p-0 pls-help-icon",
    `aria-label` = paste0("Help: ", title),
    onclick = "event.stopPropagation();",
    onmousedown = "event.stopPropagation();",
    bsicons::bs_icon(icon, size = "1em", a11y = "sem")
  )

  bslib::popover(
    trigger,
    ...,
    title = title,
    placement = placement,
    options = list(
      html = TRUE,
      sanitize = FALSE,
      trigger = "focus",
      container = "body",
      customClass = "pls-help-popover"
    )
  )
}

#' Label With Inline Help
#'
#' @param label Visible label text
#' @param title Popover title
#' @param ... Popover contents
#' @return Shiny tagList
#' @keywords internal
label_with_help <- function(label, title, ...) {
  shiny::tagList(
    shiny::span(label),
    help_popover(title, ...)
  )
}

#' Radio Button Group (Modern Style)
#'
#' @description
#' Radio buttons styled as a button group for method selection.
#'
#' @param id Input ID
#' @param label Label text
#' @param choices Named vector of choices
#' @param selected Initially selected value
#' @return Shiny UI element
#' @keywords internal
radio_button_group <- function(id, label = NULL, choices, selected = NULL) {
  if (is.null(selected)) {
    selected <- choices[1]
  }

  shiny::div(
    class = "pls-radio-group",
    if (!is.null(label)) shiny::tags$label(class = "form-label", label),
    shiny::radioButtons(
      inputId = id,
      label = NULL,
      choices = choices,
      selected = selected,
      inline = FALSE
    )
  )
}

#' Validation Message
#'
#' @description
#' Inline validation message with icon.
#'
#' @param message Message text
#' @param type Message type: "error", "warning", "success", "info"
#' @return Shiny UI element
#' @keywords internal
validation_message <- function(message, type = "error") {
  icon_name <- switch(
    type,
    error = "exclamation-circle",
    warning = "exclamation-triangle",
    success = "check-circle",
    info = "info-circle",
    "info-circle"
  )

  color_class <- switch(
    type,
    error = "text-danger",
    warning = "text-warning",
    success = "text-success",
    info = "text-info",
    "text-muted"
  )

  shiny::div(
    class = paste("pls-validation-message", color_class),
    shiny::icon(icon_name),
    shiny::span(message)
  )
}

#' Action Button (Primary)
#'
#' @description
#' Primary action button with consistent styling.
#'
#' @param id Button ID
#' @param label Button label
#' @param icon Optional icon
#' @param disabled Start disabled
#' @return Shiny UI element
#' @keywords internal
primary_button <- function(id, label, icon = NULL, disabled = FALSE) {
  btn <- shiny::actionButton(
    inputId = id,
    label = label,
    icon = if (!is.null(icon)) shiny::icon(icon) else NULL,
    class = "btn-primary pls-btn-primary"
  )

  if (disabled) {
    btn <- shiny::tagAppendAttributes(btn, disabled = "disabled")
  }

  btn
}

#' Secondary Button
#'
#' @description
#' Secondary/outline action button.
#'
#' @param id Button ID
#' @param label Button label
#' @param icon Optional icon
#' @return Shiny UI element
#' @keywords internal
secondary_button <- function(id, label, icon = NULL) {
  shiny::actionButton(
    inputId = id,
    label = label,
    icon = if (!is.null(icon)) shiny::icon(icon) else NULL,
    class = "btn-outline-secondary pls-btn-secondary"
  )
}

#' Loading Spinner
#'
#' @description
#' Loading indicator for async operations.
#'
#' @param id Optional ID for showing/hiding
#' @param text Loading text
#' @return Shiny UI element
#' @keywords internal
loading_spinner <- function(id = NULL, text = "Loading...") {
  shiny::div(
    id = id,
    class = "pls-loading",
    shiny::div(class = "spinner-border spinner-border-sm text-primary"),
    shiny::span(class = "ms-2", text)
  )
}
