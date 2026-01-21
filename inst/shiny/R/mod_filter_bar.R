# Filter Bar Module
# Horizontal controls for LV selection, thresholds, and display options
#
# Uses pure computation functions from fct_brain_viewer.R:
# - build_lv_choices()

#' Filter Bar Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
filter_bar_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "pls-filter-bar",

    # LV selector
    div(
      class = "pls-filter-item",
      span(class = "pls-filter-label", "LV:"),
      div(
        `data-test` = "filter-lv-select",
        selectInput(
          ns("lv"),
          label = NULL,
          choices = c("All" = "all", "LV1" = "1"),
          selected = "1",
          width = "100px"
        )
      )
    ),

    div(class = "pls-filter-separator"),

    # BSR threshold
    div(
      class = "pls-filter-item",
      span(class = "pls-filter-label", "BSR \u2265"),
      div(
        `data-test` = "filter-bsr-threshold",
        numericInput(
          ns("bsr_threshold"),
          label = NULL,
          value = 3.0,
          min = 0,
          max = 10,
          step = 0.5,
          width = "70px"
        )
      )
    ),

    div(class = "pls-filter-separator"),

    # P-value threshold
    div(
      class = "pls-filter-item",
      span(class = "pls-filter-label", "p <"),
      div(
        `data-test` = "filter-p-threshold",
        numericInput(
          ns("p_threshold"),
          label = NULL,
          value = 0.05,
          min = 0.001,
          max = 0.10,
          step = 0.01,
          width = "70px"
        )
      )
    ),

    div(class = "pls-filter-separator"),

    # View mode
    div(
      class = "pls-filter-item",
      span(class = "pls-filter-label", "View:"),
      div(
        `data-test` = "filter-view-mode",
        selectInput(
          ns("view_mode"),
          label = NULL,
          choices = c(
            "Montage" = "montage",
            "Orthogonal" = "ortho"
          ),
          selected = "montage",
          width = "110px"
        )
      )
    ),

    div(class = "pls-filter-separator"),

    # Display what
    div(
      class = "pls-filter-item",
      span(class = "pls-filter-label", "Show:"),
      div(
        `data-test` = "filter-what",
        selectInput(
          ns("what"),
          label = NULL,
          choices = c(
            "BSR" = "bsr",
            "Salience" = "salience"
          ),
          selected = "bsr",
          width = "100px"
        )
      )
    )
  )
}

#' Filter Bar Module Server
#'
#' @param id Module namespace ID
#' @param result_rv Reactive containing pls_result
#' @param state_rv Reactive values for app state
#' @return List of reactive filter values
#' @keywords internal
filter_bar_server <- function(id, result_rv, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update LV choices when result changes using pure function
    observe({
      result <- result_rv()
      if (is.null(result)) return()

      n_lv <- length(result$s)

      # Build choices using pure function
      p_vals <- if (!is.null(result$perm_result)) result$perm_result$sprob else NULL
      choices <- build_lv_choices(n_lv, p_vals)

      updateSelectInput(session, "lv", choices = choices, selected = "1")
    })

    # Sync to state_rv
    observe({
      state_rv$selected_lv <- if (input$lv == "all") 0L else as.integer(input$lv)
      state_rv$bsr_threshold <- input$bsr_threshold
      state_rv$p_threshold <- input$p_threshold
      state_rv$view_mode <- input$view_mode
    })

    # Return reactive filter values
    list(
      lv = reactive({
        if (input$lv == "all") NULL else as.integer(input$lv)
      }),
      bsr_threshold = reactive({ input$bsr_threshold }),
      p_threshold = reactive({ input$p_threshold }),
      view_mode = reactive({ input$view_mode }),
      what = reactive({ input$what })
    )
  })
}
