# Explore Module
# Results exploration with LV list, brain viewer, and scores

#' Explore Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
explore_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "pls-fade-in",
    style = "display: flex; flex-direction: column; flex: 1;",

    # Filter bar at top
    filter_bar_ui(ns("filters")),

    # Three-panel layout
    div(
      class = "pls-explore-layout",

      # Left sidebar: LV list + variance chart
      div(
        class = "pls-explore-sidebar",

        # LV List
        panel_card(
          title = "Latent Variables",
          div(
            class = "pls-lv-list",
            `data-test` = "explore-lv-list",
            uiOutput(ns("lv_list"))
          )
        ),

        # Variance Explained
        panel_card(
          title = "Variance Explained",
          div(
            `data-test` = "explore-variance-plot",
            plotOutput(ns("variance_plot"), height = "150px")
          )
        )
      ),

      # Main area: Brain viewer + scores
      div(
        class = "pls-explore-main",

        # Brain viewer
        brain_viewer_ui(ns("brain")),

        # Design scores
        panel_card(
          title = uiOutput(ns("scores_title"), inline = TRUE),
          div(
            `data-test` = "explore-scores-plot",
            plotOutput(ns("main_scores_plot"), height = "200px")
          )
        )
      ),

      # Right inspector panel
      inspector_ui(ns("inspector"))
    )
  )
}

#' Explore Module Server
#'
#' @param id Module namespace ID
#' @param state_rv Reactive values for app state
#' @keywords internal
explore_server <- function(id, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local reactive values
    local_rv <- reactiveValues(
      selected_lv = 1L
    )

    # Result reactive
    result_rv <- reactive({
      state_rv$result
    })

    # Selected LV reactive
    selected_lv <- reactive({
      local_rv$selected_lv
    })

    # =========================================================================
    # Filter Bar
    # =========================================================================

    filters <- filter_bar_server("filters", result_rv, state_rv)

    # Sync filter LV to local state
    observe({
      lv <- filters$lv()
      if (!is.null(lv) && lv > 0) {
        local_rv$selected_lv <- lv
      }
    })

    # =========================================================================
    # LV List
    # =========================================================================

    output$lv_list <- renderUI({
      result <- result_rv()
      if (is.null(result)) {
        return(p(class = "text-muted p-3", "No results loaded"))
      }

      n_lv <- length(result$s)
      var_exp <- (result$s^2 / sum(result$s^2)) * 100
      selected <- local_rv$selected_lv

      # P-values if available
      p_vals <- if (!is.null(result$perm_result)) {
        result$perm_result$sprob
      } else {
        rep(NA, n_lv)
      }

      # Generate list items
      items <- lapply(seq_len(n_lv), function(i) {
        is_sig <- !is.na(p_vals[i]) && p_vals[i] < 0.05
        is_selected <- i == selected

        # Status based on significance
        dot_status <- if (is_sig) "complete" else "pending"

        div(
          class = paste("pls-lv-item", if (is_selected) "selected" else ""),
          id = ns(paste0("lv_item_", i)),
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})",
                            ns("select_lv"), i),

          span(class = "pls-lv-dot", status_dot(dot_status)),
          span(class = "pls-lv-name", sprintf("LV%d", i)),
          span(
            class = "pls-lv-pvalue",
            if (!is.na(p_vals[i])) sprintf("p=%.3f", p_vals[i]) else ""
          ),
          span(class = "pls-lv-variance", sprintf("%.1f%%", var_exp[i]))
        )
      })

      tagList(items)
    })

    # Handle LV selection
    observeEvent(input$select_lv, {
      local_rv$selected_lv <- input$select_lv

      # Also update filter bar
      updateSelectInput(
        session,
        ns("filters-lv"),
        selected = as.character(input$select_lv)
      )
    })

    # =========================================================================
    # Variance Plot
    # =========================================================================

    output$variance_plot <- renderPlot({
      result <- result_rv()
      if (is.null(result)) {
        plot.new()
        return()
      }

      tryCatch({
        plsrri::plot_singular_values(
          result,
          show_pvalue = FALSE,
          title = NULL
        ) +
          ggplot2::theme(
            plot.margin = ggplot2::margin(5, 10, 5, 10),
            axis.title = ggplot2::element_text(size = 9),
            axis.text = ggplot2::element_text(size = 8)
          )
      }, error = function(e) {
        plot.new()
      })
    }, bg = "transparent")

    # =========================================================================
    # Brain Viewer
    # =========================================================================

    brain_info <- brain_viewer_server("brain", result_rv, filters)

    # =========================================================================
    # Main Scores Plot
    # =========================================================================

    output$scores_title <- renderUI({
      lv <- local_rv$selected_lv
      sprintf("Design Scores (LV%d)", lv)
    })

    output$main_scores_plot <- renderPlot({
      result <- result_rv()
      lv <- local_rv$selected_lv

      if (is.null(result)) {
        plot.new()
        text(0.5, 0.5, "No results", col = "#6B7280")
        return()
      }

      tryCatch({
        p <- plsrri::plot_scores(
          result,
          lv = lv,
          type = "design",
          plot_type = "bar"
        ) +
          ggplot2::theme(
            plot.margin = ggplot2::margin(10, 15, 10, 15)
          )
        print(p)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, "Error generating plot", col = "#EF4444", cex = 0.9)
      })
    }, bg = "transparent")

    # =========================================================================
    # Inspector
    # =========================================================================

    inspector_server("inspector", result_rv, selected_lv, state_rv)
  })
}
