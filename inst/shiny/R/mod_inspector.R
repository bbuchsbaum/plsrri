# Inspector Module
# Right-panel context details for selected LV or voxel
#
# Uses pure computation functions from fct_brain_viewer.R:
# - compute_lv_stats(), compute_bsr_summary()

#' Inspector Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
inspector_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "pls-explore-inspector",

    # LV Details Panel
    panel_card(
      title = "LV Details",
      div(
        `data-test` = "inspector-lv-details",
        uiOutput(ns("lv_details"))
      )
    ),

    # Design Scores Panel
    panel_card(
      title = "Design Scores",
      div(
        `data-test` = "inspector-scores-plot",
        plotOutput(ns("scores_plot"), height = "200px")
      )
    ),

    # Export Panel
    panel_card(
      title = "Export",
      div(
        class = "d-grid gap-2",
        actionButton(
          ns("export_nifti"),
          "NIfTI Volume",
          icon = icon("brain"),
          class = "btn-outline-primary btn-sm",
          `data-test` = "inspector-export-nifti"
        ),
        actionButton(
          ns("export_csv"),
          "CSV Tables",
          icon = icon("table"),
          class = "btn-outline-primary btn-sm",
          `data-test` = "inspector-export-csv"
        ),
        actionButton(
          ns("export_pdf"),
          "PDF Figures",
          icon = icon("file-pdf"),
          class = "btn-outline-primary btn-sm",
          `data-test` = "inspector-export-pdf"
        ),
        actionButton(
          ns("export_report"),
          "HTML Report",
          icon = icon("file-code"),
          class = "btn-outline-primary btn-sm",
          `data-test` = "inspector-export-report"
        )
      )
    )
  )
}

#' Inspector Module Server
#'
#' @param id Module namespace ID
#' @param result_rv Reactive containing pls_result
#' @param selected_lv Reactive containing selected LV number
#' @param state_rv Reactive values for app state
#' @keywords internal
inspector_server <- function(id, result_rv, selected_lv, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # LV Details
    output$lv_details <- renderUI({
      result <- result_rv()
      lv <- selected_lv()

      if (is.null(result) || is.null(lv)) {
        return(p(class = "text-muted", "Select an LV to view details"))
      }

      # Get LV statistics using pure function
      lv_stats <- compute_lv_stats(result$s, lv)
      var_exp <- lv_stats$var_explained
      cum_var <- lv_stats$cum_var_explained

      # P-value if available
      p_val <- if (!is.null(result$perm_result)) {
        result$perm_result$sprob[lv]
      } else {
        NA
      }

      # BSR summary using pure function
      bsr_summary <- if (!is.null(result$boot_result)) {
        bsr_vals <- plsrri::bsr(result, lv = lv)
        compute_bsr_summary(bsr_vals, threshold = 3)
      } else {
        NULL
      }

      div(
        # Header
        h5(sprintf("Latent Variable %d", lv)),

        # Stats table
        tags$table(
          class = "table table-sm",
          tags$tbody(
            tags$tr(
              tags$td(class = "text-muted", "Variance Explained"),
              tags$td(class = "text-end", sprintf("%.2f%%", var_exp))
            ),
            tags$tr(
              tags$td(class = "text-muted", "Cumulative"),
              tags$td(class = "text-end", sprintf("%.2f%%", cum_var))
            ),
            if (!is.na(p_val)) {
              tags$tr(
                tags$td(class = "text-muted", "P-value"),
                tags$td(
                  class = paste("text-end", if (p_val < 0.05) "text-success fw-bold" else ""),
                  sprintf("%.4f", p_val)
                )
              )
            }
          )
        ),

        # BSR summary if available
        if (!is.null(bsr_summary)) {
          div(
            class = "mt-3 p-2 bg-light rounded small",
            p(class = "mb-1 fw-bold", "Bootstrap Ratios (|BSR| > 3)"),
            div(
              class = "d-flex justify-content-between",
              span(class = "text-success",
                   sprintf("+%d voxels", bsr_summary$n_pos)),
              span(class = "text-danger",
                   sprintf("-%d voxels", bsr_summary$n_neg))
            ),
            div(
              class = "d-flex justify-content-between text-muted",
              span(sprintf("Max: %.2f", bsr_summary$max_pos)),
              span(sprintf("Min: %.2f", bsr_summary$max_neg))
            )
          )
        }
      )
    })

    # Design Scores Plot
    output$scores_plot <- renderPlot({
      result <- result_rv()
      lv <- selected_lv()

      if (is.null(result) || is.null(lv)) {
        plot.new()
        text(0.5, 0.5, "No LV selected", col = "#6B7280")
        return()
      }

      tryCatch({
        p <- plsrri::plot_scores(
          result,
          lv = lv,
          type = "design",
          plot_type = "bar"
        )
        print(p)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, "Error generating plot", col = "#EF4444", cex = 0.9)
      })
    }, bg = "transparent")

    # Export handlers
    observeEvent(input$export_nifti, {
      result <- result_rv()
      lv <- selected_lv()

      if (is.null(result) || is.null(result$mask)) {
        showNotification("NIfTI export requires a brain mask", type = "warning")
        return()
      }

      showModal(modalDialog(
        title = "Export NIfTI Volume",
        size = "m",

        textInput(
          ns("nifti_filename"),
          "Filename",
          value = sprintf("pls_lv%d_bsr.nii.gz", lv)
        ),

        radioButtons(
          ns("nifti_what"),
          "Export",
          choices = c(
            "Bootstrap Ratio" = "bsr",
            "Salience" = "salience"
          ),
          selected = "bsr"
        ),

        if (lv == 0 || is.null(lv)) {
          checkboxInput(ns("nifti_all_lvs"), "Export all LVs", value = TRUE)
        },

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("do_export_nifti"), "Export", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$do_export_nifti, {
      result <- result_rv()
      lv <- selected_lv()
      what <- input$nifti_what
      filename <- input$nifti_filename

      tryCatch({
        # Get values
        if (what == "bsr") {
          values <- plsrri::bsr(result, lv = lv)
        } else {
          values <- plsrri::salience(result, lv = lv)
        }

        # Convert to volume
        vol <- plsrri:::.values_to_neurovol(values, result$mask)

        # Write
        neuroim2::write_vol(vol, filename)

        removeModal()
        showNotification(paste("Exported:", filename), type = "message")

      }, error = function(e) {
        showNotification(paste("Export error:", e$message), type = "error")
      })
    })

    observeEvent(input$export_csv, {
      result <- result_rv()
      lv <- selected_lv()

      showModal(modalDialog(
        title = "Export CSV Tables",
        size = "m",

        checkboxGroupInput(
          ns("csv_tables"),
          "Select tables to export",
          choices = c(
            "Singular Values" = "singular",
            "Design Scores" = "scores",
            "Design Loadings" = "loadings",
            "P-values" = "pvalues"
          ),
          selected = c("singular", "scores")
        ),

        textInput(
          ns("csv_prefix"),
          "Filename prefix",
          value = "pls_results"
        ),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("do_export_csv"), "Export", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$do_export_csv, {
      result <- result_rv()
      tables <- input$csv_tables
      prefix <- input$csv_prefix

      tryCatch({
        for (tbl in tables) {
          filename <- sprintf("%s_%s.csv", prefix, tbl)

          df <- switch(
            tbl,
            singular = data.frame(
              LV = seq_along(result$s),
              SingularValue = result$s,
              VarExplained = (result$s^2 / sum(result$s^2)) * 100
            ),
            scores = as.data.frame(plsrri::scores(result, type = "design")),
            loadings = as.data.frame(plsrri::loadings(result)),
            pvalues = if (!is.null(result$perm_result)) {
              data.frame(
                LV = seq_along(result$perm_result$sprob),
                pvalue = result$perm_result$sprob
              )
            } else NULL
          )

          if (!is.null(df)) {
            write.csv(df, filename, row.names = FALSE)
          }
        }

        removeModal()
        showNotification("CSV files exported", type = "message")

      }, error = function(e) {
        showNotification(paste("Export error:", e$message), type = "error")
      })
    })

    observeEvent(input$export_pdf, {
      result <- result_rv()
      lv <- selected_lv()

      showModal(modalDialog(
        title = "Export PDF Figures",
        size = "m",

        checkboxGroupInput(
          ns("pdf_plots"),
          "Select plots to export",
          choices = c(
            "Singular Values" = "singular",
            "Design Scores" = "scores",
            "Loadings" = "loadings",
            "Brain Map" = "brain"
          ),
          selected = c("singular", "scores")
        ),

        textInput(
          ns("pdf_filename"),
          "Filename",
          value = "pls_figures.pdf"
        ),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("do_export_pdf"), "Export", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$do_export_pdf, {
      result <- result_rv()
      lv <- selected_lv()
      plots <- input$pdf_plots
      filename <- input$pdf_filename

      tryCatch({
        pdf(filename, width = 8, height = 6)

        for (plt in plots) {
          p <- switch(
            plt,
            singular = plsrri::plot_singular_values(result),
            scores = plsrri::plot_scores(result, lv = lv, type = "design"),
            loadings = plsrri::plot_loadings(result, lv = lv),
            brain = if (!is.null(result$mask)) {
              plsrri::plot_brain(result, lv = lv, what = "bsr", threshold = 3)
            } else NULL
          )

          if (!is.null(p)) {
            print(p)
          }
        }

        dev.off()

        removeModal()
        showNotification(paste("Exported:", filename), type = "message")

      }, error = function(e) {
        try(dev.off(), silent = TRUE)
        showNotification(paste("Export error:", e$message), type = "error")
      })
    })

    observeEvent(input$export_report, {
      result <- result_rv()

      showModal(modalDialog(
        title = "Generate HTML Report",
        size = "m",

        textInput(
          ns("report_filename"),
          "Filename",
          value = "pls_report.html"
        ),

        textInput(
          ns("report_title"),
          "Report Title",
          value = "PLS Analysis Report"
        ),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("do_export_report"), "Generate", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$do_export_report, {
      result <- result_rv()
      filename <- input$report_filename
      title <- input$report_title

      tryCatch({
        # Check if render_report exists
        if (exists("render_report", where = asNamespace("plsrri"))) {
          plsrri::render_report(result, output_file = filename, title = title)
          removeModal()
          showNotification(paste("Report generated:", filename), type = "message")
        } else {
          showNotification("Report generation not yet implemented", type = "warning")
          removeModal()
        }
      }, error = function(e) {
        showNotification(paste("Report error:", e$message), type = "error")
      })
    })
  })
}
