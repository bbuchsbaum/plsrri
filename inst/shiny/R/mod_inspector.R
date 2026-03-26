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

    # Bootstrap / resampling diagnostics
    panel_card(
      title = "Resampling Diagnostics",
      div(
        `data-test` = "inspector-resampling-diagnostics",
        uiOutput(ns("resampling_diagnostics"))
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

    sanitize_filename <- function(filename, default, allowed_ext = NULL, default_ext = NULL) {
      filename <- as.character(filename)[1]
      if (is.na(filename) || !nzchar(trimws(filename))) filename <- default

      # Disallow directory components (safer for hosted Shiny)
      filename <- basename(filename)
      filename <- gsub("[<>:\"/\\\\|?*]+", "_", filename)

      if (!is.null(allowed_ext) && length(allowed_ext) > 0) {
        lower_name <- tolower(filename)
        has_allowed <- any(vapply(allowed_ext, function(ext) {
          endsWith(lower_name, tolower(ext))
        }, logical(1)))

        if (!has_allowed) {
          if (!is.null(default_ext) && nzchar(default_ext)) {
            filename <- paste0(filename, default_ext)
          }
        }
      } else if (!is.null(default_ext) && nzchar(default_ext)) {
        if (!endsWith(tolower(filename), tolower(default_ext))) {
          filename <- paste0(filename, default_ext)
        }
      }

      filename
    }

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

    # Bootstrap / resampling diagnostics (not LV-specific)
    output$resampling_diagnostics <- renderUI({
      result <- result_rv()

      if (is.null(result)) {
        return(p(class = "text-muted", "No results loaded"))
      }

      br <- result$boot_result
      if (is.null(br)) {
        return(p(class = "text-muted", "No bootstrap results available"))
      }

      # Low-variability counts per behavior variable (MATLAB rri_islowvariability)
      low_var <- br$num_LowVariability_behav_boots
      low_var_ui <- NULL

      if (!is.null(low_var)) {
        low_var <- as.integer(low_var)
        n_boot <- as.integer(br$num_boot %||% NA_integer_)
        behav_names <- names(low_var)
        if (is.null(behav_names) || all(behav_names == "")) {
          behav_names <- paste0("Behav", seq_along(low_var))
        }

        df <- data.frame(
          Behavior = behav_names,
          Count = low_var,
          Percent = if (!is.na(n_boot) && n_boot > 0) round(100 * low_var / n_boot, 1) else NA_real_,
          check.names = FALSE
        )

        low_var_ui <- div(
          class = "small",
          p(class = "mb-1 fw-bold", "Low Variability (per behavior)"),
          tags$table(
            class = "table table-sm mb-2",
            tags$thead(
              tags$tr(
                tags$th("Behavior"),
                tags$th(class = "text-end", "Count"),
                tags$th(class = "text-end", "% of boots")
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(df)), function(i) {
                tags$tr(
                  tags$td(df$Behavior[i]),
                  tags$td(class = "text-end", df$Count[i]),
                  tags$td(class = "text-end",
                          if (is.na(df$Percent[i])) "" else sprintf("%.1f%%", df$Percent[i]))
                )
              })
            )
          ),
          p(
            class = "text-muted mb-0",
            "High values suggest a behavior variable has repeated values in many bootstrap draws (MATLAB `rri_islowvariability`)."
          )
        )
      }

      # badbeh details: which group/condition/behavior forced a re-draw due to SD=0
      badbeh <- br$badbeh
      countnewtotal <- as.integer(br$countnewtotal %||% 0L)
      badbeh_ui <- NULL

      if (!is.null(badbeh) && countnewtotal > 0L) {
        rows <- list()
        idx <- 0L

        for (g in seq_along(badbeh)) {
          g_list <- badbeh[[g]]
          if (is.null(g_list) || length(g_list) == 0L) next

          for (attempt in seq_along(g_list)) {
            mat <- g_list[[attempt]]
            if (is.null(mat) || !is.matrix(mat)) next

            hits <- which(mat > 0, arr.ind = TRUE)
            if (nrow(hits) == 0L) next

            for (i in seq_len(nrow(hits))) {
              idx <- idx + 1L
              rows[[idx]] <- data.frame(
                Attempt = attempt,
                Group = g,
                Condition = hits[i, 1],
                Behavior = hits[i, 2],
                Count = mat[hits[i, 1], hits[i, 2]],
                check.names = FALSE
              )
            }
          }
        }

        detail_df <- if (length(rows) > 0L) {
          df <- do.call(rbind, rows)
          df[order(df$Attempt, df$Group, df$Condition, df$Behavior), , drop = FALSE]
        } else {
          NULL
        }

        badbeh_ui <- div(
          class = "small mt-3",
          p(
            class = "mb-1 fw-bold text-warning",
            icon("triangle-exclamation"),
            sprintf("Bootstrap re-draws: %d", countnewtotal)
          ),
          p(
            class = "text-muted mb-2",
            "Some bootstrap samples had zero within-condition SD for a behavior variable; those draws were re-sampled (MATLAB `badbeh`)."
          ),
          if (!is.null(detail_df)) {
            tags$details(
              tags$summary("Show details"),
              tags$table(
                class = "table table-sm mt-2",
                tags$thead(
                  tags$tr(
                    tags$th(class = "text-end", "Attempt"),
                    tags$th(class = "text-end", "Group"),
                    tags$th(class = "text-end", "Cond"),
                    tags$th(class = "text-end", "Behav"),
                    tags$th(class = "text-end", "Count")
                  )
                ),
                tags$tbody(
                  lapply(seq_len(nrow(detail_df)), function(i) {
                    tags$tr(
                      tags$td(class = "text-end", detail_df$Attempt[i]),
                      tags$td(class = "text-end", detail_df$Group[i]),
                      tags$td(class = "text-end", detail_df$Condition[i]),
                      tags$td(class = "text-end", detail_df$Behavior[i]),
                      tags$td(class = "text-end", detail_df$Count[i])
                    )
                  })
                )
              )
            )
          }
        )
      }

      tagList(
        if (!is.null(low_var_ui)) low_var_ui,
        if (!is.null(badbeh_ui)) badbeh_ui,
        if (is.null(low_var_ui) && is.null(badbeh_ui)) {
          p(class = "text-muted", "No resampling diagnostics available for this result.")
        }
      )
    })

    # =========================================================================
    # Download handlers (no server-side writes to working directory)
    # =========================================================================

    output$download_nifti <- downloadHandler(
      filename = function() {
        lv <- suppressWarnings(as.integer(selected_lv())[1])
        if (is.na(lv) || lv < 1) lv <- 1L
        what <- as.character(input$nifti_what)[1]
        if (!what %in% c("bsr", "salience")) what <- "bsr"
        default <- sprintf("pls_lv%d_%s.nii.gz", lv, what)
        sanitize_filename(
          input$nifti_filename,
          default = default,
          allowed_ext = c(".nii.gz", ".nii"),
          default_ext = ".nii.gz"
        )
      },
      content = function(file) {
        result <- result_rv()
        req(result)
        req(result$mask)
        req(requireNamespace("neuroim2", quietly = TRUE))

        lv <- suppressWarnings(as.integer(selected_lv())[1])
        if (is.na(lv) || lv < 1) lv <- 1L
        what <- as.character(input$nifti_what)[1]
        if (!what %in% c("bsr", "salience")) what <- "bsr"

        values <- if (what == "bsr") {
          plsrri::bsr(result, lv = lv)
        } else {
          plsrri::salience(result, lv = lv)
        }

        vol <- plsrri:::.values_to_neurovol(values, result$mask)
        neuroim2::write_vol(vol, file)
      }
    )

    output$download_csv <- downloadHandler(
      filename = function() {
        prefix <- as.character(input$csv_prefix)[1]
        if (is.na(prefix) || !nzchar(trimws(prefix))) prefix <- "pls_results"
        sanitize_filename(
          paste0(prefix, "_tables.zip"),
          default = "pls_results_tables.zip",
          allowed_ext = c(".zip"),
          default_ext = ".zip"
        )
      },
      content = function(file) {
        result <- result_rv()
        req(result)

        tables <- as.character(input$csv_tables)
        if (length(tables) == 0) {
          stop("No tables selected")
        }

        prefix <- as.character(input$csv_prefix)[1]
        if (is.na(prefix) || !nzchar(trimws(prefix))) prefix <- "pls_results"
        prefix <- basename(prefix)
        prefix <- gsub("[<>:\"/\\\\|?*]+", "_", prefix)

        tmp_dir <- tempfile("plsrri_csv_")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

        csv_paths <- character(0)

        for (tbl in tables) {
          out_name <- sprintf("%s_%s.csv", prefix, tbl)
          out_path <- file.path(tmp_dir, out_name)

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
            } else {
              NULL
            },
            NULL
          )

          if (!is.null(df)) {
            utils::write.csv(df, out_path, row.names = FALSE)
            csv_paths <- c(csv_paths, out_path)
          }
        }

        if (length(csv_paths) == 0) {
          stop("No CSV files could be generated for the selected tables")
        }

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(tmp_dir)

        utils::zip(zipfile = file, files = basename(csv_paths))
      }
    )

    output$download_pdf <- downloadHandler(
      filename = function() {
        sanitize_filename(
          input$pdf_filename,
          default = "pls_figures.pdf",
          allowed_ext = c(".pdf"),
          default_ext = ".pdf"
        )
      },
      content = function(file) {
        result <- result_rv()
        req(result)

        lv <- suppressWarnings(as.integer(selected_lv())[1])
        if (is.na(lv) || lv < 1) lv <- 1L
        plots <- as.character(input$pdf_plots)
        if (length(plots) == 0) stop("No plots selected")

        grDevices::pdf(file, width = 8, height = 6)
        on.exit(grDevices::dev.off(), add = TRUE)

        for (plt in plots) {
          p <- switch(
            plt,
            singular = plsrri::plot_singular_values(result),
            scores = plsrri::plot_scores(result, lv = lv, type = "design"),
            loadings = plsrri::plot_loadings(result, lv = lv),
            brain = if (!is.null(result$mask)) {
              plsrri::plot_brain(result, lv = lv, what = "bsr", threshold = 3)
            } else {
              NULL
            },
            NULL
          )
          if (!is.null(p)) print(p)
        }
      }
    )

    output$download_report <- downloadHandler(
      filename = function() {
        sanitize_filename(
          input$report_filename,
          default = "pls_report.html",
          allowed_ext = c(".html"),
          default_ext = ".html"
        )
      },
      content = function(file) {
        result <- result_rv()
        req(result)

        title <- as.character(input$report_title)[1]
        if (is.na(title) || !nzchar(trimws(title))) title <- "PLS Analysis Report"

        if (exists("render_report", where = asNamespace("plsrri"))) {
          plsrri::render_report(result, output_file = file, title = title)
        } else {
          html <- paste0(
            "<!doctype html><html><head><meta charset='utf-8'><title>",
            title,
            "</title></head><body><h1>",
            title,
            "</h1><p>Report generation is not yet implemented.</p></body></html>"
          )
          writeLines(html, con = file, useBytes = TRUE)
        }
      }
    )

    # =========================================================================
    # Export modals (downloadButtons wired to downloadHandlers above)
    # =========================================================================

    observeEvent(input$export_nifti, {
      result <- result_rv()
      if (is.null(result) || is.null(result$mask)) {
        showNotification("NIfTI export requires a brain mask", type = "warning")
        return()
      }

      lv <- suppressWarnings(as.integer(selected_lv())[1])
      if (is.na(lv) || lv < 1) lv <- 1L

      showModal(modalDialog(
        title = "Download NIfTI Volume",
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
        footer = tagList(
          modalButton("Close"),
          downloadButton(ns("download_nifti"), "Download", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$export_csv, {
      showModal(modalDialog(
        title = "Download CSV Tables",
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
        p(class = "text-muted small mb-0", "Downloads a .zip containing one CSV per selected table."),
        footer = tagList(
          modalButton("Close"),
          downloadButton(ns("download_csv"), "Download .zip", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$export_pdf, {
      showModal(modalDialog(
        title = "Download PDF Figures",
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
          modalButton("Close"),
          downloadButton(ns("download_pdf"), "Download", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$export_report, {
      showModal(modalDialog(
        title = "Download HTML Report",
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
          modalButton("Close"),
          downloadButton(ns("download_report"), "Download", class = "btn-primary")
        )
      ))
    })
  })
}
