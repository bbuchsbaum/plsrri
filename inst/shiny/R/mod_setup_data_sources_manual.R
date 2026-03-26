# Setup module manual matrices data source handlers

setup_register_manual_data_source <- function(input, output, session, local_rv) {
  # Manual file upload
  observeEvent(input$data_files, {
    req(input$data_files)

    files <- input$data_files
    matrices <- list()

    for (i in seq_len(nrow(files))) {
      file_path <- files$datapath[i]
      file_name <- files$name[i]

      # Delegate to pure function for file parsing
      mat <- parse_uploaded_file(file_path, file_name)

      if (!is.null(mat)) {
        matrices[[length(matrices) + 1]] <- mat
      } else {
        showNotification(
          paste("Error loading", file_name, ": file could not be parsed as matrix"),
          type = "error"
        )
      }
    }

    if (length(matrices) > 0) {
      local_rv$data_matrices <- matrices
      local_rv$data_loaded <- TRUE

      # Update groups to match number of matrices
      if (length(matrices) != length(local_rv$groups)) {
        n_cond <- suppressWarnings(as.integer(input$num_conditions)[1])
        if (is.na(n_cond) || n_cond < 1) n_cond <- 1L

        local_rv$groups <- lapply(seq_along(matrices), function(i) {
          list(
            name = paste("Group", i),
            n_subj = nrow(matrices[[i]]) / n_cond
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
}

