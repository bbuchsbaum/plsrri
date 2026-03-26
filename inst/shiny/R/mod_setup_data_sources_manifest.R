# Setup module manifest data source handlers

setup_register_manifest_data_source <- function(input, output, session, local_rv, roots) {
  output$manifest_path_display <- renderUI({
    file_info <- input$manifest_file
    if (setup_shinyfiles_is_unselected(file_info)) {
      return(p(class = "text-muted small", "No manifest selected"))
    }

    path_df <- shinyFiles::parseFilePath(roots, file_info)
    if (nrow(path_df) == 0) return(p(class = "text-muted small", "No manifest selected"))

    path <- path_df$datapath[1]
    if (is.na(path) || !nzchar(path)) return(p(class = "text-muted small", "No manifest selected"))

    tagList(
      p(class = "small mb-1", icon("file"), path),
      if (!is.null(local_rv$manifest_error) && nzchar(local_rv$manifest_error)) {
        p(class = "text-danger small mb-0", icon("exclamation-triangle"), local_rv$manifest_error)
      }
    )
  })

  observeEvent(input$manifest_file, {
    file_info <- input$manifest_file
    if (setup_shinyfiles_is_unselected(file_info)) {
      local_rv$manifest_path <- NULL
      local_rv$manifest_spec <- NULL
      local_rv$manifest_error <- NULL
      return()
    }

    path_df <- shinyFiles::parseFilePath(roots, file_info)
    if (nrow(path_df) == 0) return()

    path <- path_df$datapath[1]
    if (is.na(path) || !nzchar(path)) return()

    local_rv$manifest_path <- path
    local_rv$manifest_spec <- NULL
    local_rv$manifest_error <- NULL
  }, ignoreInit = TRUE)

  # Manifest mask upload
  observeEvent(input$manifest_mask_file, {
    req(input$manifest_mask_file)

    if (!requireNamespace("neuroim2", quietly = TRUE)) {
      showNotification("Package 'neuroim2' is required to read NIfTI masks", type = "error")
      return()
    }

    tryCatch({
      mask <- neuroim2::read_vol(input$manifest_mask_file$datapath[1])
      local_rv$mask <- mask
    }, error = function(e) {
      local_rv$mask <- NULL
      showNotification(paste("Error reading mask:", e$message), type = "error")
    })
  })

  output$manifest_mask_info <- renderUI({
    if (is.null(local_rv$mask)) return(NULL)

    mask_arr <- as.array(local_rv$mask)
    n_vox <- sum(mask_arr > 0)
    dims <- dim(mask_arr)[1:3]

    p(
      class = "small mb-1",
      icon("check-circle", class = "text-success"),
      sprintf("Mask loaded: %s voxels (%dx%dx%d)", format(n_vox, big.mark = ","), dims[1], dims[2], dims[3])
    )
  })

  # Prepare manifest-derived spec (header validation only; data ingestion happens at run)
  observeEvent(
    list(input$data_source, local_rv$manifest_path, local_rv$mask),
    {
      if (!identical(as.character(input$data_source)[1], "manifest")) return()
      if (is.null(local_rv$manifest_path) || is.null(local_rv$mask)) return()

      local_rv$manifest_error <- NULL
      local_rv$manifest_spec <- tryCatch({
        plsrri::pls_spec(mask = local_rv$mask) |>
          plsrri::add_subjects_manifest(
            manifest = local_rv$manifest_path,
            mask = local_rv$mask,
            base_dir = dirname(local_rv$manifest_path)
          )
      }, error = function(e) {
        local_rv$manifest_error <- e$message
        NULL
      })

      if (is.null(local_rv$manifest_spec)) return()

      spec <- local_rv$manifest_spec
      local_rv$data_loaded <- TRUE

      # Sync UI with manifest
      if (!is.null(spec$num_cond)) {
        updateNumericInput(session, "num_conditions", value = as.integer(spec$num_cond))
      }

      # Update group summary (count unique subjects per group)
      subj_counts <- vapply(spec$datamat_lst, function(df) length(unique(df$subject)), integer(1))
      group_names <- if (!is.null(spec$groups)) as.character(spec$groups) else paste0("Group ", seq_along(subj_counts))
      local_rv$groups <- lapply(seq_along(subj_counts), function(i) {
        list(name = group_names[[i]], n_subj = subj_counts[[i]])
      })
    },
    ignoreInit = TRUE
  )

  output$manifest_info <- renderUI({
    if (!identical(as.character(input$data_source)[1], "manifest")) return(NULL)

    if (!is.null(local_rv$manifest_error) && nzchar(local_rv$manifest_error)) {
      return(div(class = "text-danger small mt-2", icon("exclamation-triangle"), local_rv$manifest_error))
    }

    spec <- local_rv$manifest_spec
    if (is.null(spec)) return(NULL)

    n_groups <- length(spec$datamat_lst)
    n_obs <- sum(vapply(spec$datamat_lst, nrow, integer(1)))
    n_cond <- if (!is.null(spec$num_cond)) as.integer(spec$num_cond) else NA_integer_
    n_lags <- NA_integer_
    if (is.list(spec$feature_layout) && !is.null(spec$feature_layout$n_lags)) {
      n_lags <- as.integer(spec$feature_layout$n_lags)
    }

    msg <- sprintf("%d groups, %d conditions, %d observations", n_groups, n_cond, n_obs)
    if (!is.na(n_lags)) msg <- paste0(msg, sprintf(" \u2022 %d lags", n_lags))

    div(class = "small text-muted mt-2", icon("check-circle", class = "text-success"), paste0(" ", msg))
  })
}

