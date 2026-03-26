# Setup module saved spec data source handlers

setup_register_load_spec_data_source <- function(input, output, session, local_rv) {
  observeEvent(input$spec_file, {
    req(input$spec_file)

    tryCatch({
      spec <- readRDS(input$spec_file$datapath)

      if (inherits(spec, "pls_spec")) {
        local_rv$spec <- spec
        local_rv$data_loaded <- TRUE

        # Mask / behavior from spec (if present)
        if (!is.null(spec$mask)) {
          local_rv$mask <- spec$mask
        }
        if (!is.null(spec$stacked_behavdata)) {
          local_rv$behav_data <- spec$stacked_behavdata
          local_rv$behav_loaded <- TRUE
        }

        # Populate UI from spec
        if (length(spec$datamat_lst) > 0) {
          local_rv$data_matrices <- spec$datamat_lst
          updateNumericInput(session, "num_conditions", value = spec$num_cond)

          local_rv$groups <- lapply(seq_along(spec$num_subj_lst), function(i) {
            grp <- NA_character_
            if (!is.null(spec$groups) && length(spec$groups) >= i) {
              grp <- as.character(spec$groups[i])
            }
            if (is.na(grp) || !nzchar(grp)) grp <- paste("Group", i)

            list(
              name = grp,
              n_subj = spec$num_subj_lst[i]
            )
          })
        }

        # Update method — map integer to closest UI radio button value
        method_name <- pls_method_int_to_name(spec$method)
        method <- switch(
          method_name,
          task = "task",
          task_nonrotated = "task",
          behavior = "behavior",
          behavior_nonrotated = "behavior",
          multiblock = "multiblock",
          multiblock_nonrotated = "multiblock",
          "task"
        )
        updateRadioButtons(session, "method", selected = method)

        # Update resampling
        updateNumericInput(session, "num_perm", value = spec$num_perm)
        updateNumericInput(session, "num_boot", value = spec$num_boot)

        if (!is.null(spec$cormode)) {
          updateSelectInput(session, "cormode", selected = as.character(spec$cormode))
          updateSelectInput(session, "cormode_seed", selected = as.character(spec$cormode))
        }

        showNotification("Specification loaded successfully", type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error loading spec:", e$message), type = "error")
    })
  })
}

