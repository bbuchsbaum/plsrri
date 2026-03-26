# Setup module validation
# Extracted from mod_setup.R to keep setup_server readable and maintainable.

setup_register_validation_handlers <- function(input, output, session, local_rv, seed_atlas) {
  validate_setup <- reactive({
    analysis_source <- as.character(input$analysis_source)[1]
    method <- as.character(input$method)[1]

    if (identical(analysis_source, "attach")) {
      errors <- character(0)
      pa <- local_rv$prepared_analysis
      if (!is_valid_prepared_analysis(pa)) {
        errors <- c(errors, "Attach a valid first-level output root")
      }
      if (identical(method, "behavior") || identical(method, "multiblock")) {
        if (!isTRUE(local_rv$behav_loaded) || is.null(local_rv$behav_data)) {
          errors <- c(errors, "Behavior data is required for Behavior/Multiblock PLS")
        }
      }
      return(errors)
    }

    # Delegate to pure validation function
    errors <- validate_setup_config(
      data_source = input$data_source,
      data_loaded = local_rv$data_loaded,
      bids_dir = if (identical(as.character(input$data_source)[1], "bids") && !is.null(local_rv$bids_path)) local_rv$bids_path else input$bids_dir,
      manifest_path = local_rv$manifest_path,
      mask_loaded = !is.null(local_rv$mask),
      groups = local_rv$groups,
      num_conditions = input$num_conditions,
      num_boot = input$num_boot
    )

    n_cond <- suppressWarnings(as.integer(input$num_conditions)[1])
    if (is.na(n_cond) || n_cond < 1) n_cond <- 0L
    total_subj <- sum(vapply(local_rv$groups, function(g) {
      x <- suppressWarnings(as.integer(g$n_subj)[1])
      if (is.na(x) || x < 0) 0L else x
    }, integer(1)))
    expected_rows <- total_subj * n_cond

    if (identical(method, "behavior") || identical(method, "multiblock")) {
      if (!isTRUE(local_rv$behav_loaded) || is.null(local_rv$behav_data)) {
        errors <- c(errors, "Behavior data is required for Behavior/Multiblock PLS")
      } else if (is.finite(expected_rows) && expected_rows > 0 && nrow(local_rv$behav_data) != expected_rows) {
        errors <- c(errors, sprintf(
          "Behavior matrix has %d rows; expected %d (subjects \u00d7 conditions)",
          nrow(local_rv$behav_data), expected_rows
        ))
      }
    }

    if (identical(as.character(input$data_source)[1], "bids") && isTRUE(input$bids_use_pipeline)) {
      if (is.null(local_rv$bids_spec) && is.null(local_rv$pipeline_spec_loaded)) {
        errors <- c(errors, "Click 'Build from BIDS' to validate the dataset preview")
      }

      design_formula <- as.character(input$bids_design_formula)[1]
      if (is.na(design_formula) || !nzchar(trimws(design_formula))) {
        errors <- c(errors, "First-level design formula is required for workstation pipeline mode")
      }

      if (identical(method, "seed")) {
        errors <- c(errors, "Seed PLS is not yet supported with in-app first-level pipeline mode")
      }

      basis_formula <- grepl("basis\\s*=\\s*['\"](fir|tent)['\"]", design_formula %||% "", perl = TRUE)
      basis_pattern <- as.character(input$bids_basis_pattern)[1]
      basis_order <- as.character(input$bids_basis_order)[1]
      if (isTRUE(basis_formula)) {
        if (is.na(basis_pattern) || !nzchar(trimws(basis_pattern))) {
          errors <- c(errors, "Basis-expanded designs require a basis label pattern")
        }
        if (is.na(basis_order) || !nzchar(trimws(basis_order))) {
          errors <- c(errors, "Basis-expanded designs require a basis order")
        }
      }
    }

    if (identical(method, "seed")) {
      if (!requireNamespace("neuroim2", quietly = TRUE)) {
        errors <- c(errors, "Seed PLS requires the 'neuroim2' package for mask handling")
      }

      if (is.null(local_rv$mask)) {
        errors <- c(errors, "Seed PLS requires a brain mask (NIfTI)")
      }

      seed_source <- as.character(input$seed_source)[1]
      use_all <- isTRUE(input$seed_use_all_labels)

      if (identical(seed_source, "atlas")) {
        if (!requireNamespace("neuroatlas", quietly = TRUE)) {
          errors <- c(errors, "Atlas-based seed selection requires the 'neuroatlas' package")
        }
        if (!shiny::isTruthy(input$seed_rois)) {
          errors <- c(errors, "Select at least one seed ROI")
        }
      } else if (identical(seed_source, "mask")) {
        if (is.null(local_rv$seed_mask)) {
          errors <- c(errors, "Upload a seed mask (0/1 NIfTI)")
        } else if (!is.null(local_rv$mask)) {
          seed_dim <- dim(as.array(local_rv$seed_mask))[1:3]
          mask_dim <- dim(as.array(local_rv$mask))[1:3]
          if (!identical(seed_dim, mask_dim)) {
            errors <- c(errors, "Seed mask dimensions do not match the brain mask")
          }
        }
      } else if (identical(seed_source, "custom")) {
        if (is.null(local_rv$seed_custom_atlas)) {
          errors <- c(errors, "Upload a seed atlas (integer labels NIfTI)")
        } else if (!is.null(local_rv$mask)) {
          atlas_dim <- dim(as.array(local_rv$seed_custom_atlas))[1:3]
          mask_dim <- dim(as.array(local_rv$mask))[1:3]
          if (!identical(atlas_dim, mask_dim)) {
            errors <- c(errors, "Seed atlas dimensions do not match the brain mask")
          }
        }

        if (use_all) {
          if (is.null(local_rv$seed_custom_ids) || length(local_rv$seed_custom_ids) == 0) {
            errors <- c(errors, "Seed atlas has no non-zero labels")
          }
        } else {
          if (!shiny::isTruthy(input$seed_label_ids)) {
            errors <- c(errors, "Select at least one seed label id (or use all labels)")
          }
        }
      } else {
        errors <- c(errors, "Select a seed definition type")
      }

      if (identical(as.character(input$data_source)[1], "manual") &&
        isTRUE(local_rv$data_loaded) && !is.null(local_rv$data_matrices) && !is.null(local_rv$mask)) {
        mask_arr <- as.array(local_rv$mask)
        n_vox <- sum(mask_arr > 0)
        n_cols <- vapply(local_rv$data_matrices, ncol, integer(1))
        ok <- n_cols == n_vox | (n_vox > 0 && (n_cols %% n_vox) == 0L)
        if (!all(ok)) {
          errors <- c(errors, "Mask voxel count does not match the number of columns in the data matrix")
        }
      }
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

  validate_setup
}
