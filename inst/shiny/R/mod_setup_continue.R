# Setup module "Continue" handler (spec building)
# Extracted from mod_setup.R to keep setup_server readable and maintainable.

setup_register_continue_handler <- function(input,
                                            output,
                                            session,
                                            local_rv,
                                            continue_trigger,
                                            validate_setup,
                                            seed_atlas) {
  observeEvent(input$btn_continue, {

    pls_options <- collect_pls_options_from_setup(input, local_rv)

    # =========================================================================
    # Attach mode: build spec from pipeline outputs via public API
    # =========================================================================

    if (identical(local_rv$analysis_source, "attach")) {
      pa <- local_rv$prepared_analysis
      if (is.null(pa)) {
        showNotification("Pipeline outputs are not loaded or invalid.", type = "error")
        return()
      }

      spec <- tryCatch(
        plsrri::pipeline_build_pls_spec_from_ui(pa$analysis_plan, pls_options),
        error = function(e) {
          showNotification(paste("Error building spec:", e$message), type = "error")
          NULL
        }
      )
      if (is.null(spec)) return()

      local_rv$spec <- spec
      local_rv$prepared_analysis <- build_prepared_analysis_from_attach(
        root = pa$pipeline_root,
        attach_info = list(
          summary = pa$summary,
          firstlevel_plan = pa$firstlevel_plan,
          firstlevel_manifest = pa$firstlevel_manifest
        ),
        plan = pa$analysis_plan,
        spec = spec,
        pls_options = pls_options
      )
      continue_trigger(continue_trigger() + 1)
      return()
    }

    # =========================================================================
    # Direct BIDS workstation pipeline mode
    # =========================================================================

    if (identical(as.character(input$data_source)[1], "bids") && isTRUE(input$bids_use_pipeline)) {
      pipeline_spec <- tryCatch(
        build_bids_pipeline_spec_from_setup(
          bids_path = local_rv$bids_path,
          task = as.character(input$bids_task)[1],
          space = as.character(input$bids_space)[1],
          participants_df = local_rv$bids_part_df,
          group_col = as.character(input$bids_group_col)[1],
          group_values = as.character(input$bids_group_values),
          design_formula = as.character(input$bids_design_formula)[1],
          output_root = local_rv$bids_output_root,
          output_type = as.character(input$bids_pipeline_output_type)[1],
          output_statistic = as.character(input$bids_pipeline_statistic)[1],
          basis_pattern = as.character(input$bids_basis_pattern)[1],
          basis_order = .parse_csv_chr(input$bids_basis_order),
          method = as.character(input$method)[1],
          nperm = as.integer(input$num_perm),
          nboot = as.integer(input$num_boot),
          nsplit = as.integer(input$num_split),
          clim = as.numeric(input$confidence),
          meancentering = as.integer(input$meancentering),
          cormode = if (identical(as.character(input$method)[1], "behavior") ||
            identical(as.character(input$method)[1], "multiblock")) as.integer(input$cormode) else NULL,
          boot_type = input$boot_type,
          analyze_mode = as.character(input$bids_analyze_mode)[1]
        ),
        error = function(e) {
          showNotification(paste("Error building pipeline spec:", e$message), type = "error")
          NULL
        }
      )
      if (is.null(pipeline_spec)) return()

      local_rv$spec <- NULL
      local_rv$prepared_analysis <- build_prepared_analysis_from_bids_pipeline(
        pipeline_spec = pipeline_spec,
        pls_options = pls_options,
        summary = if (!is.null(local_rv$bids_spec)) summarize_pls_spec_for_review(local_rv$bids_spec) else NULL,
        analyze_mode = as.character(input$bids_analyze_mode)[1]
      )
      continue_trigger(continue_trigger() + 1)
      return()
    }

    # =========================================================================
    # Direct mode: existing spec-building logic
    # =========================================================================

    errors <- validate_setup()

    if (length(errors) > 0) {
      showNotification("Please fix validation errors", type = "error")
      return()
    }

    # Build spec
    groups <- local_rv$groups
    n_cond <- suppressWarnings(as.integer(input$num_conditions)[1])
    if (is.na(n_cond)) n_cond <- 0L

    # Determine method integer (delegate to pure function)
    method_int <- map_method_to_int(input$method)

    data_source <- as.character(input$data_source)[1]
    spec <- NULL

    if (identical(data_source, "manifest")) {
      spec <- local_rv$manifest_spec
      if (is.null(spec)) {
        showNotification("Manifest inputs are not ready (check manifest + mask).", type = "error")
        return()
      }
    } else if (identical(data_source, "bids")) {
      spec <- local_rv$bids_spec
      if (is.null(spec)) {
        showNotification("BIDS inputs are not ready. Click 'Build from BIDS' first.", type = "error")
        return()
      }
    } else {
      # Default: manual matrices or future sources (BIDS/load)
      spec <- plsrri::pls_spec()

      if (!is.null(local_rv$data_matrices)) {
        spec$datamat_lst <- local_rv$data_matrices
      }

      spec$num_subj_lst <- vapply(groups, function(g) suppressWarnings(as.integer(g$n_subj)[1]), integer(1))
      spec$num_cond <- as.integer(n_cond)
      spec$groups <- sapply(groups, function(g) g$name)
      spec$mask <- local_rv$mask
    }
    spec$method <- method_int
    spec$num_perm <- as.integer(input$num_perm)
    spec$num_boot <- as.integer(input$num_boot)
    spec$clim <- as.integer(input$confidence)
    spec$boot_type <- input$boot_type
    spec$num_split <- as.integer(input$num_split)
    spec$meancentering_type <- as.integer(input$meancentering)
    if (!identical(data_source, "manifest")) {
      spec$groups <- sapply(groups, function(g) g$name)
      spec$mask <- local_rv$mask
    }

    # Attach behavior/seed data as needed
    if (identical(input$method, "behavior") || identical(input$method, "multiblock")) {
      spec$stacked_behavdata <- local_rv$behav_data
      spec$cormode <- as.integer(input$cormode)
    } else if (identical(input$method, "seed")) {
      seed_source <- as.character(input$seed_source)[1]
      seed_lags_txt <- as.character(input$seed_lags)[1]
      seed_lags <- NULL
      if (!is.na(seed_lags_txt) && nzchar(trimws(seed_lags_txt))) {
        seed_lags <- tryCatch(plsrri:::.parse_int_spec(seed_lags_txt), error = function(e) {
          showNotification(paste("Invalid seed lags:", e$message), type = "error")
          NULL
        })
        if (is.null(seed_lags)) return()
      }

      if (identical(seed_source, "atlas")) {
        atlas <- seed_atlas()
        if (is.null(atlas)) {
          showNotification("Seed atlas could not be loaded", type = "error")
          return()
        }

        meta <- tryCatch(neuroatlas::roi_metadata(atlas), error = function(e) NULL)
        if (is.null(meta) || nrow(meta) == 0) {
          showNotification("Could not read ROI metadata for selected atlas", type = "error")
          return()
        }

        label_col <- if ("label_full" %in% names(meta)) "label_full" else "label"
        roi_labels <- as.character(input$seed_rois)
        roi_ids <- meta$id[match(roi_labels, meta[[label_col]])]

        spec$cormode <- as.integer(input$cormode_seed)
        spec$seed_info <- list(
          seed_source = "atlas",
          atlas = input$seed_atlas,
          schaefer_parcels = input$schaefer_parcels,
          schaefer_networks = input$schaefer_networks,
          schaefer_resolution = input$schaefer_resolution,
          roi_labels = roi_labels,
          roi_ids = roi_ids,
          atlas_vol = atlas$atlas,
          lags = seed_lags,
          summary = "mean"
        )

        if (identical(data_source, "manual")) {
          seed_data <- tryCatch({
            plsrri:::pls_seed_data_from_labels(
              datamat_lst = local_rv$data_matrices,
              mask = local_rv$mask,
              labels = atlas$atlas,
              roi_ids = roi_ids,
              roi_labels = roi_labels,
              lags = seed_lags,
              na_rm = TRUE
            )
          }, error = function(e) {
            showNotification(paste("Error computing seed data:", e$message), type = "error")
            NULL
          })

          if (is.null(seed_data)) return()
          spec$stacked_behavdata <- seed_data
        }
      } else if (identical(seed_source, "mask")) {
        if (is.null(local_rv$seed_mask)) {
          showNotification("Seed mask not loaded", type = "error")
          return()
        }

        seed_name <- as.character(input$seed_mask_name)[1]
        if (is.na(seed_name) || !nzchar(seed_name)) seed_name <- "seed"

        spec$cormode <- as.integer(input$cormode_seed)
        spec$seed_info <- list(seed_source = "mask", seed_name = seed_name, seed_mask = local_rv$seed_mask, lags = seed_lags, summary = "mean")

        if (identical(data_source, "manual")) {
          seed_data <- tryCatch({
            plsrri:::pls_seed_data_from_labels(
              datamat_lst = local_rv$data_matrices,
              mask = local_rv$mask,
              labels = as.array(local_rv$seed_mask) > 0,
              roi_ids = 1L,
              roi_labels = seed_name,
              lags = seed_lags,
              na_rm = TRUE
            )
          }, error = function(e) {
            showNotification(paste("Error computing seed data:", e$message), type = "error")
            NULL
          })

          if (is.null(seed_data)) return()
          spec$stacked_behavdata <- seed_data
        }
      } else if (identical(seed_source, "custom")) {
        if (is.null(local_rv$seed_custom_atlas)) {
          showNotification("Seed atlas not loaded", type = "error")
          return()
        }

        use_all <- isTRUE(input$seed_use_all_labels)
        roi_ids <- if (use_all) local_rv$seed_custom_ids else suppressWarnings(as.integer(input$seed_label_ids))
        roi_ids <- roi_ids[!is.na(roi_ids)]

        if (length(roi_ids) == 0) {
          showNotification("No seed labels selected", type = "error")
          return()
        }

        roi_labels <- paste0("seed_", roi_ids)

        spec$cormode <- as.integer(input$cormode_seed)
        spec$seed_info <- list(seed_source = "custom_atlas", seed_atlas = local_rv$seed_custom_atlas, roi_ids = roi_ids, roi_labels = roi_labels, lags = seed_lags, summary = "mean")

        if (identical(data_source, "manual")) {
          seed_data <- tryCatch({
            plsrri:::pls_seed_data_from_labels(
              datamat_lst = local_rv$data_matrices,
              mask = local_rv$mask,
              labels = local_rv$seed_custom_atlas,
              roi_ids = roi_ids,
              roi_labels = roi_labels,
              lags = seed_lags,
              na_rm = TRUE
            )
          }, error = function(e) {
            showNotification(paste("Error computing seed data:", e$message), type = "error")
            NULL
          })

          if (is.null(seed_data)) return()
          spec$stacked_behavdata <- seed_data
        }
      } else {
        showNotification("Select a seed definition type", type = "error")
        return()
      }
    }

    local_rv$spec <- spec
    local_rv$prepared_analysis <- build_prepared_analysis_from_spec(spec, source = "direct")

    # Trigger continue
    continue_trigger(continue_trigger() + 1)
  })
}
