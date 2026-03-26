# Setup module BIDS data source handlers

setup_register_bids_data_source <- function(input, output, session, local_rv, roots) {
  output$bids_path_display <- renderUI({
    dir_info <- input$bids_dir
    if (setup_shinyfiles_is_unselected(dir_info)) {
      if (!is.null(local_rv$bids_path) && nzchar(local_rv$bids_path)) {
        return(
          tagList(
            p(class = "small mb-1", icon("folder"), local_rv$bids_path),
            p(class = "text-muted small mb-0", icon("file-import"), "Loaded from pipeline YAML")
          )
        )
      }
      return(p(class = "text-muted small", "No directory selected"))
    }

    path <- shinyFiles::parseDirPath(roots, dir_info)
    if (length(path) > 0) {
      path <- path[1]
      looks_like_bids <- file.exists(file.path(path, "dataset_description.json"))

      tagList(
        p(class = "small mb-1", icon("folder"), path),
        if (!looks_like_bids) {
          p(
            class = "text-warning small mb-0",
            icon("exclamation-triangle"),
            "This folder doesn't look like a BIDS dataset root (missing dataset_description.json)."
          )
        }
      )
    }
  })

  output$bids_output_root_display <- renderUI({
    dir_info <- input$bids_output_root_dir
    if (setup_shinyfiles_is_unselected(dir_info)) {
      if (!is.null(local_rv$bids_output_root) && nzchar(local_rv$bids_output_root)) {
        return(
          tagList(
            p(class = "small mt-2 mb-0", icon("folder-open"), local_rv$bids_output_root),
            p(class = "text-muted small mb-0", icon("file-import"), "Loaded from pipeline YAML")
          )
        )
      }
      return(p(class = "text-muted small mt-2 mb-0", "Using a temporary output root if none is chosen"))
    }

    path <- shinyFiles::parseDirPath(roots, dir_info)
    if (length(path) == 0) return(NULL)
    p(class = "small mt-2 mb-0", icon("folder-open"), path[1])
  })

  # Track BIDS path and load participants.tsv (for grouping UI)
  observeEvent(input$bids_dir, {
    dir_info <- input$bids_dir
    if (setup_shinyfiles_is_unselected(dir_info)) {
      local_rv$bids_path <- NULL
      local_rv$bids_part_df <- NULL
      local_rv$bids_spec <- NULL
      local_rv$pipeline_spec_loaded <- NULL
      local_rv$pipeline_yaml_source <- NULL
      local_rv$pipeline_yaml_loaded_at <- NULL
      local_rv$bids_error <- NULL
      updateSelectInput(session, "bids_group_col", choices = c("all"), selected = "all")
      updateSelectizeInput(session, "bids_group_values", choices = character(0), selected = character(0), server = TRUE)
      return()
    }

    path <- shinyFiles::parseDirPath(roots, dir_info)
    if (length(path) == 0) return()
    path <- path[1]
    local_rv$bids_path <- path
    local_rv$bids_spec <- NULL
    local_rv$pipeline_spec_loaded <- NULL
    local_rv$pipeline_yaml_source <- NULL
    local_rv$pipeline_yaml_loaded_at <- NULL
    local_rv$bids_error <- NULL

    part_file <- file.path(path, "participants.tsv")
    if (!file.exists(part_file)) {
      local_rv$bids_part_df <- NULL
      updateSelectInput(session, "bids_group_col", choices = c("all"), selected = "all")
      return()
    }

    part_df <- tryCatch(utils::read.delim(part_file, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(part_df) || !"participant_id" %in% names(part_df)) {
      local_rv$bids_part_df <- NULL
      updateSelectInput(session, "bids_group_col", choices = c("all"), selected = "all")
      return()
    }

    local_rv$bids_part_df <- part_df
    group_cols <- setdiff(names(part_df), "participant_id")
    choices <- c("all", group_cols)
    updateSelectInput(session, "bids_group_col", choices = choices, selected = "all")
  }, ignoreInit = TRUE)

  observeEvent(input$bids_output_root_dir, {
    dir_info <- input$bids_output_root_dir
    if (setup_shinyfiles_is_unselected(dir_info)) {
      local_rv$bids_output_root <- NULL
      return()
    }
    path <- shinyFiles::parseDirPath(roots, dir_info)
    if (length(path) == 0) return()
    local_rv$bids_output_root <- as.character(path[1])
  }, ignoreInit = TRUE)

  observeEvent(input$bids_group_col, {
    col <- as.character(input$bids_group_col)[1]
    if (is.na(col) || !nzchar(col) || identical(col, "all")) {
      updateSelectizeInput(session, "bids_group_values", choices = character(0), selected = character(0), server = TRUE)
      return()
    }
    df <- local_rv$bids_part_df
    if (is.null(df) || !col %in% names(df)) return()
    vals <- sort(unique(as.character(df[[col]])))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    selected <- intersect(isolate(input$bids_group_values), vals)
    updateSelectizeInput(session, "bids_group_values", choices = vals, selected = selected, server = TRUE)
  }, ignoreInit = TRUE)

  # Optional BIDS mask upload (overrides derived mask)
  observeEvent(input$bids_mask_file, {
    req(input$bids_mask_file)

    if (!requireNamespace("neuroim2", quietly = TRUE)) {
      showNotification("Package 'neuroim2' is required to read NIfTI masks", type = "error")
      return()
    }

    tryCatch({
      mask <- neuroim2::read_vol(input$bids_mask_file$datapath[1])
      local_rv$mask <- mask
    }, error = function(e) {
      showNotification(paste("Error reading BIDS mask:", e$message), type = "error")
    })
  }, ignoreInit = TRUE)

  # Build BIDS-derived manifest spec (header validation only; data ingestion happens at run)
  observeEvent(input$btn_build_bids, {
    path <- local_rv$bids_path
    if (is.null(path) || !nzchar(path)) {
      showNotification("Select a BIDS directory first", type = "error")
      return()
    }

    local_rv$bids_error <- NULL
    local_rv$bids_spec <- NULL

    task <- as.character(input$bids_task)[1]
    if (is.na(task) || !nzchar(trimws(task))) task <- NULL

    space <- as.character(input$bids_space)[1]
    if (is.na(space) || !nzchar(trimws(space))) space <- NULL

    file_regex <- as.character(input$bids_file_regex)[1]
    if (is.na(file_regex) || !nzchar(trimws(file_regex))) file_regex <- ".*\\.nii(\\.gz)?$"

    cond_keys_txt <- as.character(input$bids_condition_keys)[1]
    cond_keys_txt <- if (is.na(cond_keys_txt)) "" else cond_keys_txt
    condition_keys <- trimws(strsplit(cond_keys_txt, ",", fixed = TRUE)[[1]])
    condition_keys <- condition_keys[nzchar(condition_keys)]
    if (length(condition_keys) == 0) condition_keys <- "cond"

    volumes <- as.character(input$bids_volumes)[1]
    if (is.na(volumes) || !nzchar(trimws(volumes))) volumes <- NULL

    group_col <- as.character(input$bids_group_col)[1]
    if (is.na(group_col) || !nzchar(group_col) || identical(group_col, "all")) {
      group_col <- NULL
    }

    groups <- NULL
    if (!is.null(group_col)) {
      selected_vals <- as.character(input$bids_group_values)
      selected_vals <- selected_vals[!is.na(selected_vals) & nzchar(selected_vals)]
      if (length(selected_vals) == 0) {
        df <- local_rv$bids_part_df
        if (!is.null(df) && group_col %in% names(df)) {
          selected_vals <- sort(unique(as.character(df[[group_col]])))
          selected_vals <- selected_vals[!is.na(selected_vals) & nzchar(selected_vals)]
        }
      }
      if (length(selected_vals) > 0) groups <- selected_vals
    }

    spec0 <- plsrri::pls_spec(mask = local_rv$mask)
    local_rv$bids_spec <- tryCatch({
      plsrri::add_subjects_bids_manifest(
        spec = spec0,
        bids_dir = path,
        groups = groups,
        group_col = group_col,
        task = task,
        space = space,
        file_regex = file_regex,
        condition_keys = condition_keys,
        volumes = volumes,
        mask_method = as.character(input$bids_mask_method)[1],
        strict = TRUE
      )
    }, error = function(e) {
      local_rv$bids_error <- e$message
      NULL
    })

    if (is.null(local_rv$bids_spec)) return()

    spec <- local_rv$bids_spec
    local_rv$mask <- spec$mask

    # Sync UI with derived spec
    if (!is.null(spec$num_cond)) {
      updateNumericInput(session, "num_conditions", value = as.integer(spec$num_cond))
    }

    subj_counts <- vapply(spec$datamat_lst, function(df) length(unique(df$subject)), integer(1))
    group_names <- if (!is.null(spec$groups)) as.character(spec$groups) else paste0("Group ", seq_along(subj_counts))
    local_rv$groups <- lapply(seq_along(subj_counts), function(i) {
      list(name = group_names[[i]], n_subj = subj_counts[[i]])
    })
  })

  output$bids_info <- renderUI({
    if (!identical(as.character(input$data_source)[1], "bids")) return(NULL)

    if (!is.null(local_rv$bids_error) && nzchar(local_rv$bids_error)) {
      return(div(class = "text-danger small mt-2", icon("exclamation-triangle"), local_rv$bids_error))
    }

    if (is.null(local_rv$bids_spec) && !is.null(local_rv$pipeline_spec_loaded)) {
      spec <- local_rv$pipeline_spec_loaded
      task <- plsrri:::.pipeline_as_chr(spec$dataset$task %||% character(0))
      task <- if (length(task)) task[[1]] else "--"
      mode <- as.character(plsrri:::.pipeline_nested(spec, c("ui", "analyze_mode"), "end_to_end"))[1]
      source_label <- local_rv$pipeline_yaml_source
      if (is.null(source_label) || is.na(source_label) || !nzchar(as.character(source_label)[1])) {
        source_label <- "Recent snapshot"
      }

      return(
        div(
          class = "small text-muted mt-2",
          div(icon("file-import", class = "text-info"), " Imported pipeline YAML"),
          div(class = "mt-1", strong("Task: "), task, " • ", strong("Mode: "), mode),
          div(class = "mt-1 text-break", strong("Source: "), source_label),
          if (!is.null(local_rv$pipeline_yaml_loaded_at)) {
            div(class = "mt-1", strong("Loaded: "), local_rv$pipeline_yaml_loaded_at)
          }
        )
      )
    }

    spec <- local_rv$bids_spec
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
