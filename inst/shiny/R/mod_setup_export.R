# Setup export helpers
# YAML export + CLI command preview from the current prepared-analysis state.

setup_recent_pipeline_dir <- function() {
  override <- getOption("plsrri.shiny.recent_dir", NULL)
  if (!is.null(override) && nzchar(as.character(override)[1])) {
    return(normalizePath(as.character(override)[1], winslash = "/", mustWork = FALSE))
  }
  normalizePath(
    file.path(tools::R_user_dir("plsrri", which = "config"), "shiny-recent"),
    winslash = "/",
    mustWork = FALSE
  )
}

setup_recent_pipeline_index_path <- function() {
  file.path(setup_recent_pipeline_dir(), "recent-configs.tsv")
}

setup_read_recent_pipeline_index <- function() {
  path <- setup_recent_pipeline_index_path()
  if (!file.exists(path)) {
    return(data.frame(
      key = character(0),
      label = character(0),
      file = character(0),
      source = character(0),
      saved_at = character(0),
      stringsAsFactors = FALSE
    ))
  }
  utils::read.delim(path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
}

setup_write_recent_pipeline_index <- function(index) {
  dir.create(setup_recent_pipeline_dir(), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    index,
    file = setup_recent_pipeline_index_path(),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )
  invisible(index)
}

setup_recent_pipeline_label <- function(spec, source = "snapshot") {
  task <- plsrri:::.pipeline_as_chr(spec$dataset$task %||% character(0))
  task <- if (length(task)) task[[1]] else "task"
  mode <- as.character(plsrri:::.pipeline_nested(spec, c("ui", "analyze_mode"), "end_to_end"))[1]
  method <- as.character(spec$pls$method %||% "task")[1]
  sprintf("%s | %s | %s | %s", format(Sys.time(), "%Y-%m-%d %H:%M"), task, method, source)
}

setup_record_recent_pipeline_spec <- function(spec, source = "snapshot", max_n = 8L) {
  dir <- setup_recent_pipeline_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  key <- paste0("cfg-", timestamp, "-", substr(sprintf("%08x", sample.int(2^31 - 1L, 1L)), 1, 6))
  file <- file.path(dir, paste0(key, ".yml"))
  writeLines(pipeline_spec_yaml_text(spec), con = file, useBytes = TRUE)

  index <- setup_read_recent_pipeline_index()
  entry <- data.frame(
    key = key,
    label = setup_recent_pipeline_label(spec, source = source),
    file = normalizePath(file, winslash = "/", mustWork = FALSE),
    source = as.character(source)[1],
    saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  index <- rbind(entry, index)
  index <- index[!duplicated(index$key), , drop = FALSE]
  if (nrow(index) > max_n) {
    drop_files <- index$file[(max_n + 1L):nrow(index)]
    unlink(drop_files[file.exists(drop_files)])
    index <- index[seq_len(max_n), , drop = FALSE]
  }
  setup_write_recent_pipeline_index(index)
  entry
}

setup_recent_pipeline_choices <- function(index = setup_read_recent_pipeline_index()) {
  if (!nrow(index)) return(character(0))
  stats::setNames(index$key, index$label)
}

setup_load_recent_pipeline_spec <- function(key) {
  index <- setup_read_recent_pipeline_index()
  row <- index[index$key %in% as.character(key)[1], , drop = FALSE]
  if (!nrow(row)) {
    stop("Recent pipeline config not found", call. = FALSE)
  }
  path <- row$file[[1]]
  if (!file.exists(path)) {
    stop("Recent pipeline config file does not exist: ", path, call. = FALSE)
  }
  plsrri::read_pipeline_spec(path)
}

setup_export_yaml_content <- function(prepared, file) {
  validate_prepared_analysis(prepared)
  spec <- prepared_analysis_pipeline_spec(prepared)
  writeLines(pipeline_spec_yaml_text(spec), con = file, useBytes = TRUE)
  setup_record_recent_pipeline_spec(spec, source = "export")
  invisible(file)
}

setup_cli_modal_body <- function(prepared, spec_path = "plsrri-analysis.yml") {
  commands <- prepared_analysis_cli_commands(prepared, spec_path = spec_path)
  paste(
    c(
      "Local run:",
      commands$local,
      "",
      "Staged / HPC-friendly commands:",
      commands$staged
    ),
    collapse = "\n"
  )
}

setup_read_pipeline_yaml_spec <- function(file_info, roots) {
  if (setup_shinyfiles_is_unselected(file_info)) {
    return(NULL)
  }

  path <- shinyFiles::parseFilePaths(roots, file_info)$datapath
  if (!length(path)) {
    return(NULL)
  }

  plsrri::read_pipeline_spec(path[[1]])
}

setup_apply_pipeline_yaml <- function(session, local_rv, spec) {
  vals <- pipeline_spec_to_setup_values(spec)
  source_path <- spec$.spec_path %||% NULL
  if (!is.null(source_path) && (!nzchar(as.character(source_path)[1]) || is.na(source_path))) {
    source_path <- NULL
  }
  part_df <- NULL
  part_file <- file.path(vals$bids_path, "participants.tsv")
  if (file.exists(part_file)) {
    part_df <- tryCatch(utils::read.delim(part_file, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(part_df) && !"participant_id" %in% names(part_df)) {
      part_df <- NULL
    }
  }

  local_rv$analysis_source <- vals$analysis_source
  local_rv$bids_path <- vals$bids_path
  local_rv$bids_output_root <- vals$bids_output_root
  local_rv$bids_part_df <- part_df
  local_rv$pipeline_spec_loaded <- vals$pipeline_spec
  local_rv$spec <- NULL
  local_rv$prepared_analysis <- build_prepared_analysis_from_bids_pipeline(
    pipeline_spec = vals$pipeline_spec,
    pls_options = list(
      method = vals$method,
      nperm = vals$num_perm,
      nboot = vals$num_boot,
      nsplit = vals$num_split,
      clim = vals$confidence,
      meancentering = suppressWarnings(as.integer(vals$meancentering)),
      cormode = suppressWarnings(as.integer(vals$cormode)),
      boot_type = vals$boot_type
    ),
    summary = NULL,
    analyze_mode = vals$bids_analyze_mode
  )

  group_choices <- c("all")
  if (!is.null(part_df)) {
    group_choices <- c("all", setdiff(names(part_df), "participant_id"))
  }

  updateRadioButtons(session, "analysis_source", selected = vals$analysis_source)
  updateRadioButtons(session, "data_source", selected = vals$data_source)
  updateCheckboxInput(session, "bids_use_pipeline", value = isTRUE(vals$bids_use_pipeline))
  updateTextInput(session, "bids_task", value = vals$bids_task)
  updateTextInput(session, "bids_space", value = vals$bids_space)
  updateSelectInput(session, "bids_group_col", choices = group_choices, selected = vals$bids_group_col)
  updateSelectizeInput(session, "bids_group_values", selected = vals$bids_group_values, server = TRUE)
  updateTextInput(session, "bids_design_formula", value = vals$bids_design_formula)
  updateSelectInput(session, "bids_pipeline_output_type", selected = vals$bids_pipeline_output_type)
  updateTextInput(session, "bids_pipeline_statistic", value = vals$bids_pipeline_statistic)
  updateTextInput(session, "bids_basis_pattern", value = vals$bids_basis_pattern)
  updateTextInput(session, "bids_basis_order", value = vals$bids_basis_order)
  updateRadioButtons(session, "bids_analyze_mode", selected = vals$bids_analyze_mode)
  updateRadioButtons(session, "method", selected = vals$method)
  updateNumericInput(session, "num_perm", value = vals$num_perm)
  updateNumericInput(session, "num_boot", value = vals$num_boot)
  updateNumericInput(session, "num_split", value = vals$num_split)
  updateNumericInput(session, "confidence", value = vals$confidence)
  updateSelectInput(session, "meancentering", selected = vals$meancentering)
  updateSelectInput(session, "boot_type", selected = vals$boot_type)
  updateSelectInput(session, "cormode", selected = vals$cormode)

  local_rv$pipeline_yaml_source <- source_path
  local_rv$pipeline_yaml_loaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  invisible(vals)
}

setup_current_prepared_for_export <- function(input, local_rv) {
  if (identical(local_rv$analysis_source, "attach")) {
    prepared <- local_rv$prepared_analysis
    if (!is_valid_prepared_analysis(prepared) || is.null(prepared$pipeline_spec)) {
      return(NULL)
    }
    return(prepared)
  }

  if (identical(as.character(input$data_source)[1], "bids") && isTRUE(input$bids_use_pipeline)) {
    pipeline_spec <- build_bids_pipeline_spec_from_setup(
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
    )

    return(build_prepared_analysis_from_bids_pipeline(
      pipeline_spec = pipeline_spec,
      pls_options = collect_pls_options_from_setup(input, local_rv),
      summary = if (!is.null(local_rv$bids_spec)) summarize_pls_spec_for_review(local_rv$bids_spec) else NULL,
      analyze_mode = as.character(input$bids_analyze_mode)[1]
    ))
  }

  NULL
}

setup_register_export_handlers <- function(input, output, session, local_rv) {
  roots <- c(home = "~", wd = ".")
  shinyFiles::shinyFileChoose(
    input,
    "pipeline_yaml_file",
    roots = roots,
    session = session
  )

  output$recent_pipeline_ui <- renderUI({
    index <- setup_read_recent_pipeline_index()
    choices <- setup_recent_pipeline_choices(index)
    if (!length(choices)) return(NULL)

    div(
      class = "pls-setup-recent",
      selectInput(
        session$ns("recent_pipeline_key"),
        label = NULL,
        choices = choices,
        selected = unname(choices[[1]]),
        width = "260px"
      ),
      actionButton(
        session$ns("btn_load_recent_pipeline"),
        "Load Recent",
        class = "btn btn-outline-secondary pls-btn-secondary"
      )
    )
  })

  current_exportable <- reactive({
    tryCatch(setup_current_prepared_for_export(input, local_rv), error = function(e) NULL)
  })

  observe({
    exportable <- !is.null(current_exportable())
    if (exportable) {
      shinyjs::enable("download_yaml")
      shinyjs::enable("btn_show_cli")
    } else {
      shinyjs::disable("download_yaml")
      shinyjs::disable("btn_show_cli")
    }
  })

  output$download_yaml <- downloadHandler(
    filename = function() {
      "plsrri-analysis.yml"
    },
    content = function(file) {
      prepared <- current_exportable()
      setup_export_yaml_content(prepared, file)
    }
  )

  observeEvent(input$btn_show_cli, {
    prepared <- current_exportable()
    if (is.null(prepared)) {
      showNotification("CLI export is only available for pipeline-backed BIDS configurations.", type = "message")
      return()
    }

    body <- setup_cli_modal_body(prepared, spec_path = "plsrri-analysis.yml")

    showModal(modalDialog(
      title = "CLI Commands",
      size = "l",
      easyClose = TRUE,
      tags$pre(class = "bg-light p-3 rounded small", body),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$pipeline_yaml_file, {
    tryCatch({
      spec <- setup_read_pipeline_yaml_spec(input$pipeline_yaml_file, roots)
      if (is.null(spec)) return()
      setup_record_recent_pipeline_spec(spec, source = "import")
      setup_apply_pipeline_yaml(session, local_rv, spec)
      showNotification("Pipeline YAML loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading pipeline YAML:", e$message), type = "error")
    })
  }, ignoreInit = TRUE)

  observeEvent(input$btn_load_recent_pipeline, {
    tryCatch({
      spec <- setup_load_recent_pipeline_spec(input$recent_pipeline_key)
      setup_apply_pipeline_yaml(session, local_rv, spec)
      showNotification("Recent pipeline config loaded", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading recent pipeline config:", e$message), type = "error")
    })
  }, ignoreInit = TRUE)
}
