# Setup module status + method description
# Extracted from mod_setup.R to keep setup_server readable and maintainable.

setup_register_method_description <- function(input, output) {
  output$method_description <- renderUI({
    method <- input$method

    desc <- switch(
      method,
      task = "Identifies brain patterns that differ across task conditions. Best for experimental designs with distinct conditions.",
      behavior = "Finds brain patterns correlated with behavioral measures (RT, accuracy, etc.).",
      seed = "Seed-based connectivity: finds brain patterns correlated with one or more seed ROI signals (Behavior PLS under the hood).",
      multiblock = "Combines task effects with behavioral correlations for richer analyses.",
      ""
    )

    p(class = "small text-muted mt-2", desc)
  })
}

setup_register_status_indicators <- function(input, output, session, local_rv, seed_atlas) {
  # Keep data_loaded consistent with the selected data source
  observe({
    analysis_source <- as.character(input$analysis_source)[1]
    if (identical(analysis_source, "attach")) {
      local_rv$data_loaded <- is_valid_prepared_analysis(local_rv$prepared_analysis)
      return()
    }

    src <- as.character(input$data_source)[1]
    ready <- FALSE

    if (identical(src, "manual")) {
      ready <- !is.null(local_rv$data_matrices) && length(local_rv$data_matrices) > 0
    } else if (identical(src, "manifest")) {
      ready <- !is.null(local_rv$manifest_spec)
    } else if (identical(src, "bids")) {
      ready <- !is.null(local_rv$bids_spec) || !is.null(local_rv$pipeline_spec_loaded)
    } else if (identical(src, "load")) {
      ready <- FALSE
    }

    local_rv$data_loaded <- isTRUE(ready)
  })

  output$data_status <- renderUI({
    if (isTRUE(local_rv$data_loaded)) {
      status_dot("complete")
    } else {
      status_dot("pending")
    }
  })

  output$design_status <- renderUI({
    groups <- local_rv$groups
    n_cond <- suppressWarnings(as.integer(input$num_conditions)[1])
    if (is.na(n_cond)) n_cond <- 0L

    subj_counts <- vapply(groups, function(g) {
      x <- suppressWarnings(as.integer(g$n_subj)[1])
      if (is.na(x)) 0L else x
    }, integer(1))

    valid <- length(groups) > 0 && all(subj_counts > 0) && n_cond > 0

    if (valid) {
      status_dot("complete")
    } else {
      status_dot("pending")
    }
  })

  output$behav_status <- renderUI({
    method <- as.character(input$method)[1]

    if (identical(method, "behavior") || identical(method, "multiblock")) {
      ok <- isTRUE(local_rv$behav_loaded) && !is.null(local_rv$behav_data)
      return(status_dot(if (ok) "complete" else "pending"))
    }

    if (identical(method, "seed")) {
      seed_source <- as.character(input$seed_source)[1]
      use_all <- isTRUE(input$seed_use_all_labels)

      ok <- !is.null(local_rv$mask) && switch(
        seed_source,
        atlas = shiny::isTruthy(input$seed_rois) && !is.null(seed_atlas()),
        mask = !is.null(local_rv$seed_mask),
        custom = !is.null(local_rv$seed_custom_atlas) &&
          if (use_all) {
            !is.null(local_rv$seed_custom_ids) && length(local_rv$seed_custom_ids) > 0
          } else {
            shiny::isTruthy(input$seed_label_ids)
          },
        FALSE
      )
      return(status_dot(if (ok) "complete" else "pending"))
    }

    status_dot("pending")
  })
}
