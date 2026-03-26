# Setup module group handlers
# Extracted from mod_setup.R to keep setup_server readable and maintainable.

setup_register_group_handlers <- function(input, output, session, local_rv, ns) {
  output$group_inputs <- renderUI({
    groups <- local_rv$groups

    lapply(seq_along(groups), function(i) {
      div(
        class = "d-flex align-items-center gap-2 mb-2",
        textInput(
          ns(paste0("group_name_", i)),
          label = NULL,
          value = groups[[i]]$name,
          placeholder = paste("Group", i),
          width = "120px"
        ),
        numericInput(
          ns(paste0("group_subj_", i)),
          label = NULL,
          value = groups[[i]]$n_subj,
          min = 1,
          width = "80px"
        ),
        span(class = "text-muted small", "subjects")
      )
    })
  })

  observeEvent(input$add_group, {
    groups <- local_rv$groups
    new_idx <- length(groups) + 1
    groups[[new_idx]] <- list(name = paste("Group", new_idx), n_subj = 10)
    local_rv$groups <- groups
  })

  observeEvent(input$remove_group, {
    groups <- local_rv$groups
    if (length(groups) > 1) {
      local_rv$groups <- groups[-length(groups)]
    }
  })

  # Sync group inputs back to local_rv
  observe({
    groups <- local_rv$groups
    for (i in seq_along(groups)) {
      name_input <- input[[paste0("group_name_", i)]]
      subj_input <- input[[paste0("group_subj_", i)]]

      if (!is.null(name_input)) {
        groups[[i]]$name <- name_input
      }
      if (!is.null(subj_input)) {
        groups[[i]]$n_subj <- as.integer(subj_input)
      }
    }
    local_rv$groups <- groups
  })

  output$design_summary <- renderUI({
    groups <- local_rv$groups
    n_cond <- suppressWarnings(as.integer(input$num_conditions)[1])
    if (is.na(n_cond) || n_cond < 1) n_cond <- 1L

    subj_counts <- vapply(groups, function(g) {
      x <- suppressWarnings(as.integer(g$n_subj)[1])
      if (is.na(x) || x < 0) 0L else x
    }, integer(1))
    total_subj <- sum(subj_counts)
    total_obs <- total_subj * n_cond

    div(
      class = "small text-muted mt-3 p-2 bg-light rounded",
      sprintf("Total: %d subjects, %d observations", total_subj, total_obs)
    )
  })
}

