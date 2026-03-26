# Setup Module
# Data loading, method selection, resampling configuration

#' Setup Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI element
#' @keywords internal
setup_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "pls-fade-in",

    # Top-level analysis source switch
    div(
      class = "mb-4",
      `data-test` = "setup-analysis-source",
      radioButtons(
        ns("analysis_source"),
        label = NULL,
        choiceValues = c("attach", "direct"),
        choiceNames = list(
          tagList(icon("link"), " Attach First-Level Outputs"),
          tagList(icon("database"), " Build From Data In App")
        ),
        selected = "direct",
        inline = TRUE
      )
    ),

    # Attach mode layout
    conditionalPanel(
      condition = "input['analysis_source'] == 'attach'",
      ns = ns,
      div(
        class = "pls-setup-grid",
        # Left column
        div(
          setup_ui_attach_card(ns)
        ),
        # Right column
        div(
          setup_ui_analysis_method_card(ns),
          setup_ui_behav_seed_card(ns),
          setup_ui_resampling_card(ns)
        )
      )
    ),

    # Build-from-data mode layout (existing)
    conditionalPanel(
      condition = "input['analysis_source'] == 'direct'",
      ns = ns,
      div(
        class = "pls-setup-grid",
        # Left column
        div(
          setup_ui_data_source_card(ns),
          setup_ui_study_design_card(ns)
        ),
        # Right column
        div(
          setup_ui_analysis_method_card(ns),
          setup_ui_behav_seed_card(ns),
          setup_ui_resampling_card(ns)
        )
      )
    ),

    setup_ui_continue_row(ns)
  )
}

#' Setup Module Server
#'
#' @param id Module namespace ID
#' @param state_rv Reactive values for app state
#' @return List with continue trigger and spec
#' @keywords internal
setup_server <- function(id, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Business logic functions are sourced by app.R (fct_data_validation.R)

	    # Local reactive values
			    local_rv <- reactiveValues(
			      groups = list(
			        list(name = "Group 1", n_subj = 20),
			        list(name = "Group 2", n_subj = 18)
			      ),
			      data_loaded = FALSE,
			      data_matrices = NULL,
			      bids_path = NULL,
			      bids_output_root = NULL,
			      bids_part_df = NULL,
			      bids_spec = NULL,
			      pipeline_spec_loaded = NULL,
			      pipeline_yaml_source = NULL,
			      pipeline_yaml_loaded_at = NULL,
			      bids_error = NULL,
			      manifest_path = NULL,
			      manifest_spec = NULL,
			      manifest_error = NULL,
			      behav_loaded = FALSE,
			      behav_data = NULL,
			      mask = NULL,
			      seed_mask = NULL,
		      seed_custom_atlas = NULL,
		      seed_custom_ids = NULL,
		      validation_errors = character(0),
		      spec = NULL,
		      # Attach mode fields
		      analysis_source = "direct",
		      prepared_analysis = NULL,
		      prepared_analysis_error = character(0)
		    )

    # Triggers for external communication
    continue_trigger <- reactiveVal(0)

	    # =========================================================================
	    # Analysis Source Tracking
	    # =========================================================================

	    observeEvent(input$analysis_source, {
	      local_rv$analysis_source <- input$analysis_source
	      local_rv$prepared_analysis <- NULL
	      if (!identical(input$analysis_source, "direct")) {
	        local_rv$pipeline_spec_loaded <- NULL
	        local_rv$pipeline_yaml_source <- NULL
	        local_rv$pipeline_yaml_loaded_at <- NULL
	      }
	    }, ignoreInit = TRUE)

	    # =========================================================================
	    # Attach Mode Handlers
	    # =========================================================================

	    roots <- c(home = "~", wd = ".")
	    setup_register_attach_handlers(input, output, session, local_rv, roots)

	    # =========================================================================
	    # Data Source Handling (Build From Data mode)
	    # =========================================================================

	    setup_register_data_source_handlers(input, output, session, local_rv)

	    seed_atlas <- setup_register_behavior_seed_handlers(input, output, session, local_rv)
	
		    # =========================================================================
		    # Group Management
		    # =========================================================================

	    setup_register_group_handlers(input, output, session, local_rv, ns)
	
    # =========================================================================
    # Method Description
    # =========================================================================

    setup_register_method_description(input, output)
	
	    # =========================================================================
	    # Status Indicators
	    # =========================================================================

	    setup_register_status_indicators(input, output, session, local_rv, seed_atlas)
	
    # =========================================================================
    # Validation
    # =========================================================================

	    validate_setup <- setup_register_validation_handlers(input, output, session, local_rv, seed_atlas)

	    # =========================================================================
	    # Export Handlers
	    # =========================================================================

	    setup_register_export_handlers(input, output, session, local_rv)
	
    # =========================================================================
    # Continue Handler
    # =========================================================================

	    setup_register_continue_handler(
	      input = input,
	      output = output,
	      session = session,
	      local_rv = local_rv,
	      continue_trigger = continue_trigger,
	      validate_setup = validate_setup,
	      seed_atlas = seed_atlas
	    )
	
    # =========================================================================
    # Return Values
    # =========================================================================

    list(
      continue = continue_trigger,
      spec = reactive({ local_rv$spec }),
      analysis_source = reactive({ local_rv$analysis_source }),
      prepared_analysis = reactive({ local_rv$prepared_analysis })
    )
  })
}
