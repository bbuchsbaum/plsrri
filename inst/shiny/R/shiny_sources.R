# Shared manifest of Shiny app source files.
#
# Keep this list in dependency order (helpers/functions before modules).
# Used by:
# - inst/shiny/app.R
# - tests/testthat/helper-shiny-modules.R
# - tests/testthat/test-mod_setup.R

plsrri_shiny_source_list <- function() {
  c(
    # Core helpers
    "theme.R",
    "state.R",
    "ui_components.R",

    # Pure functions
    "fct_brain_viewer.R",
    "fct_data_validation.R",
    "fct_prepared_analysis.R",
    "fct_seed_data.R",
    "fct_brain_renderer.R",
    "fct_surface_mapper.R",

    # Setup (submodules first)
    "mod_setup_ui_cards.R",
    "mod_setup_data_sources_helpers.R",
    "mod_setup_data_sources_bids.R",
    "mod_setup_data_sources_manifest.R",
    "mod_setup_data_sources_manual.R",
    "mod_setup_data_sources_load_spec.R",
    "mod_setup_data_sources.R",
    "mod_setup_behavior_seed.R",
    "mod_setup_groups.R",
    "mod_setup_status.R",
    "mod_setup_validation.R",
    "mod_setup_export.R",
    "mod_setup_attach.R",
    "mod_setup_continue.R",
    "mod_setup.R",

    # Analysis + explore
    "mod_analyze.R",
    "mod_explore.R",
    "mod_brain_viewer.R",
    "mod_surface_viewer.R",
    "mod_filter_bar.R",
    "mod_inspector.R"
  )
}

plsrri_source_shiny_files <- function(shiny_r_path, local = FALSE) {
  stopifnot(is.character(shiny_r_path), length(shiny_r_path) == 1)

  shiny_files <- plsrri_shiny_source_list()
  for (f in shiny_files) {
    file_path <- file.path(shiny_r_path, f)
    if (!file.exists(file_path)) {
      stop(sprintf("Missing Shiny source file: %s", file_path), call. = FALSE)
    }
    source(file_path, local = local)
  }

  invisible(NULL)
}
