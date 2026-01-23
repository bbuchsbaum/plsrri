# Helper to load Shiny modules for testServer() testing
#
# Sources all Shiny module files so they're available during testing.
# This file is automatically loaded by testthat before running tests.

# Load required packages for module testing (skip if optional deps missing)
required_pkgs <- c("shiny", "shinyjs", "bslib", "bsicons", "shinyFiles", "R6")
has_pkg <- vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
if (!all(has_pkg)) {
  # Avoid hard failures when Suggests packages are not installed. The
  # corresponding module tests should skip.
  return(invisible(NULL))
}

library(shiny)
library(shinyjs)

# Path to Shiny module files
shiny_r_path <- system.file("shiny", "R", package = "plsrri")

# If package not installed, use direct path
if (shiny_r_path == "") {
  shiny_r_path <- file.path(getwd(), "inst", "shiny", "R")
  # Handle case where we're in tests/testthat
  if (!dir.exists(shiny_r_path)) {
    shiny_r_path <- file.path(getwd(), "..", "..", "inst", "shiny", "R")
  }
}

# Source Shiny module files
# fct_* files must be sourced before modules that depend on them
shiny_files <- c(
  "state.R",
  "theme.R",
  "ui_components.R",
  "fct_brain_viewer.R",
  "fct_data_validation.R",
  "fct_seed_data.R",
  "fct_brain_renderer.R",
  "mod_filter_bar.R",
  "mod_brain_viewer.R",
  "mod_inspector.R",
  "mod_explore.R",
  "mod_analyze.R",
  "mod_setup.R"
)

for (f in shiny_files) {
  file_path <- file.path(shiny_r_path, f)
  if (file.exists(file_path)) {
    source(file_path, local = FALSE)
  }
}
