# Helper to load Shiny modules for testServer() testing
#
# Sources all Shiny module files so they're available during testing.
# This file is automatically loaded by testthat before running tests.

# Load required packages for module testing
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
shiny_files <- c(
  "state.R",
  "theme.R",
  "ui_components.R",
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
