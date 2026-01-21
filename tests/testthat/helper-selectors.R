# Data-test attribute reference for Shiny module testing
#
# This file documents all data-test attributes added to Shiny module UI elements
# for reliable test selection without relying on namespaced IDs.
#
# Convention: `data-test = "module-element"` (e.g., "setup-continue-btn")
#
# =============================================================================
# Setup Module (mod_setup.R)
# =============================================================================
#   setup-data-source      - Radio buttons for data source selection
#   setup-bids-btn         - BIDS directory browser button
#   setup-file-upload      - File upload input wrapper
#   setup-num-conditions   - Number of conditions input
#   setup-method           - Analysis method radio buttons
#   setup-num-perm         - Permutation count input
#   setup-num-boot         - Bootstrap sample count input
#   setup-continue-btn     - Continue to analyze button
#   setup-validation       - Validation messages area
#   setup-data-status      - Data status indicator dot
#   setup-design-status    - Design status indicator dot
#
# =============================================================================
# Analyze Module (mod_analyze.R)
# =============================================================================
#   analyze-progress         - Progress bar container
#   analyze-progress-percent - Progress percentage text
#   analyze-stage            - Current analysis stage display
#   analyze-elapsed          - Elapsed time display
#   analyze-cancel-btn       - Cancel analysis button
#   analyze-back-btn         - Back to setup button
#   analyze-error            - Error display container
#   analyze-complete         - Completion display container
#   analyze-explore-btn      - Explore results button (on completion)
#
# =============================================================================
# Explore Module (mod_explore.R)
# =============================================================================
#   explore-lv-list       - Latent variable list container
#   explore-variance-plot - Variance explained plot
#   explore-scores-plot   - Main design scores plot
#
# =============================================================================
# Filter Bar Module (mod_filter_bar.R)
# =============================================================================
#   filter-lv-select      - LV selector dropdown
#   filter-bsr-threshold  - BSR threshold input
#   filter-p-threshold    - P-value threshold input
#   filter-view-mode      - View mode selector (montage/ortho)
#   filter-what           - Display selector (BSR/Salience)
#
# =============================================================================
# Brain Viewer Module (mod_brain_viewer.R)
# =============================================================================
#   brain-montage-btn  - Montage view toggle button
#   brain-ortho-btn    - Orthogonal view toggle button
#   brain-axis-select  - Axis selector (axial/coronal/sagittal)
#   brain-plot         - Brain plot output container
#   brain-colorbar     - Colorbar legend info
#
# =============================================================================
# Inspector Module (mod_inspector.R)
# =============================================================================
#   inspector-lv-details    - LV details section
#   inspector-scores-plot   - Design scores plot
#   inspector-export-nifti  - Export NIfTI button
#   inspector-export-csv    - Export CSV button
#   inspector-export-pdf    - Export PDF button
#   inspector-export-report - Export HTML report button
#

# =============================================================================
# SELECTORS constant for programmatic access
# =============================================================================
# Provides structured access to data-test attribute values for use in tests.

SELECTORS <- list(
  setup = list(
    data_source = "setup-data-source",
    bids_btn = "setup-bids-btn",
    file_upload = "setup-file-upload",
    num_conditions = "setup-num-conditions",
    method = "setup-method",
    num_perm = "setup-num-perm",
    num_boot = "setup-num-boot",
    continue_btn = "setup-continue-btn",
    validation = "setup-validation",
    data_status = "setup-data-status",
    design_status = "setup-design-status"
  ),
  analyze = list(
    progress = "analyze-progress",
    progress_percent = "analyze-progress-percent",
    stage = "analyze-stage",
    elapsed = "analyze-elapsed",
    cancel_btn = "analyze-cancel-btn",
    back_btn = "analyze-back-btn",
    error = "analyze-error",
    complete = "analyze-complete",
    explore_btn = "analyze-explore-btn"
  ),
  explore = list(
    lv_list = "explore-lv-list",
    variance_plot = "explore-variance-plot",
    scores_plot = "explore-scores-plot"
  ),
  filter = list(
    lv_select = "filter-lv-select",
    bsr_threshold = "filter-bsr-threshold",
    p_threshold = "filter-p-threshold",
    view_mode = "filter-view-mode",
    what = "filter-what"
  ),
  brain = list(
    montage_btn = "brain-montage-btn",
    ortho_btn = "brain-ortho-btn",
    axis_select = "brain-axis-select",
    plot = "brain-plot",
    colorbar = "brain-colorbar"
  ),
  inspector = list(
    lv_details = "inspector-lv-details",
    scores_plot = "inspector-scores-plot",
    export_nifti = "inspector-export-nifti",
    export_csv = "inspector-export-csv",
    export_pdf = "inspector-export-pdf",
    export_report = "inspector-export-report"
  )
)

# =============================================================================
# Usage Notes
# =============================================================================
#
# testServer() tests (Phase 1-4):
#   testServer() tests interact with server logic via session$setInputs()
#   and session$returned(). Data-test attributes are not directly used here
#   because we're testing server logic, not UI selection.
#
#   Example:
#     testServer(setup_server, args = list(state_rv = state_rv), {
#       session$setInputs(method = "task")
#       session$setInputs(num_perm = 500)
#       expect_equal(input$method, "task")
#     })
#
# shinytest2 E2E tests (Phase 5):
#   For browser-based E2E testing, use data-test attributes via CSS selectors.
#   This provides stable element selection regardless of namespace changes.
#
#   Example:
#     app$click("[data-test='setup-continue-btn']")
#     app$set_inputs(`[data-test='setup-method']` = "behavior")
#     app$expect_values(selector = "[data-test='analyze-progress-percent']")
#
# Helper function for CSS selector generation:
#   data_test_selector <- function(name) {
#     sprintf("[data-test='%s']", name)
#   }
#
#   # Usage:
#   app$click(data_test_selector(SELECTORS$setup$continue_btn))
#

#' Generate CSS selector for data-test attribute
#'
#' @param test_id The data-test attribute value
#' @return CSS selector string
#' @examples
#' data_test_selector("setup-continue-btn")
#' # Returns: "[data-test='setup-continue-btn']"
#' @keywords internal
data_test_selector <- function(test_id) {
  sprintf("[data-test='%s']", test_id)
}
