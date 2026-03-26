#' Shiny GUI Launcher
#'
#' @description
#' Functions for launching the interactive Shiny GUI for PLS neuroimaging analysis.
#'
#' @name pls-gui
NULL

#' Launch PLS GUI
#'
#' @description
#' Launches the interactive Shiny GUI for PLS neuroimaging analysis.
#' The GUI provides a modern, workflow-focused interface for:
#'
#' - Loading data (BIDS directories, matrices, or saved specifications)
#' - Configuring analysis parameters (method, resampling, etc.)
#' - Running PLS analysis with progress tracking
#' - Exploring results with interactive brain visualization
#' - Exporting results (NIfTI, CSV, PDF, HTML reports)
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'   Common options include:
#'   - `port`: TCP port for the server (default: automatic)
#'   - `host`: Host IP address (default: "127.0.0.1")
#'   - `launch.browser`: Launch browser automatically (default: TRUE)
#'
#' @return Invisibly returns the Shiny app object when the app closes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch GUI with default settings
#' launch_pls_gui()
#'
#' # Launch on specific port
#' launch_pls_gui(port = 3838)
#'
#' # Launch without opening browser
#' launch_pls_gui(launch.browser = FALSE)
#' }
launch_pls_gui <- function(...) {
  # Check for required packages
  required_pkgs <- c("shiny", "bslib", "shinyjs", "R6")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required for the GUI but not installed:\n",
      paste("  -", missing_pkgs, collapse = "\n"),
      "\n\nInstall with: install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      "))",
      call. = FALSE
    )
  }

  # Find app directory

  app_dir <- system.file("shiny", package = "plsrri")

  if (app_dir == "" || !dir.exists(app_dir)) {
    stop(
      "Could not find Shiny app directory. ",
      "Make sure plsrri is installed correctly with GUI files.",
      call. = FALSE
    )
  }

  # Check that app.R exists
  if (!file.exists(file.path(app_dir, "app.R"))) {
    stop(
      "app.R not found in Shiny directory. ",
      "The package installation may be incomplete.",
      call. = FALSE
    )
  }

  cli::cli_alert_info("Launching PLS Neuroimaging Analysis GUI...")
  cli::cli_alert_info("Press Ctrl+C or close the browser to stop the app.")

  # Run the app
  shiny::runApp(app_dir, ...)
}

#' Check GUI Dependencies
#'
#' @description
#' Checks if all required packages for the Shiny GUI are installed.
#'
#' @return Logical indicating whether all dependencies are met.
#' @export
#'
#' @examples
#' check_gui_deps()
check_gui_deps <- function() {
  required_pkgs <- c(
    "shiny",
    "bslib",
    "bsicons",
    "shinyjs",
    "shinyFiles",
    "R6"
  )

  suggested_pkgs <- c(
    "future",
    "promises"
  )

  cli::cli_h2("PLS GUI Dependency Check")

  # Check required
  cli::cli_h3("Required packages")
  all_present <- TRUE

  for (pkg in required_pkgs) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_alert_success("{pkg}")
    } else {
      cli::cli_alert_danger("{pkg} - NOT INSTALLED")
      all_present <- FALSE
    }
  }

  # Check suggested
  cli::cli_h3("Suggested packages (for async execution)")

  for (pkg in suggested_pkgs) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_alert_success("{pkg}")
    } else {
      cli::cli_alert_warning("{pkg} - not installed (optional)")
    }
  }

  if (!all_present) {
    cli::cli_alert_info(
      "Install missing packages with:\n",
      "install.packages(c('shiny', 'bslib', 'bsicons', 'shinyjs', 'shinyFiles', 'R6'))"
    )
  }

  invisible(all_present)
}
