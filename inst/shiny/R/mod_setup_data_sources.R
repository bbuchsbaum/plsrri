# Setup module data source handlers (coordinator)
# Split into submodules for readability and testability.

setup_register_data_source_handlers <- function(input, output, session, local_rv) {
  roots <- c(home = "~", wd = ".")

  # BIDS directory selection
  shinyFiles::shinyDirChoose(
    input,
    "bids_dir",
    roots = roots,
    session = session
  )

  shinyFiles::shinyDirChoose(
    input,
    "bids_output_root_dir",
    roots = roots,
    session = session
  )

  # Manifest file selection
  shinyFiles::shinyFileChoose(
    input,
    "manifest_file",
    roots = roots,
    session = session
  )

  setup_register_bids_data_source(input, output, session, local_rv, roots)
  setup_register_manifest_data_source(input, output, session, local_rv, roots)
  setup_register_manual_data_source(input, output, session, local_rv)
  setup_register_load_spec_data_source(input, output, session, local_rv)
}
