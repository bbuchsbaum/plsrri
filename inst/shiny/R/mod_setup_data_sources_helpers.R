# Setup module data source helpers
# Shared utilities for shinyFiles + common parsing.

setup_shinyfiles_is_unselected <- function(info) {
  is.null(info) ||
    (is.atomic(info) && length(info) == 0) ||
    is.integer(info) ||
    (is.numeric(info) && length(info) == 1 && info == as.integer(info))
}

