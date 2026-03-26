# Basic UI regression tests for inline help popovers.

testthat::skip_if_not_installed("shiny")
testthat::skip_if_not_installed("bslib")
testthat::skip_if_not_installed("bsicons")
testthat::skip_if_not_installed("shinyjs")

test_that("setup UI includes inline help icons", {
  ui <- setup_ui("setup")
  html <- paste(as.character(ui), collapse = "")

  # Smoke test: help_popover() should leave a recognizable class in the HTML.
  expect_true(grepl("pls-help-icon", html, fixed = TRUE))

  # We expect multiple help icons across the setup page (data source, design, method, resampling).
  count <- lengths(regmatches(html, gregexpr("pls-help-icon", html, fixed = TRUE)))
  expect_gte(count, 5)
})

