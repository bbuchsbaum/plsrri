# Technology Stack: Shiny GUI Testing

**Project:** plsrri Shiny GUI Quality Infrastructure
**Researched:** 2026-01-21
**Overall Confidence:** HIGH

## Executive Summary

The 2025/2026 Shiny testing ecosystem has matured around a three-tier approach: (1) unit testing with testthat for business logic, (2) testServer() for reactive server-side testing, and (3) shinytest2 for end-to-end browser-based testing. For plsrri's modular architecture with R6 state management and planned surfwidget integration, this stack provides comprehensive coverage while balancing test speed with behavioral fidelity.

---

## Recommended Testing Stack

### Core Testing Framework

| Technology | Version | Purpose | Confidence |
|------------|---------|---------|------------|
| testthat | 3.3.2 | Unit testing foundation, snapshot testing | HIGH |
| shiny::testServer() | 1.12.1 (built-in) | Server-side reactive testing | HIGH |
| shinytest2 | 0.5.0 | End-to-end browser testing | HIGH |
| shinytesters | 0.1.0 | Mock update functions in testServer() | MEDIUM |
| covr | 3.6.5 | Code coverage reporting | HIGH |

### Why This Stack

**testthat 3.3.2** (Published: 2026-01-11)
- Industry standard R testing framework
- Native support for R6 class mocking via `local_mocked_r6_class()`
- Snapshot testing for complex outputs
- plsrri already uses testthat (edition 3 configured in DESCRIPTION)

**testServer()** (Built into shiny 1.12.1)
- Fast module testing without browser overhead
- Tests reactive logic directly
- Perfect for plsrri's 6 modules (setup, analyze, explore, brain_viewer, filter_bar, inspector)
- Works with MockShinySession for R6 state testing

**shinytest2 0.5.0** (Published: 2026-01-09)
- End-to-end testing via headless Chromium
- AppDriver class for programmatic interaction
- Required for testing surfwidget htmlwidget rendering
- Integrates with testthat's snapshot infrastructure

**shinytesters 0.1.0** (Published: 2025-09-01)
- Bridges gap in testServer() for update functions
- Auto-updates inputs when updateSelectInput() etc. are called in observers
- Eliminates need for manual session$setInputs() in chained reactivity

---

## When to Use Each Testing Approach

### Decision Matrix

| Scenario | Use | Why |
|----------|-----|-----|
| Pure R functions in R/ | testthat | No Shiny context needed |
| R6 AppState class methods | testthat + local_mocked_r6_class() | Test state transitions directly |
| Module server reactive logic | testServer() | Fast, no browser, tests reactivity |
| Module with update* functions | testServer() + shinytesters | Mock updates to test observer chains |
| Full workflow (Setup -> Analyze -> Explore) | shinytest2 AppDriver | End-to-end user simulation |
| surfwidget/htmlwidget rendering | shinytest2 + run_js() | Need real browser for WebGL |
| Visual regression | shinytest2 expect_screenshot() | Use sparingly - brittle |

### Testing Pyramid for plsrri

```
                    /\
                   /  \   shinytest2 (E2E)
                  /----\  - Critical workflows only
                 /      \ - surfwidget verification
                /--------\
               /          \  testServer()
              /------------\ - All 6 module servers
             /              \ - Reactive chains
            /----------------\
           /                  \  testthat (Unit)
          /--------------------\ - R6 AppState
         /                      \ - Business logic
        /------------------------\ - Utility functions
```

---

## testServer() Patterns for plsrri

### Basic Module Testing

```r
test_that("brain_viewer_server responds to filter changes", {
  # Create mock result
  mock_result <- create_test_pls_result()

  testServer(
    brain_viewer_server,
    args = list(
      result_rv = reactive(mock_result),
      filters = list(
        lv = reactive(1L),
        bsr_threshold = reactive(3.0),
        what = reactive("bsr")
      )
    ),
    {
      # Test initial state
      expect_equal(local_rv$view_mode, "montage")

      # Simulate button click
      session$setInputs(btn_ortho = 1)
      expect_equal(local_rv$view_mode, "ortho")

      # Check output exists
      expect_true(!is.null(output$brain_plot))
    }
  )
})
```

### Testing R6 State with MockShinySession

```r
test_that("AppState advances steps correctly", {
  # Test R6 directly (no Shiny needed)
  state <- AppState$new()

  expect_equal(state$step, 1L)
  state$advance_step()
  expect_equal(state$step, 2L)
  expect_equal(state$max_step, 2L)

  # Cannot skip ahead

  state$set_step(3L)
  expect_equal(state$step, 2L)  # Still 2, not 3
})

test_that("create_reactive_state syncs with R6", {
  testServer(
    function(input, output, session) {
      app_state <- create_reactive_state(session)
      rv <- app_state$rv

      # Expose for testing
      output$step <- renderText(rv$step)

      observeEvent(input$advance, {
        app_state$state$advance_step()
        app_state$sync_to_rv()
      })
    },
    {
      expect_equal(output$step, "1")
      session$setInputs(advance = 1)
      expect_equal(output$step, "2")
    }
  )
})
```

### Using shinytesters for Update Functions

```r
test_that("setup module updates condition choices", {
  shinytesters::use_shiny_testers("shiny")

  testServer(
    setup_server,
    args = list(rv = reactiveValues(spec = NULL)),
    {
      # When data source changes, condition dropdown updates
      session$setInputs(data_source = "bids", bids_path = "/test/path")

      # shinytesters makes this work - updateSelectInput is mocked
      expect_true(length(input$conditions) > 0)
    }
  )
})
```

---

## shinytest2 Patterns for plsrri

### Basic AppDriver Usage

```r
test_that("complete workflow from setup to explore", {
  skip_on_cran()  # CRAN policy: no chromote in tests

  app <- AppDriver$new(
    app_dir = system.file("shiny", package = "plsrri"),
    name = "workflow-test",
    timeout = 10000
  )
  on.exit(app$stop(), add = TRUE)

  # Step 1: Setup
  app$set_inputs(`setup-data_source` = "manual")
  app$upload_file(`setup-data_file` = test_data_path())
  app$click("setup-btn_continue")

  # Wait for step transition
  app$wait_for_value(input = "stepper-current_step", ignore = list(1L))

  # Verify we're on analyze step
  expect_equal(app$get_value(input = "stepper-current_step"), 2L)
})
```

### htmlwidget Testing (surfwidget)

```r
test_that("surfwidget renders brain surface", {
  skip_on_cran()
  skip_if_not_installed("neurosurf")

  app <- AppDriver$new(
    app_dir = test_app_with_surfwidget(),
    timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  # Wait for widget initialization
  app$wait_for_js(
    "document.querySelector('#surface-viewer canvas') !== null",
    timeout = 10000
  )

  # Verify three.js scene exists via JavaScript
  scene_exists <- app$get_js(
    "!!HTMLWidgets.find('#surface-viewer').getScene()"
  )
  expect_true(scene_exists)

  # Test programmatic interaction via widget's JS API
  app$run_js("
    var widget = HTMLWidgets.find('#surface-viewer');
    widget.setView('lateral');
  ")

  # Verify view changed
  current_view <- app$get_js(
    "HTMLWidgets.find('#surface-viewer').getView()"
  )
  expect_equal(current_view, "lateral")
})
```

### Value Snapshots (Preferred over Screenshots)

```r
test_that("explore panel shows correct LV summary", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shiny", package = "plsrri"),
    load_timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  # Navigate to explore with preloaded result
  load_test_result(app)

  # Snapshot input/output values, NOT screenshot
  app$expect_values(
    input = c("explore-lv_select", "explore-threshold"),
    output = c("explore-lv_summary", "explore-variance_explained")
  )
})
```

### Custom AppDriver Extensions

```r
# Helper for surfwidget testing
SurfwidgetDriver <- R6::R6Class(
  "SurfwidgetDriver",
  inherit = shinytest2::AppDriver,
  public = list(
    wait_for_surface = function(id, timeout = 10000) {
      selector <- sprintf("#%s canvas", id)
      self$wait_for_js(
        sprintf("document.querySelector('%s') !== null", selector),
        timeout = timeout
      )
    },

    set_surface_view = function(id, view) {
      self$run_js(sprintf(
        "HTMLWidgets.find('#%s').setView('%s');",
        id, view
      ))
    },

    get_surface_data = function(id) {
      self$get_js(sprintf(
        "JSON.stringify(HTMLWidgets.find('#%s').getData())",
        id
      ))
    }
  )
)
```

---

## Mock Data & Fixtures

### Directory Structure

```
tests/
  testthat/
    fixtures/
      pls_result_minimal.rds    # Minimal valid pls_result
      pls_result_with_mask.rds  # Result with brain mask for plotting
      surface_geometry.rds      # Test SurfaceGeometry for surfwidget
      test_datamat.rds          # Small test data matrix
    helpers/
      helper-create-fixtures.R  # Functions to create test data
      helper-shiny.R            # Shiny test utilities
    setup-shinytest2.R          # shinytest2 configuration
```

### Fixture Creation Pattern

```r
# tests/testthat/helpers/helper-create-fixtures.R

#' Create minimal valid pls_result for testing
create_test_pls_result <- function(n_lv = 3, n_voxels = 100, with_mask = FALSE) {
  # Small synthetic result that exercises all code paths
  result <- list(
    u = matrix(rnorm(n_voxels * n_lv), ncol = n_lv),
    v = matrix(rnorm(10 * n_lv), ncol = n_lv),
    s = runif(n_lv, 1, 10),
    perm_result = list(
      sprob = runif(n_lv, 0, 0.1)
    ),
    boot_result = list(
      bsr = matrix(rnorm(n_voxels * n_lv), ncol = n_lv)
    )
  )

  if (with_mask) {
    result$mask <- create_test_mask(n_voxels)
  }

  class(result) <- c("pls_result", "list")
  result
}

#' Create test SurfaceGeometry for surfwidget testing
create_test_surface <- function() {
  if (!requireNamespace("neurosurf", quietly = TRUE)) {
    skip("neurosurf not available")
  }

  # Minimal valid surface for widget rendering
  # ... implementation
}
```

### Test Isolation with withr

```r
test_that("analysis handles temp directory correctly", {
  withr::local_tempdir(pattern = "pls_test")

  # Test runs in isolated temp directory
  # Automatically cleaned up after test
})
```

---

## Code Coverage

### Configuration

```r
# In tests/testthat/setup.R or helper file

# Skip coverage for generated code
# nocov start
# ... RcppExports.R content ...
# nocov end
```

### .covrignore

```
inst/shiny/www/*
*.js
*.css
```

### Running Coverage

```r
# Package coverage
covr::package_coverage()

# Coverage report with Shiny app viewer
covr::report()

# CI-friendly coverage
covr::codecov(token = Sys.getenv("CODECOV_TOKEN"))
```

---

## Installation Commands

```r
# Core testing stack
install.packages(c("testthat", "shinytest2", "covr"))

# For update function mocking in testServer
install.packages("shinytesters")

# Chromium browser (required for shinytest2)
# Installs automatically, or manually:
chromote::find_chrome()
```

### DESCRIPTION Additions

```
Suggests:
    testthat (>= 3.3.1),
    shinytest2 (>= 0.5.0),
    shinytesters (>= 0.1.0),
    covr (>= 3.6.0),
    withr
```

---

## CI/CD Integration

### GitHub Actions Workflow

```yaml
# .github/workflows/test.yml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - uses: r-lib/actions/setup-r-dependencies@v2

      # Unit and testServer tests (fast)
      - name: Run tests
        run: |
          testthat::test_local()
        shell: Rscript {0}

  e2e:
    runs-on: ubuntu-latest
    needs: test  # Only run if unit tests pass
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - uses: r-lib/actions/setup-r-dependencies@v2

      # Chrome for shinytest2
      - uses: browser-actions/setup-chrome@latest

      - name: Run E2E tests
        run: |
          shinytest2::test_app("inst/shiny")
        shell: Rscript {0}
```

---

## Key Recommendations

### 1. Start with testServer() for Module Testing

The plsrri Shiny app has 6 well-defined modules. Test each module server in isolation with testServer() before adding E2E tests. This catches 80% of bugs with 20% of the effort.

### 2. Use shinytest2 Sparingly but Strategically

Reserve shinytest2 for:
- Critical user workflows (Setup -> Analyze -> Explore)
- surfwidget/htmlwidget verification (requires real browser)
- Integration points between modules

### 3. Avoid Screenshot Snapshots

Screenshots are the most brittle test type. Use `expect_values()` for inputs/outputs instead. Only use `expect_screenshot()` for surfwidget visual verification where no other option exists.

### 4. Test R6 AppState Directly

The AppState class can be tested with plain testthat - no Shiny context needed for most methods. This is faster and more reliable.

### 5. Create Reusable Fixtures

Build a library of test fixtures (minimal pls_result, test surfaces) that can be loaded quickly. Avoid creating heavy test data in each test.

---

## Sources

### Official Documentation (HIGH Confidence)
- [shinytest2 Official Documentation](https://rstudio.github.io/shinytest2/)
- [testServer() Reference](https://shiny.posit.co/r/reference/shiny/latest/testserver.html)
- [testthat Mocking Guide](https://testthat.r-lib.org/articles/mocking.html)
- [local_mocked_r6_class() Reference](https://testthat.r-lib.org/reference/local_mocked_r6_class.html)
- [AppDriver Reference](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
- [Robust Testing Article](https://rstudio.github.io/shinytest2/articles/robust.html)
- [covr Package](https://covr.r-lib.org/)

### CRAN Packages (HIGH Confidence - Version Verified)
- [testthat 3.3.2](https://cran.r-project.org/package=testthat) - Published 2026-01-11
- [shinytest2 0.5.0](https://cran.r-project.org/package=shinytest2) - Published 2026-01-09
- [shiny 1.12.1](https://cran.r-project.org/package=shiny) - Published 2025-12-09
- [shinytesters 0.1.0](https://cran.r-project.org/package=shinytesters) - Published 2025-09-01
- [covr 3.6.5](https://cran.r-project.org/package=covr) - Published 2025-11-09

### Community Resources (MEDIUM Confidence)
- [Mastering Shiny - Testing Chapter](https://mastering-shiny.org/scaling-testing.html)
- [Appsilon - testServer Guide](https://www.appsilon.com/post/how-to-write-tests-with-shiny-testserver)
- [R-bloggers - shinytesters Release](https://www.r-bloggers.com/2025/08/shinytesters-updating-inputs-in-testserver/)
- [R-bloggers - JavaScript APIs in shinytest2](https://www.r-bloggers.com/2025/10/simplifying-interactions-with-complex-widgets-in-shinytest2-using-javascript-apis/)
- [ThinkR - MockShinySession with R6](https://rtask.thinkr.fr/setting-values-in-r6-classes-and-testing-shinymockshinysession/)
- [Jumping Rivers - shinytest2 Series](https://www.jumpingrivers.com/blog/end-to-end-testing-shinytest2-part-1/)

### Project Context (Verified)
- neurosurf surfwidget uses htmlwidgets framework: [GitHub](https://github.com/bbuchsbaum/neurosurf)
- plsrri existing test structure: testthat edition 3 configured
