# Domain Pitfalls: Shiny Testing & GUI Quality

**Domain:** Shiny GUI hardening (testing, design refinement, htmlwidget integration)
**Project:** plsrri Shiny GUI
**Researched:** 2026-01-21
**Confidence:** MEDIUM-HIGH (verified via official docs, community sources, and project inspection)

---

## Critical Pitfalls

Mistakes that cause rewrites, major test suite failures, or broken integrations.

---

### Pitfall 1: Fragile shinytest2 Tests Coupled to Implementation Details

**What goes wrong:** Tests break constantly when implementation changes, even though app behavior is unchanged. Test suite becomes a maintenance burden rather than a safety net.

**Why it happens:**
- Using raw input IDs that change when modules are restructured
- Over-reliance on screenshot testing that fails due to OS, font, or Chrome rendering differences
- Snapshot testing that captures incidental details (DOM structure, whitespace) rather than behavior
- Testing implementation wiring ("did the plot render?") instead of behavior ("does the plot show correct data?")

**Consequences:**
- False-positive failures unrelated to code changes
- Team loses trust in test suite and starts ignoring failures
- Test maintenance time exceeds value provided
- CI/CD pipeline becomes unreliable

**Prevention:**
1. **Use data-test attributes** instead of namespaced input IDs:
   ```r
   actionButton("add", label = "Add", `data-test` = "button-add")
   # Test with:
   app$click(selector = "[data-test=button-add]")
   ```
2. **Prefer `expect_values()` over screenshots**: Test computed values, not visual appearance
3. **Use `shiny::exportTestValues()`** to expose reactive values for direct testing
4. **Test behavior, not wiring**: Don't test that Shiny renders correctly; test that your code produces correct data

**Detection (warning signs):**
- Tests fail after CSS-only changes
- Tests fail differently on CI vs local machine
- Test failures don't correlate with actual bugs
- Namespaced IDs like `"setup-data_source-manual"` appear in test code

**Phase mapping:** Testing phase - establish test architecture patterns early

**Sources:**
- [Robust testing - shinytest2](https://rstudio.github.io/shinytest2/articles/robust.html)
- [How to Write Robust shinytest2 Tests](https://jakubsobolewski.com/blog/robust-shinytest2/)
- [Creating Robust E2E Test Selectors](https://jakubsobolewski.com/blog/robust-targetting-of-html-for-tests/)

---

### Pitfall 2: R6 State + reactiveValues Synchronization Failures

**What goes wrong:** The `AppState` R6 class and `reactiveValues` wrapper get out of sync. State changes in R6 don't trigger reactive updates, or reactive updates don't persist to R6.

**Why it happens:**
- R6 objects are reference-based; assigning `state$field <- value` doesn't automatically invalidate reactive dependencies
- The `sync_to_rv()` pattern in `create_reactive_state()` requires manual calls that can be forgotten
- R6 class properties using `reactiveValues` can share environments between instances
- `observeEvent` and `observe` blocks can miss state changes if synchronization timing is wrong

**Consequences:**
- UI shows stale data while R6 state has updated
- Tests pass on isolated state but fail in integration
- "Ghost" state where app appears to work but internal state is corrupted
- Difficult-to-reproduce bugs that depend on event ordering

**Prevention:**
1. **Single source of truth**: Either use R6 OR reactiveValues, not both as parallel stores
2. **If using hybrid approach**: Always modify through wrapper functions that update both:
   ```r
   # Current pattern in state.R is risky - direct R6 mutation won't trigger reactivity
   # Better: all mutations go through sync functions
   update_step <- function(new_step) {
     state$step <- new_step
     rv$step <- new_step  # Always sync both
   }
   ```
3. **Use `makeReactiveBinding()`** to make R6 fields reactive
4. **Test state synchronization explicitly**: Write tests that verify R6 and rv stay in sync

**Detection (warning signs):**
- UI doesn't update after calling R6 methods
- Refreshing the browser "fixes" display issues
- `isolate(rv$field)` differs from `state$field` after mutations
- Tests using `testServer()` pass but manual testing shows bugs

**Phase mapping:** Testing phase - add state synchronization tests; Design phase - consider simplifying state architecture

**Sources:**
- [R6 class reactiveValues property and instantiation - Posit Community](https://forum.posit.co/t/r6-class-reactivevalues-property-and-instantiation/31025)
- [Setting values in R6 classes and testing with MockShinySession](https://rtask.thinkr.fr/setting-values-in-r6-classes-and-testing-shinymockshinysession/)
- [Mastering Shiny - Reactive building blocks](https://mastering-shiny.org/reactivity-objects.html)

---

### Pitfall 3: htmlwidget Static Rendering and Shiny Method Access

**What goes wrong:** When integrating the neurosurf surfwidget (threejs-based), the widget renders but can't communicate with Shiny. `Shiny.setInputValue()` calls fail silently.

**Why it happens:**
- Statically-rendered htmlwidgets execute before `initShiny` runs
- The widget's JavaScript `renderValue` function runs before Shiny methods are registered
- htmlDependency injection timing differs based on whether scripts are same-host or cross-origin
- In `renderUI` contexts, static rendering can happen multiple times

**Consequences:**
- Widget displays correctly but clicks/interactions don't update Shiny inputs
- Intermittent failures: sometimes works, sometimes doesn't (timing-dependent)
- No error messages - failures are silent
- User interactions appear to do nothing

**Prevention:**
1. **Use `HTMLWidgets.addPostRenderHandler(callback)`** instead of immediate Shiny calls:
   ```javascript
   HTMLWidgets.addPostRenderHandler(function() {
     // Shiny methods are now available
     Shiny.setInputValue('my_input', value);
   });
   ```
2. **Check for Shiny availability** before using:
   ```javascript
   if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
     Shiny.setInputValue('input', value);
   }
   ```
3. **Avoid static rendering in dynamic contexts**: Use `renderUI` carefully with htmlwidgets
4. **Test widget Shiny integration explicitly**: Don't assume rendering = working

**Detection (warning signs):**
- Widget renders but `input$widget_click` is never populated
- Console shows no errors but interactions have no effect
- Works in RStudio viewer but not in Shiny app (or vice versa)
- Intermittent failures that clear on page refresh

**Phase mapping:** htmlwidget integration phase - must verify Shiny binding works early

**Sources:**
- [Statically rendered htmlwidgets in Shiny don't have access to Shiny methods - GitHub Issue #2635](https://github.com/rstudio/shiny/issues/2635)
- [JavaScript for R - Widgets with Shiny](https://book.javascript-for-r.com/shiny-widgets.html)
- [htmlwidgets NEWS](https://cran.r-project.org/web/packages/htmlwidgets/news/news.html)

---

### Pitfall 4: WebGL Memory Leaks in Reactive threejs Widget

**What goes wrong:** Memory usage grows continuously as users interact with the brain surface viewer. Browser eventually becomes sluggish or crashes.

**Why it happens:**
- threejs creates WebGL resources (geometries, materials, textures) that aren't automatically garbage collected
- Shiny's reactive re-rendering creates new objects without disposing old ones
- `WebGLProgram` instances leak when scenes are recreated
- Canvas elements accumulate when widgets re-render

**Consequences:**
- Browser memory grows unbounded during session
- GPU memory exhaustion causes rendering failures
- App becomes unusable after extended use
- Users report "it worked at first but got slow"

**Prevention:**
1. **Explicit disposal in `renderValue`**: Before creating new objects, dispose old ones:
   ```javascript
   // In widget binding's renderValue:
   if (this.scene) {
     this.scene.traverse(function(obj) {
       if (obj.geometry) obj.geometry.dispose();
       if (obj.material) {
         if (obj.material.map) obj.material.map.dispose();
         obj.material.dispose();
       }
     });
     this.renderer.dispose();
   }
   ```
2. **Reuse materials and geometries** instead of creating new instances
3. **Use `use.orbitcontrols=TRUE`** in rthreejs for more CPU-efficient rendering
4. **Monitor memory** during development: Chrome DevTools Memory tab

**Detection (warning signs):**
- Chrome DevTools shows increasing JS heap over time
- `performance.memory.usedJSHeapSize` grows monotonically
- Switching LVs repeatedly causes slowdown
- GPU process memory in Task Manager keeps growing

**Phase mapping:** htmlwidget integration phase - implement disposal from the start

**Sources:**
- [Memory leak with WebGL - three.js Issue #2851](https://github.com/mrdoob/three.js/issues/2851)
- [WebGL memory management puzzlers - three.js forum](https://discourse.threejs.org/t/webgl-memory-management-puzzlers/24583)
- [rthreejs GitHub](https://github.com/bwlewis/rthreejs)

---

## Moderate Pitfalls

Mistakes that cause delays, test flakiness, or technical debt.

---

### Pitfall 5: testServer Cannot Test UI Update Functions

**What goes wrong:** Tests using `testServer()` fail when module code uses `updateSelectInput()`, `updateRadioButtons()`, or similar UI update functions.

**Why it happens:**
- `testServer()` runs in a mock session environment that doesn't have a real browser
- UI update functions like `updateSelectInput()` send messages to the client that never arrive
- The MockShinySession doesn't simulate UI-side behavior

**Consequences:**
- Tests fail even though the app works correctly in browser
- False confidence in test coverage (untestable code paths)
- Temptation to skip tests for modules with UI updates

**Prevention:**
1. **Separate testable logic** from UI updates:
   ```r
   # Extract the logic that decides what to update
   compute_filter_options <- function(data, selected_group) {
     # Pure function - easily testable
     unique(data[data$group == selected_group, "option"])
   }

   # UI update is thin wrapper
   observe({
     options <- compute_filter_options(data(), input$group)
     updateSelectInput(session, "filter", choices = options)
   })
   ```
2. **Use shinytest2 for UI update tests**: End-to-end tests can verify UI updates work
3. **Document untestable-with-testServer code paths**
4. **Use `exportTestValues()` to verify computed values** even if UI update isn't tested

**Detection (warning signs):**
- `testServer()` tests fail with "session$sendInputMessage" errors
- Tests pass but don't actually cover the UI update logic
- Module coverage appears complete but UI behavior is buggy

**Phase mapping:** Testing phase - design module structure for testability

**Sources:**
- [Mastering Shiny - Chapter 21 Testing](https://mastering-shiny.org/scaling-testing.html)
- [Appsilon - How to Write Tests with testServer](https://www.appsilon.com/post/how-to-write-tests-with-shiny-testserver)

---

### Pitfall 6: shinytest2 Module Namespace Brittleness

**What goes wrong:** Tests break when module IDs change, modules are nested differently, or input IDs are renamed. Test code is littered with fragile namespace strings.

**Why it happens:**
- Shiny modules use namespaced IDs like `"setup-data_source"` or `"explore-brain_viewer-btn_montage"`
- Tests reference these compound IDs directly
- Restructuring modules (nesting, renaming) requires updating all affected tests
- No central registry of test-relevant IDs

**Consequences:**
- Refactoring is painful - every module change breaks multiple tests
- Test code is hard to read with deeply nested namespace strings
- Copy-paste errors with namespace prefixes

**Prevention:**
1. **Use data-test attributes** (see Pitfall 1) - these survive namespace changes:
   ```r
   # In UI
   actionButton(ns("run"), "Run Analysis", `data-test` = "run-analysis-btn")

   # In test - doesn't care about module nesting
   app$click(selector = "[data-test=run-analysis-btn]")
   ```
2. **Create test helper functions** that encapsulate namespace logic:
   ```r
   click_run_analysis <- function(app) {
     app$click(selector = "[data-test=run-analysis-btn]")
   }
   ```
3. **For set_inputs**, use the selector approach where possible

**Detection (warning signs):**
- Test code has strings like `"mod_a-mod_b-mod_c-input_id"`
- Renaming a module requires updating 10+ test files
- Tests break after "cosmetic" refactoring

**Phase mapping:** Testing phase - establish data-test conventions before writing tests

**Sources:**
- [Trouble with namespace in application as package - shinytest2 Issue #215](https://github.com/rstudio/shinytest2/issues/215)
- [Creating Robust E2E Test Selectors](https://jakubsobolewski.com/blog/robust-targetting-of-html-for-tests/)

---

### Pitfall 7: CSS Specificity Wars with Bootstrap/bslib

**What goes wrong:** Custom CSS rules don't apply, or worse, override Bootstrap styles unpredictably. The 4px grid and semantic color system conflicts with Bootstrap defaults.

**Why it happens:**
- Bootstrap CSS has high specificity selectors
- bslib themes inject CSS after custom stylesheets
- Custom `.pls-*` classes may have lower specificity than Bootstrap's chained selectors
- `!important` escalation war begins

**Consequences:**
- Inconsistent styling across components
- Styles work in isolation but break when Bootstrap components added
- Maintenance nightmare with scattered `!important` declarations
- Dark mode or theme switching breaks custom styles

**Prevention:**
1. **Use CSS custom properties (variables)** that bslib respects:
   ```css
   /* This works with bslib theme switching */
   .pls-card {
     background: var(--bs-body-bg, white);
     border-color: var(--bs-border-color, #e5e7eb);
   }
   ```
2. **Namespace all custom classes** (already done with `pls-*` prefix - good)
3. **Use `bs_add_rules()`** to inject CSS through bslib, not separate stylesheet:
   ```r
   theme <- bs_theme() %>%
     bs_add_rules(sass::sass_file("www/styles.scss"))
   ```
4. **Test styles with different Bootstrap versions** (3, 4, 5)
5. **Avoid `!important`** - instead, increase specificity legitimately

**Detection (warning signs):**
- Styles work in dev but break in different Shiny deployment
- Switching bslib themes breaks custom components
- `!important` appearing frequently in styles.css
- Same element styled differently in different browsers

**Phase mapping:** Design refinement phase - audit CSS specificity before adding new styles

**Sources:**
- [bslib - Theming](https://rstudio.github.io/bslib/articles/theming/index.html)
- [Outstanding User Interfaces with Shiny - Chapter 9](https://unleash-shiny.rinterface.com/beautify-with-bootstraplib)
- [ModalDialog css conflicts with bslib theme - Posit Community](https://forum.posit.co/t/modaldialog-css-conflicts-with-bslib-theme/164525)

---

### Pitfall 8: shinytest2 Timing and Wait Issues

**What goes wrong:** Tests are flaky - sometimes pass, sometimes fail. `set_inputs()` returns before the app has processed the input, leading to stale `get_value()` calls.

**Why it happens:**
- By default, `set_inputs()` waits for any output to change, not a specific output
- If the input doesn't cause an output change, the default 3-second timeout fails
- htmlwidgets and complex reactive chains have unpredictable timing
- Different machines have different performance characteristics

**Consequences:**
- Flaky tests that fail randomly on CI
- Tests pass locally but fail on slower CI machines (or vice versa)
- Developers add arbitrary `Sys.sleep()` calls that slow down test suite

**Prevention:**
1. **Use `wait_for_value()` for specific outputs**:
   ```r
   app$set_inputs(`lv-selector` = 2)
   app$wait_for_value(output = "brain_plot")  # Wait for specific output
   ```
2. **Set `wait_ = FALSE` for inputs that don't immediately affect outputs**:
   ```r
   app$set_inputs(toggle = TRUE, wait_ = FALSE)
   ```
3. **Increase timeout for slow operations**:
   ```r
   app$set_inputs(run_analysis = "click", timeout_ = 30000)  # 30 seconds
   ```
4. **Use `wait_for_idle()`** for complex reactive chains:
   ```r
   app$wait_for_idle(timeout = 5000)
   ```
5. **Never use `Sys.sleep()`** - use explicit waits instead

**Detection (warning signs):**
- Tests have `Sys.sleep()` calls
- Same test passes/fails randomly
- Tests timeout with "no output changed" errors
- Tests pass in interactive mode but fail in batch mode

**Phase mapping:** Testing phase - establish wait patterns in test helpers

**Sources:**
- [AppDriver - shinytest2 Reference](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
- [shinytest2 FAQ](https://rstudio.github.io/shinytest2/articles/zzz-faq.html)

---

## Minor Pitfalls

Mistakes that cause annoyance but are fixable.

---

### Pitfall 9: Blindly Accepting Initial Snapshots

**What goes wrong:** First test run captures incorrect baseline values. All subsequent tests pass against wrong expected values.

**Prevention:**
- Always manually inspect initial snapshots before committing
- Use explicit value comparisons when expected values are known:
  ```r
  expect_equal(app$get_value(output = "n_subjects"), 10)  # Not snapshot
  ```

**Phase mapping:** Testing phase - code review should verify initial snapshot correctness

---

### Pitfall 10: Testing Only Happy Paths

**What goes wrong:** Tests verify app works when everything goes right, but miss error handling, edge cases, and validation.

**Prevention:**
- Write tests for invalid inputs, missing data, network failures
- Test that error messages appear correctly
- Test boundary conditions (0 subjects, 1 LV, max values)

**Phase mapping:** Testing phase - require edge case tests in PR checklist

---

### Pitfall 11: MockShinySession Doesn't Mock session$request

**What goes wrong:** Tests for authentication or request-header-dependent code fail because `session$request` is an empty environment.

**Prevention:**
- Override MockShinySession's request field for auth tests:
  ```r
  MockShinySession$set(
    "active", "request",
    function(value) list(HTTP_X_USER = "test@example.com"),
    overwrite = TRUE
  )
  ```
- Restore original after test with `on.exit()`

**Phase mapping:** Testing phase - if app uses session$request, address early

**Sources:**
- [Setting values in R6 classes and testing with MockShinySession](https://rtask.thinkr.fr/setting-values-in-r6-classes-and-testing-shinymockshinysession/)

---

### Pitfall 12: Windows WebGL Rendering in RStudio

**What goes wrong:** neurosurf surfwidget shows blank or doesn't render in RStudio's viewer on Windows.

**Prevention:**
- Document that users need to use "Open in Browser" button on Windows
- Or detect Windows + RStudio and show helpful message
- Test on Windows explicitly before release

**Sources:**
- [rthreejs README](https://github.com/bwlewis/rthreejs)

---

## Phase-Specific Warnings

| Phase | Likely Pitfall | Mitigation |
|-------|---------------|------------|
| Testing (shinytest2) | Fragile tests (#1), namespace brittleness (#6), timing (#8) | Establish data-test attributes and wait helpers first |
| Testing (testServer) | UI update untestability (#5), R6/rv sync (#2) | Design modules for testability, test state sync explicitly |
| Design refinement (CSS) | Bootstrap specificity (#7) | Use CSS variables, avoid !important, test with bslib |
| htmlwidget integration | Shiny method access (#3), WebGL leaks (#4), Windows rendering (#12) | Verify Shiny binding early, implement disposal from start |
| R6 state management | Synchronization (#2) | Test R6-rv sync, consider single source of truth |

---

## Project-Specific Observations

Based on inspection of `/Users/bbuchsbaum/code/plsrri/inst/shiny/`:

1. **state.R has sync risk**: The `create_reactive_state()` function maintains parallel R6 and reactiveValues stores. The `sync_to_rv()` function must be called manually after R6 mutations - easy to forget.

2. **mod_brain_viewer.R uses plotOutput**: Currently uses standard `plotOutput`, not an htmlwidget. If migrating to neurosurf surfwidget, this is a complete rewrite of the rendering logic.

3. **styles.css is well-structured**: Uses CSS custom properties and `pls-*` namespace. Main risk is Bootstrap version compatibility - verify with bslib's default Bootstrap 5.

4. **No test files exist for Shiny modules**: The `tests/testthat/` directory has no `test-mod_*.R` files. Starting from zero means testing architecture decisions are still open.

5. **shinyjs usage in mod_brain_viewer**: Uses `shinyjs::addClass/removeClass` for button toggle state. This may have timing issues with shinytest2 - verify button state assertions use waits.

---

## Quality Gate Verification

- [x] Pitfalls are specific to Shiny/htmlwidget domain
- [x] Prevention strategies are actionable (code examples provided where relevant)
- [x] Phase mapping included for all critical and moderate pitfalls
- [x] Sources cited for claims

---

## Sources Summary

### Official Documentation
- [shinytest2 - Robust Testing](https://rstudio.github.io/shinytest2/articles/robust.html)
- [shinytest2 - AppDriver Reference](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
- [shinytest2 - FAQ](https://rstudio.github.io/shinytest2/articles/zzz-faq.html)
- [Mastering Shiny - Testing](https://mastering-shiny.org/scaling-testing.html)
- [Mastering Shiny - Reactive building blocks](https://mastering-shiny.org/reactivity-objects.html)
- [bslib - Theming](https://rstudio.github.io/bslib/articles/theming/index.html)
- [htmlwidgets NEWS](https://cran.r-project.org/web/packages/htmlwidgets/news/news.html)

### Community & Third-Party
- [Appsilon - How to Write Tests with testServer](https://www.appsilon.com/post/how-to-write-tests-with-shiny-testserver)
- [Jakub Sobolewski - How to Write Robust shinytest2 Tests](https://jakubsobolewski.com/blog/robust-shinytest2/)
- [Jakub Sobolewski - Creating Robust E2E Test Selectors](https://jakubsobolewski.com/blog/robust-targetting-of-html-for-tests/)
- [ThinkR - Setting values in R6 classes and MockShinySession](https://rtask.thinkr.fr/setting-values-in-r6-classes-and-testing-shinymockshinysession/)
- [Outstanding User Interfaces with Shiny - Chapter 9](https://unleash-shiny.rinterface.com/beautify-with-bootstraplib)

### GitHub Issues
- [Shiny #2635 - Statically rendered htmlwidgets](https://github.com/rstudio/shiny/issues/2635)
- [shinytest2 #215 - Namespace in application as package](https://github.com/rstudio/shinytest2/issues/215)
- [three.js #2851 - Memory leak with WebGL](https://github.com/mrdoob/three.js/issues/2851)

### Package Repositories
- [rthreejs GitHub](https://github.com/bwlewis/rthreejs)
- [neurosurf GitHub](https://github.com/bbuchsbaum/neurosurf)
