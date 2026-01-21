# Feature Landscape: Production-Quality Shiny GUI

**Domain:** Scientific neuroimaging analysis tool (PLS)
**Researched:** 2026-01-21
**Context:** Shiny GUI quality milestone for plsrri

## Table Stakes

Features users expect in a production-quality Shiny application. Missing any of these makes the tool feel incomplete or unreliable.

### Testing Infrastructure

| Feature | Why Expected | Complexity | Status | Notes |
|---------|--------------|------------|--------|-------|
| Module-level unit tests (testServer) | Standard for any R package with Shiny components; enables testing reactive logic without browser | Medium | Missing | Test each mod_*.R file; use `shiny::testServer()` for server-side logic |
| End-to-end tests (shinytest2) | Industry standard for Shiny E2E testing; catches integration issues | Medium-High | Missing | Use shinytest2 (not deprecated shinytest); record critical user flows |
| Test fixtures for mock results | Tests need realistic data without running actual analysis | Low | Missing | Create `pls_result` mock objects with realistic structure |
| CI integration | Tests must run automatically on commits | Low | Partial | GitHub Actions exists for R CMD check; add Shiny-specific tests |

**Sources:**
- [Mastering Shiny - Testing Chapter](https://mastering-shiny.org/scaling-testing.html)
- [shinytest2 Official Documentation](https://rstudio.github.io/shinytest2/)
- [How to Write Robust shinytest2 Tests](https://www.r-bloggers.com/2025/08/how-to-write-robust-shinytest2-tests-for-r-shiny-apps/)

### User Feedback Patterns

| Feature | Why Expected | Complexity | Status | Notes |
|---------|--------------|------------|--------|-------|
| Loading indicators during computation | Users need to know app is working, not frozen | Low | Partial | `loading_spinner` exists; ensure it covers all long operations |
| Progress feedback for analysis | Multi-second operations need progress indication | Medium | Partial | Analysis runs in mod_analyze.R; needs progress bar |
| Validation messages on inputs | Users need immediate feedback on invalid inputs | Low | Partial | `validation_message` component exists; ensure consistent use |
| Graceful error handling | Errors should not crash app; show user-friendly messages | Medium | Partial | Add tryCatch around all server operations |
| Empty states | Clear messaging when no data loaded | Low | Partial | Some panels show "--" but need consistent empty state design |

**Sources:**
- [Engineering Production-Grade Shiny Apps - UX Matters](https://engineering-shiny.org/ux-matters.html)
- [Appsilon UX Design Guide](https://www.appsilon.com/post/ux-design-of-shiny-apps-7-steps-to-design-dashboards-people-love)

### Input Handling

| Feature | Why Expected | Complexity | Status | Notes |
|---------|--------------|------------|--------|-------|
| Input validation before processing | Prevent cryptic R errors from reaching users | Medium | Partial | Setup module needs validation; use shinyFeedback or custom |
| Required field indicators | Red asterisks or similar for mandatory fields | Low | Missing | design-principles defines pattern; implement consistently |
| Disabled states for unavailable actions | Buttons/inputs disabled until prerequisites met | Low | Partial | Continue button logic exists; extend to all conditional actions |
| Action buttons for expensive operations | Avoid auto-reactivity on parameter changes | Low | Exists | Run Analysis button in mod_analyze.R |

**Sources:**
- [Engineering Production-Grade Shiny Apps - UX Matters](https://engineering-shiny.org/ux-matters.html)

### Visual Design Consistency

| Feature | Why Expected | Complexity | Status | Notes |
|---------|--------------|------------|--------|-------|
| Consistent spacing (4px grid) | Professional appearance; design-principles spec | Low | Exists | CSS variables defined in theme.R; audit actual usage |
| Semantic color system | Status colors (success/warning/error) used consistently | Low | Exists | pls_colors_semantic defined; audit component usage |
| Status dot indicators | Visual workflow state communication | Low | Exists | status_dot component exists; used in stepper and LV list |
| Card component pattern | Consistent panel styling | Low | Exists | panel_card wrapper exists |
| Typography hierarchy | Clear visual hierarchy with heading levels | Low | Partial | Inter font configured; audit heading usage |

**Sources:**
- [bslib Theming Documentation](https://rstudio.github.io/bslib/articles/theming/index.html)
- [bslib v0.9.0 with brand.yml](https://shiny.posit.co/blog/posts/bslib-0.9.0/)

### Accessibility Basics

| Feature | Why Expected | Complexity | Status | Notes |
|---------|--------------|------------|--------|-------|
| Keyboard navigation | Not all users can use mouse | Medium | Unknown | Test tab order through critical flows |
| Color-blind safe palettes | ~8% of males have color vision deficiency | Low | Partial | Use viridis for data viz; audit status colors |
| Semantic HTML structure | Screen reader compatibility | Low | Unknown | Audit heading hierarchy, form labels |
| Form labels for inputs | ARIA compliance | Low | Partial | shiny::selectInput includes labels; verify all inputs |

**Sources:**
- [Engineering Production-Grade Shiny Apps - Accessibility](https://engineering-shiny.org/ux-matters.html)
- [viridis color package](https://cran.r-project.org/web/packages/viridis/)

### Export Functionality

| Feature | Why Expected | Complexity | Status | Notes |
|---------|--------------|------------|--------|-------|
| Export brain maps (NIfTI) | Scientific reproducibility; standard neuroimaging format | Medium | Exists | Inspector module has export |
| Export data tables (CSV) | Results need to be usable in other tools | Low | Exists | Inspector module has export |
| Export figures (PNG/PDF) | Publication-quality figures | Medium | Partial | Needs explicit figure download buttons |
| Export HTML report | Shareable summary of analysis | High | Exists | render_report.R functionality |

**Sources:**
- [Advancing Nursing Research Through Interactive Data Visualization](https://pmc.ncbi.nlm.nih.gov/articles/PMC9900251/)
- [Reproducible Research Techniques](https://learning.nceas.ucsb.edu/2019-11-RRCourse/introduction-to-shiny.html)

---

## Differentiators

Features that elevate the UX beyond table stakes. Not expected, but valuable for scientific tools.

### Enhanced Visualization

| Feature | Value Proposition | Complexity | Status | Notes |
|---------|-------------------|------------|--------|-------|
| Surface-based brain visualization | Cortical data shown on inflated surfaces; clearer than volume slices | High | Planned | neurosurf integration via surfwidget |
| Synchronized volume/surface views | Same thresholding/LV selection across both views | Medium | Planned | Requires abstraction layer |
| Interactive plot brushing | Click on brain region to see corresponding scores | High | Not planned | Would require coordinated plotly |
| Colorbar customization | User control over colormap and range | Medium | Not started | Enhance existing viewer |

**Sources:**
- [ggseg and ggseg3d Packages](https://journals.sagepub.com/doi/10.1177/2515245920928009)
- [ShinySurfer Tool](https://sandrakla.github.io/ShinySurfer_Homepage/)

### Analysis Workflow

| Feature | Value Proposition | Complexity | Status | Notes |
|---------|-------------------|------------|--------|-------|
| Session state persistence | Resume interrupted analysis | Medium | Not started | Save/restore AppState |
| Undo/redo for parameter changes | Error recovery without full restart | High | Not planned | Complex state management |
| Batch analysis mode | Run multiple analyses with parameter sweep | High | Not planned | Would require job queue |
| Analysis presets | Save and recall common configurations | Medium | Not started | Store parameter sets |

### Result Exploration

| Feature | Value Proposition | Complexity | Status | Notes |
|---------|-------------------|------------|--------|-------|
| Cluster table with peak coordinates | Standard neuroimaging reporting format | Medium | Not started | Extract from brain maps |
| Region labeling (atlas lookup) | Automatic anatomical labels for clusters | Medium | Not started | Requires atlas integration |
| Side-by-side LV comparison | Compare multiple LVs simultaneously | Medium | Not started | Layout changes to explore module |
| Reproducibility export | R code to recreate analysis | Medium | Not started | Generate script from spec |

**Sources:**
- [archeoViz Reproducibility Features](https://cran.r-project.org/web/packages/archeoViz/vignettes/archeoViz.html)
- [A Practical Guide for Neuroimaging Visualizations](https://apertureneuro.org/article/85104-a-practical-guide-for-generating-reproducible-and-programmatic-neuroimaging-visualizations)

### Developer Experience

| Feature | Value Proposition | Complexity | Status | Notes |
|---------|-------------------|------------|--------|-------|
| Behavior-driven test structure | Tests describe user behavior, not implementation | Medium | Not started | Use data-testid pattern |
| Visual regression tests | Catch unintended UI changes | High | Not planned | Platform-dependent rendering issues |
| Component storybook | Isolated component development | Medium | Not planned | Not standard in R ecosystem |

**Sources:**
- [BDD in R Shiny](https://www.r-bloggers.com/2025/11/behavior-driven-development-in-r-shiny-setting-up-test-preconditions-with-given-steps/)

---

## Anti-Features

Features to explicitly NOT build. Common mistakes in scientific/neuroimaging tools.

### Feature Creep

| Anti-Feature | Why Avoid | What to Do Instead |
|--------------|-----------|-------------------|
| Kitchen-sink visualization options | Increases complexity, slows performance, steepens learning curve | Curate sensible defaults; hide advanced options |
| Auto-reactive parameter changes | Every slider change triggers expensive recomputation | Require explicit "Run" button; debounce filters |
| Interactive everything (Plotly everywhere) | Adds visual noise, slows rendering, often unnecessary | Use static ggplot2 where interactivity adds no value |
| Multiple analysis methods in one view | Confuses users about what they're looking at | One method per session; clear method indicator |

**Sources:**
- [Engineering Production-Grade Shiny Apps - Feature Creep](https://engineering-shiny.org/ux-matters.html)

### Scientific Tool Pitfalls

| Anti-Feature | Why Avoid | What to Do Instead |
|--------------|-----------|-------------------|
| Hiding statistical details | Researchers need to see p-values, effect sizes, confidence intervals | Surface all relevant statistics; document methods |
| Non-reproducible defaults | Random seeds, hidden parameters lead to unreproducible results | Document all parameters; provide reproducibility export |
| Proprietary file formats | Locks users into tool; prevents verification | Use standard formats (NIfTI, CSV); document format specs |
| Undocumented thresholding | Users don't know what they're looking at | Show current threshold in UI; explain what it means |

### UX Anti-Patterns

| Anti-Feature | Why Avoid | What to Do Instead |
|--------------|-----------|-------------------|
| Cryptic R error messages | Users see "Error in X: argument Y is missing" | Wrap in tryCatch; show user-friendly message |
| Silent failures | Operations fail without notification | Always provide feedback (success, warning, error) |
| Confusing navigation | Users don't know where they are or how to proceed | Clear stepper; consistent back/next pattern |
| Information overload | Too much data shown at once | Progressive disclosure; inspector panel for details |
| Styling headers for hierarchy | Using h1-h6 for visual size, not semantic structure | Use proper heading hierarchy; style with CSS |

**Sources:**
- [Engineering Production-Grade Shiny Apps - Fail Informatively](https://engineering-shiny.org/ux-matters.html)
- [The Effective Statistician - Shiny Mistakes](https://theeffectivestatistician.com/r-shiny-how-to-it-set-up-effectively-and-avoid-common-mistakes/)

### Testing Anti-Patterns

| Anti-Feature | Why Avoid | What to Do Instead |
|--------------|-----------|-------------------|
| Testing implementation, not behavior | Fragile tests that break on refactoring | Use data-testid attributes; test user-visible outcomes |
| Snapshot-heavy E2E tests | Platform differences cause false failures | Use snapshots sparingly; prefer behavioral assertions |
| Testing UI appearance | Visual tests are flaky across environments | Keep visual tests local; use for development only |
| Missing test fixtures | Tests depend on real analysis (slow, flaky) | Create mock pls_result objects with realistic structure |

**Sources:**
- [How to Write Robust shinytest2 Tests](https://jakubsobolewski.com/blog/robust-shinytest2/)
- [Mastering Shiny - Testing Fragility](https://mastering-shiny.org/scaling-testing.html)

---

## Feature Dependencies

```
Testing Infrastructure
  |
  +-> Test fixtures (needed for all other tests)
       |
       +-> Module tests (testServer)
       |    |
       |    +-> Integration tests (module interactions)
       |
       +-> E2E tests (shinytest2)
            |
            +-> CI integration

User Feedback
  |
  +-> Loading indicators (needed for progress feedback)
       |
       +-> Progress bars (built on loading infrastructure)

Visual Design
  |
  +-> Theme audit (foundation for all design work)
       |
       +-> Component audit
       |    |
       |    +-> Typography audit
       |
       +-> State audit (error/empty/disabled)

Export Functionality
  |
  +-> Figure export (foundation)
       |
       +-> Report export (includes figures)

Surface Visualization (Differentiator)
  |
  +-> Brain viewer abstraction layer
       |
       +-> neurosurf integration
            |
            +-> Synchronized controls
```

---

## MVP Recommendation

For production quality, prioritize in this order:

### Phase 1: Testing Foundation
1. **Test fixtures** - Mock pls_result objects (LOW complexity, HIGH value)
2. **Module tests** - testServer for all mod_*.R files (MEDIUM complexity, HIGH value)
3. **Critical E2E tests** - Setup->Analyze->Explore happy path (MEDIUM complexity, HIGH value)

### Phase 2: User Feedback Polish
4. **Validation consistency** - Audit all inputs for validation (LOW complexity, MEDIUM value)
5. **Error handling audit** - tryCatch all server operations (MEDIUM complexity, HIGH value)
6. **Loading states** - Ensure all long operations show feedback (LOW complexity, MEDIUM value)

### Phase 3: Design Consistency
7. **CSS/theme audit** - Verify design-principles compliance (LOW complexity, MEDIUM value)
8. **State audit** - Error, empty, disabled states (LOW complexity, MEDIUM value)
9. **Accessibility basics** - Keyboard nav, color contrast (MEDIUM complexity, MEDIUM value)

### Defer to Post-MVP
- Surface visualization (HIGH complexity, requires abstraction layer first)
- Session persistence (MEDIUM complexity, not critical for correctness)
- Cluster tables/region labeling (MEDIUM complexity, enhancement not core)
- Visual regression tests (HIGH complexity, platform issues)

---

## Testing Coverage Expectations

Based on research, production-quality Shiny testing should include:

### Minimum Viable Testing

| Test Type | Coverage Target | Rationale |
|-----------|-----------------|-----------|
| Module server tests (testServer) | All modules with reactive logic | Fast, tests logic without browser overhead |
| Critical path E2E (shinytest2) | Happy path for each workflow step | Catches integration issues |
| Error condition tests | All user-triggerable error paths | Prevents cryptic R errors reaching users |

### Recommended Testing

| Test Type | Coverage Target | Rationale |
|-----------|-----------------|-----------|
| Input validation tests | All form inputs | Ensures validation messages appear correctly |
| State transition tests | All workflow transitions | Verifies stepper logic works |
| Export function tests | All export formats | Critical functionality for scientists |
| Accessibility tests | Keyboard navigation through critical flows | Required for broad usability |

### Test Structure per Module

```r
# tests/testthat/test-mod_{name}.R

# Server logic tests (fast, comprehensive)
test_that("mod_{name} server initializes correctly", {
  testServer(mod_{name}_server, args = list(rv = mock_rv), {
    # Test initial state
  })
})

test_that("mod_{name} handles valid input", {
  testServer(mod_{name}_server, args = list(rv = mock_rv), {
    # Simulate user action
    # Assert reactive updates
  })
})

test_that("mod_{name} handles invalid input gracefully", {
  testServer(mod_{name}_server, args = list(rv = mock_rv), {
    # Test error conditions
  })
})
```

**Sources:**
- [Mastering Shiny - Four-Level Testing Hierarchy](https://mastering-shiny.org/scaling-testing.html)
- [testServer vs shinytest2](https://www.appsilon.com/post/how-to-write-tests-with-shiny-testserver)
- [shinytest2 Getting Started](https://rstudio.github.io/shinytest2/articles/shinytest2.html)

---

## Confidence Assessment

| Category | Confidence | Rationale |
|----------|------------|-----------|
| Testing patterns | HIGH | Official documentation (Mastering Shiny, shinytest2), verified practices |
| UX patterns | HIGH | Engineering Production-Grade Shiny Apps, Appsilon guides |
| Anti-features | MEDIUM | Community experience, some from blog posts |
| Surface viz features | MEDIUM | Domain-specific, based on ggseg/ShinySurfer examples |
| Accessibility requirements | MEDIUM | General web standards; Shiny-specific guidance sparse |

---

*Researched: 2026-01-21*
*Sources: Official Shiny documentation, Engineering Production-Grade Shiny Apps, shinytest2, Mastering Shiny, Appsilon UX guides, neuroimaging visualization papers*
