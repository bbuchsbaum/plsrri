# Project Research Summary

**Project:** plsrri Shiny GUI Quality Infrastructure
**Domain:** Scientific neuroimaging analysis tool with Shiny GUI
**Researched:** 2026-01-21
**Confidence:** HIGH

## Executive Summary

The plsrri Shiny GUI quality milestone centers on establishing production-grade testing infrastructure for a scientific neuroimaging application. The 2025/2026 Shiny testing ecosystem has matured around a three-tier approach: testthat for unit testing business logic, testServer() for reactive server-side testing, and shinytest2 for end-to-end browser testing. This stack provides comprehensive coverage while balancing test speed with behavioral fidelity. The existing app has a solid modular foundation with R6 state management and bslib theming that supports this testing approach.

The recommended approach is to prioritize testing infrastructure before UI polish or new features. The app's existing architecture (6 discrete modules, centralized AppState R6 class, separation of business logic) is well-suited for testServer() testing. However, two critical refactoring needs emerge: (1) extracting business logic from module servers to enable pure function unit testing, and (2) creating a visualization backend abstraction using R6 strategy pattern to support both current neuroim2 rendering and future surfwidget integration.

The primary risks are fragile test suites (tests coupled to implementation details rather than behavior), R6/reactiveValues synchronization failures, and htmlwidget integration challenges with WebGL memory leaks and Shiny method access timing. All three risks have well-documented prevention strategies: use data-test attributes instead of namespaced IDs, establish single source of truth for state management, and implement WebGL disposal from the start with surfwidget.

## Key Findings

### Recommended Stack

The testing stack builds on testthat 3.3.2 (already configured in the project) with shinytest2 0.5.0 for E2E testing and shinytesters 0.1.0 to bridge gaps in testServer() for update functions.

**Core technologies:**
- **testthat 3.3.2**: Unit testing foundation with snapshot testing and R6 mocking via `local_mocked_r6_class()` — industry standard, already in use
- **shiny::testServer()**: Built into shiny 1.12.1, fast module testing without browser overhead — perfect for the 6 existing modules
- **shinytest2 0.5.0**: End-to-end browser testing via headless Chromium — required for surfwidget htmlwidget verification
- **shinytesters 0.1.0**: Mock update functions in testServer() — eliminates need for manual session$setInputs() in chained reactivity
- **covr 3.6.5**: Code coverage reporting — standard for R packages

### Expected Features

**Must have (table stakes):**
- Module-level unit tests (testServer) for all mod_*.R files
- End-to-end tests (shinytest2) for critical user flows
- Test fixtures for mock pls_result objects
- CI integration for automated test execution
- Consistent error handling with user-friendly messages
- Loading indicators for all long-running operations
- Input validation with clear feedback

**Should have (competitive):**
- Surface-based brain visualization via neurosurf surfwidget
- Synchronized volume/surface view controls
- Session state persistence for interrupted analysis
- Colorbar customization for brain maps

**Defer (v2+):**
- Interactive plot brushing (click region to see scores)
- Batch analysis mode with parameter sweep
- Cluster table with peak coordinates and atlas lookup
- Visual regression tests (platform-dependent, brittle)

### Architecture Approach

The architecture requires targeted refactoring in three areas: extracting business logic from modules to enable unit testing, creating a visualization backend abstraction for swappable renderers, and restructuring state management to support testServer() patterns. The strategy pattern with R6 abstract BrainRenderer class enables pluggable neuroim2 and surfwidget backends.

**Major components:**
1. **UI Components (mod_*_ui.R)** — Pure HTML/widget generation, minimal logic
2. **Module Servers (mod_*_server.R)** — Thin reactive coordination, delegate to fct_*
3. **Business Logic (fct_*, utils_*)** — Non-reactive computation, easily unit tested
4. **AppState (R6)** — Centralized state container with renderer registry
5. **Visualization Layer (BrainRenderer R6)** — Abstract interface with neuroim2/surfwidget implementations
6. **Renderer Registry** — Factory pattern for visualization backend selection

### Critical Pitfalls

1. **Fragile shinytest2 tests coupled to implementation** — Use data-test attributes instead of namespaced IDs; prefer expect_values() over screenshots; test behavior not wiring
2. **R6 State + reactiveValues synchronization failures** — Establish single source of truth; if hybrid approach, always mutate through sync functions; test state synchronization explicitly
3. **htmlwidget static rendering and Shiny method access** — Use `HTMLWidgets.addPostRenderHandler()` instead of immediate Shiny calls; check for Shiny availability before using
4. **WebGL memory leaks in reactive threejs widget** — Explicit disposal in renderValue before creating new objects; reuse materials and geometries; monitor memory during development
5. **testServer cannot test UI update functions** — Separate testable logic from UI updates; use shinytest2 for UI update verification; use `exportTestValues()` for computed values

## Implications for Roadmap

Based on research, suggested phase structure:

### Phase 1: Testing Foundation
**Rationale:** Testing infrastructure must come first to catch regressions in all subsequent work. Research confirms testServer() is the right tool for module testing.
**Delivers:** Test fixtures, module tests for all 6 modules, CI integration
**Addresses:** Module-level unit tests, test fixtures, CI integration from table stakes
**Avoids:** Fragile tests (#1) by establishing data-test conventions from the start

### Phase 2: Business Logic Extraction
**Rationale:** Extracting business logic from modules into fct_* files is prerequisite for comprehensive unit testing and visualization abstraction.
**Delivers:** Pure function unit tests, prepare_* helper functions, testable separation
**Uses:** testthat 3.3.2 for unit tests
**Implements:** fct_brain_viewer.R, fct_data_validation.R patterns from architecture research

### Phase 3: Visualization Abstraction Layer
**Rationale:** Must abstract visualization before adding surfwidget backend. Research shows R6 strategy pattern is proven approach.
**Delivers:** BrainRenderer abstract class, Neuroim2Renderer, RendererRegistry, MockBrainRenderer for testing
**Uses:** R6 for class hierarchy
**Avoids:** Hardcoded visualization (#2 from architecture anti-patterns)

### Phase 4: surfwidget Integration
**Rationale:** With abstraction in place, surfwidget becomes a new renderer implementation. Research identified specific htmlwidget pitfalls to address.
**Delivers:** SurfwidgetRenderer, volumetric-to-surface mapping, htmlwidget output bindings
**Uses:** neurosurf::surfwidget, shinytest2 for widget testing
**Avoids:** Shiny method access timing (#3), WebGL memory leaks (#4) by implementing prevention from the start

### Phase 5: E2E Test Suite
**Rationale:** End-to-end tests require working surfwidget integration and stable module tests as foundation.
**Delivers:** Critical path E2E tests (Setup->Analyze->Explore), surfwidget rendering verification
**Uses:** shinytest2 0.5.0, SurfwidgetDriver helper class
**Avoids:** Timing issues (#8) by using wait_for_value() patterns from research

### Phase 6: UX Polish
**Rationale:** After testing infrastructure is solid, polish UX without fear of regressions.
**Delivers:** Validation audit, error handling audit, loading state improvements, CSS/theme verification
**Addresses:** User feedback patterns, visual design consistency from table stakes

### Phase Ordering Rationale

- **Testing first:** Enables safe refactoring and regression detection for all subsequent work
- **Business logic extraction before visualization abstraction:** The abstraction requires pure functions to test against
- **Abstraction before surfwidget:** Trying to add surfwidget without abstraction leads to parallel hardcoded implementations
- **E2E after surfwidget:** Cannot test widget integration until widget exists
- **UX polish last:** With test coverage, UX changes can be verified automatically

### Research Flags

Phases likely needing deeper research during planning:
- **Phase 4 (surfwidget):** Complex integration with neurosurf, volumetric-to-surface mapping specifics, WebGL disposal implementation details
- **Phase 3 (Visualization Abstraction):** R6 abstract class patterns in R, renderer capability negotiation

Phases with standard patterns (skip research-phase):
- **Phase 1 (Testing Foundation):** Well-documented testServer() and shinytest2 patterns
- **Phase 2 (Business Logic Extraction):** Standard refactoring, Engineering Shiny best practices
- **Phase 5 (E2E Tests):** Standard shinytest2 usage, patterns documented
- **Phase 6 (UX Polish):** Audit and incremental improvement, no research needed

## Confidence Assessment

| Area | Confidence | Notes |
|------|------------|-------|
| Stack | HIGH | Official documentation for all tools, CRAN version verification, tools already partially in use |
| Features | HIGH | Based on official Shiny testing guides, Engineering Production-Grade Shiny Apps, Appsilon guides |
| Architecture | HIGH | Patterns from Mastering Shiny, R6 design patterns, verified with existing plsrri structure |
| Pitfalls | MEDIUM-HIGH | Mix of official docs and community sources; GitHub issues verified; project-specific observations from code inspection |

**Overall confidence:** HIGH

### Gaps to Address

- **Volumetric-to-surface mapping specifics:** Research identified need for this but implementation details require neurosurf API exploration during Phase 4
- **shinytesters edge cases:** Package is relatively new (0.1.0, September 2025); may encounter undocumented limitations during Phase 1
- **Windows WebGL rendering:** Known issue with RStudio viewer on Windows; need to verify surfwidget behavior and document workaround during Phase 4
- **R6/reactiveValues synchronization in current state.R:** Current implementation has sync risk; may need refactoring during Phase 2

## Sources

### Primary (HIGH confidence)
- [shinytest2 Official Documentation](https://rstudio.github.io/shinytest2/) — testing patterns, AppDriver reference, robust testing guide
- [testServer() Reference](https://shiny.posit.co/r/reference/shiny/latest/testserver.html) — module testing
- [Mastering Shiny - Testing Chapter](https://mastering-shiny.org/scaling-testing.html) — testing pyramid, module testing patterns
- [testthat 3.3.2](https://cran.r-project.org/package=testthat) — R6 mocking, snapshot testing
- [bslib Theming](https://rstudio.github.io/bslib/articles/theming/index.html) — CSS patterns, Bootstrap integration

### Secondary (MEDIUM confidence)
- [Engineering Production-Grade Shiny Apps](https://engineering-shiny.org/) — UX patterns, project structure, feature prioritization
- [Appsilon Testing Guides](https://www.appsilon.com/post/how-to-write-tests-with-shiny-testserver) — testServer() patterns, best practices
- [Jakub Sobolewski - Robust shinytest2 Tests](https://jakubsobolewski.com/blog/robust-shinytest2/) — data-test attributes, selector patterns
- [shinytesters 0.1.0](https://cran.r-project.org/package=shinytesters) — update function mocking

### Tertiary (LOW confidence)
- [threeBrain renderBrain Documentation](https://rdrr.io/cran/threeBrain/man/renderBrain.html) — alternative visualization backend, needs validation if pursued
- [three.js Memory Leak Issues](https://github.com/mrdoob/three.js/issues/2851) — WebGL disposal patterns, needs verification with surfwidget specifically

---
*Research completed: 2026-01-21*
*Ready for roadmap: yes*
