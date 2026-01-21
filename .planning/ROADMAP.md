# Roadmap: plsrri Shiny GUI Hardening

## Overview

This milestone transforms the existing plsrri Shiny GUI from working prototype to production-ready application through comprehensive testing, architecture refinement, and surface visualization integration. The journey follows a test-first approach: establish testing infrastructure, extract and abstract for testability, integrate new visualization capability, verify with E2E tests, then polish UX with confidence. Each phase builds on the previous, with testing enabling safe refactoring throughout.

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3, 4, 5, 6): Planned milestone work
- Decimal phases (e.g., 2.1): Urgent insertions (marked with INSERTED)

- [ ] **Phase 1: Testing Foundation** - Module tests, fixtures, data-test conventions
- [ ] **Phase 2: Business Logic Extraction** - Extract fct_* files for unit testability
- [ ] **Phase 3: Visualization Abstraction** - BrainRenderer R6 interface with registry
- [ ] **Phase 4: surfwidget Integration** - neurosurf surface visualization
- [ ] **Phase 5: E2E Test Suite** - shinytest2 critical path tests
- [ ] **Phase 6: UX Polish** - Validation, error handling, design audit

## Phase Details

### Phase 1: Testing Foundation
**Goal**: Developers can run module tests and use realistic fixtures for all 6 Shiny modules
**Depends on**: Nothing (first phase)
**Requirements**: TEST-01, TEST-02
**Research**: Skip (standard patterns)
**Success Criteria** (what must be TRUE):
  1. Running `devtools::test()` executes testServer() tests for all 6 modules
  2. Mock pls_result fixtures exist with/without bootstrap and with/without permutation variants
  3. Tests use data-test attributes for element selection (not namespaced IDs)
  4. All module tests pass on clean checkout
**Plans**: TBD

Plans:
- [ ] 01-01: TBD

### Phase 2: Business Logic Extraction
**Goal**: Business logic is testable via pure functions separate from reactive module code
**Depends on**: Phase 1 (tests catch extraction regressions)
**Requirements**: ARCH-01
**Research**: Skip (standard refactoring)
**Success Criteria** (what must be TRUE):
  1. fct_brain_viewer.R exists with pure functions extracted from mod_brain_viewer
  2. fct_data_validation.R exists with validation logic extracted from mod_setup
  3. Unit tests exist for extracted functions (not requiring Shiny context)
  4. Module servers delegate to fct_* functions rather than containing business logic
**Plans**: TBD

Plans:
- [ ] 02-01: TBD

### Phase 3: Visualization Abstraction
**Goal**: Brain visualization uses pluggable backend via R6 strategy pattern
**Depends on**: Phase 2 (extracted functions inform interface design)
**Requirements**: ARCH-02, ARCH-03
**Research**: Needed (R6 abstract class patterns, renderer capability negotiation)
**Success Criteria** (what must be TRUE):
  1. BrainRenderer R6 abstract class defines render interface (render_volume, update_threshold, etc.)
  2. Neuroim2Renderer implements BrainRenderer using existing plot_brain() approach
  3. RendererRegistry factory returns appropriate renderer based on capabilities
  4. MockBrainRenderer exists for testing without visualization dependencies
  5. mod_brain_viewer uses renderer through registry (not direct plot_brain calls)
**Plans**: TBD

Plans:
- [ ] 03-01: TBD

### Phase 4: surfwidget Integration
**Goal**: Users can view PLS results on cortical surfaces via neurosurf surfwidget
**Depends on**: Phase 3 (abstraction enables new renderer implementation)
**Requirements**: SURF-01, SURF-02, SURF-03, SURF-04
**Research**: Needed (neurosurf API, vol_to_surf mapping, WebGL disposal)
**Success Criteria** (what must be TRUE):
  1. SurfwidgetRenderer implements BrainRenderer interface using neurosurf::surfwidget
  2. vol_to_surf() correctly maps BSR and salience volumes to surface meshes
  3. Surface tab appears in Explore view showing cortical surface with mapped results
  4. Filter bar controls (LV, threshold) update both volume and surface views simultaneously
  5. WebGL resources are properly disposed when switching views (no memory leaks)
**Plans**: TBD

Plans:
- [ ] 04-01: TBD

### Phase 5: E2E Test Suite
**Goal**: Critical user flows are verified via automated browser tests
**Depends on**: Phase 4 (surfwidget must exist to test)
**Requirements**: TEST-03
**Research**: Skip (standard shinytest2 patterns)
**Success Criteria** (what must be TRUE):
  1. shinytest2 tests exist for Setup -> Analyze -> Explore critical path
  2. Tests verify surfwidget renders correctly in Explore view
  3. Tests use wait_for_value() patterns to handle async operations
  4. All E2E tests pass on clean checkout
**Plans**: TBD

Plans:
- [ ] 05-01: TBD

### Phase 6: UX Polish
**Goal**: GUI provides consistent, validated, user-friendly experience per design-principles
**Depends on**: Phase 5 (test coverage enables safe UX changes)
**Requirements**: UX-01, UX-02, UX-03, UX-04
**Research**: Skip (audit and incremental improvement)
**Success Criteria** (what must be TRUE):
  1. Setup inputs show inline validation with status indicators (green check/red X)
  2. Analysis and export operations display loading indicators with progress updates
  3. CSS and components follow design-principles (4px grid, semantic colors, status dots)
  4. Errors display user-friendly messages with suggested recovery actions
  5. Filter bar, stepper, cards, and inspector match design-principles specification
**Plans**: TBD

Plans:
- [ ] 06-01: TBD

## Progress

**Execution Order:**
Phases execute in numeric order: 1 -> 2 -> 3 -> 4 -> 5 -> 6

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Testing Foundation | 0/TBD | Not started | - |
| 2. Business Logic Extraction | 0/TBD | Not started | - |
| 3. Visualization Abstraction | 0/TBD | Not started | - |
| 4. surfwidget Integration | 0/TBD | Not started | - |
| 5. E2E Test Suite | 0/TBD | Not started | - |
| 6. UX Polish | 0/TBD | Not started | - |

---
*Roadmap created: 2026-01-21*
*Depth: standard (6 phases)*
*Requirements coverage: 14/14 mapped*
