# Phase 2: Business Logic Extraction - Context

**Gathered:** 2026-01-21
**Status:** Ready for planning

<domain>
## Phase Boundary

Extract business logic from Shiny modules into pure `fct_*` functions for unit testability. Creates fct_brain_viewer.R (from mod_brain_viewer), fct_data_validation.R (from mod_setup), with corresponding unit tests. Module servers delegate to these functions rather than containing business logic.

</domain>

<decisions>
## Implementation Decisions

### Claude's Discretion

All implementation decisions delegated to Claude. Apply these principles:

**Extraction scope:**
- Extract logic that doesn't require reactive context: data transformations, validation rules, computations, formatting
- Leave reactive wiring in modules: observers, reactive values, UI updates
- Threshold: If a block of code could be tested without `testServer()`, it belongs in fct_*

**Function organization:**
- One fct_* file per module that has extractable logic
- Functions named descriptively by operation: `validate_data_file()`, `compute_threshold_mask()`, `format_brain_coordinates()`
- Group related functions together; split if file exceeds ~300 lines
- Follow existing R/ file patterns in the package

**Module delegation:**
- Modules call fct_* functions directly with needed parameters
- Keep reactive layer thin — reactive expressions handle timing, fct_* functions handle computation
- Parameters should be plain R objects, not reactive values (unwrap in module)

**Testing approach:**
- Unit tests for each fct_* function
- Test normal cases, edge cases, validation failures
- Tests run without Shiny context (plain testthat)
- Coverage: all public functions, focus on validation logic paths

</decisions>

<specifics>
## Specific Ideas

No specific requirements — open to standard approaches.

Apply R package conventions: functions exported if needed by tests, internal helpers prefixed with `.` or unexported.

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope.

</deferred>

---

*Phase: 02-business-logic-extraction*
*Context gathered: 2026-01-21*
