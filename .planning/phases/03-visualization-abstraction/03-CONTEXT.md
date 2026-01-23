# Phase 3: Visualization Abstraction - Context

**Gathered:** 2026-01-22
**Status:** Ready for planning

<domain>
## Phase Boundary

Brain visualization uses pluggable backend via R6 strategy pattern. This phase creates BrainRenderer abstract class, Neuroim2Renderer implementation wrapping existing plot_brain(), RendererRegistry for renderer selection, and MockBrainRenderer for testing. mod_brain_viewer is refactored to use renderers through the registry.

</domain>

<decisions>
## Implementation Decisions

### Renderer interface scope
- Render-only interface — no lifecycle methods (initialize, dispose)
- Caller manages lifecycle; renderer just renders
- Duck typing for capabilities — no explicit capability flags
- Missing methods error naturally; no need for supports_X checks

### Registry design
- User choice for renderer selection — UI toggle between available renderers
- Only show renderers that are available (hidden, not disabled)
- Neuroim2Renderer is the default when surfwidget isn't available
- If only one renderer available, still show the selector (consistent UI)

### Migration strategy
- Preserve existing plot_brain() functionality — Neuroim2Renderer wraps it internally
- Full refactor of mod_brain_viewer in this phase — uses renderer via registry by end
- Existing tests must continue to pass throughout migration

### Claude's Discretion
- Return type of render methods (UI element vs render function) — based on existing mod_brain_viewer patterns
- Data input format (full pls_result vs specific components) — based on testing and flexibility needs
- Registry scope (singleton vs per-session) — based on Shiny session isolation requirements
- Renderer choice UI location (filter bar vs settings) — based on existing UI structure
- State preservation when switching renderers — based on technical feasibility

</decisions>

<specifics>
## Specific Ideas

No specific requirements — open to standard R6 patterns for abstract classes and strategy pattern.

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 03-visualization-abstraction*
*Context gathered: 2026-01-22*
