# Pipeline Attach API

Public API for attaching to a completed CLI pipeline output directory.
These functions form the stable contract between the Shiny UI and the
pipeline internals — the UI should call these, not low-level helpers.
